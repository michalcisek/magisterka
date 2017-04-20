library(RSQLite)
library(rvest)

#pobranie danych z bazy
db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
spolki <- dbGetQuery(db, "select distinct nazwa, ISIN from notowania")
dbDisconnect(db)

#wybieramy tylko te spolki ktore spelnialy wymagania w skrypcie filtrowanie.R
wybrane_akcje <- readRDS("wybrane_akcje.rds")
spolki <- spolki[which(spolki$nazwa %in% wybrane_akcje$nazwa), ]

#slownik indeksow sektorowych
sektory <- c('WIG-BANKI', 'WIG-BUDOW', 'WIG-CHEMIA', 'WIG-ENERG', 'WIG-GORNIC', 
             'WIG-INFO', 'WIG-LEKI', 'WIG-MEDIA', 'WIG-MOTO', 'WIG-NRCHOM', 
             'WIG-ODZIEZ', 'WIG-PALIWA', 'WIG-SPOZYW', 'WIG-TELEKOM')

#funkcja scrapujaca do jakich indeksow przynalezy dana spolka
scrapuj_przynaleznosc <- function(isin){
  
  web <- paste("http://www.money.pl/gielda/spolki-gpw/", isin, ",indeksy.html", sep="")
  web <- try(read_html(web), silent=T)
  
  if('try-error' %in% class(web)){
    return(NA)
  } else{
    
    web %>% 
      html_nodes('.ns') %>% 
      html_text() %>% 
      paste(., collapse=" ") -> web
    
    return(web)
  }
}  

#pobranie przynaleznosci do indeksow sektorowych
przynaleznosc <- sapply(spolki$ISIN, scrapuj_przynaleznosc)
#zapis przynaleznosci do pliku RDS
df <- data.frame(spolki, przynaleznosc)
saveRDS(df, "przynaleznosc_sektor.rds")
rm(df)
#zaladowanie pliku RDS
przynaleznosc <- readRDS("przynaleznosc_sektor.rds")


#funkcja wyciagajaca z przynaleznosci tylko udzial w indeksie sektorowym danej spolki
dopasuj_sektor <- function(tekst){
  
  sektor <- sapply(sektory, function(x) grepl(x, tekst))
  sektor <- sektory[which(sektor == T)]
  if (length(sektor) > 1){
    return(paste(sektor, collapse = " --- "))
  } else if (length(sektor) == 0) {
    return(NA)
  } else {
    return(sektor)
  }
}

#przypisanie spolce indeksu sektorowego do ktorego nalezy
spolki$sektor <- sapply(przynaleznosc$przynaleznosc, dopasuj_sektor) 
spolki$sektor <- unlist(spolki$sektor)




db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
indeksy <- dbGetQuery(db, "select * from indeksy")
dbDisconnect(db)


library(dplyr)

indeksy %>% 
  select(-one_of('ISIN', 'waluta', 'zmiana_kursu', 'wolumen', 'transakcje', 'wartosc_obrotu')) %>% 
  filter(nazwa %in% sektory) %>% 
  group_by(nazwa) %>% 
  #wyliczenie momentum
  do(mutate(., momentum = roll_meanr(ifelse(zamkniecie >= lag(zamkniecie), 1, -1), 5))) %>% 
  #wyliczenie volatility
  do(mutate(., volatility = roll_meanr(c(Delt(zamkniecie, k=1)), 5))) %>% 
  do(mutate(., return_5 = c(Delt(zamkniecie, k=5)))) %>% 
  do(mutate(., ret_5 = lead(return_5, 5))) %>% 
  View  
