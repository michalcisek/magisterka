library(RSQLite)
library(rvest)

#pobranie danych z bazy
db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
spolki <- dbGetQuery(db, "select distinct nazwa, ISIN from notowania")
dbDisconnect(db)

#wybieramy tylko te spolki ktore spelnialy wymagania w skrypcie filtrowanie.R
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
saveRDS()

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
spolki$sektor <- sapply(przynaleznosc, dopasuj_sektor) 
spolki$sektor <- unlist(spolki$sektor)
