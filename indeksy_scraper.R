rm(list=ls())
library(rvest)
library(pbapply)
library(stringi)
library(sqldf)

#wygenerowanie sekwencji dat od pierwszego notowania
dni <- seq(as.Date("1991-04-16"),as.Date("2017-04-16"),by="days")
dni <- dni[-which(weekdays(dni) %in% c("sobota","niedziela"))]

#funkcja pobierajaca notowania indeksow
pobierz_akcje<-function(date){
  
  web<-read_html(paste("https://www.gpw.pl/notowania_archiwalne_full?type=1&date=",date, sep=""))
  
  web %>%
    html_nodes("td , .left, th") %>% 
    html_text()  %>% 
    as.vector() %>%
    matrix(., nrow=11, ncol=length(.)/11) %>%
    t() %>%
    data.frame(.,stringsAsFactors=F) -> df
  
  colnames(df)<-df[1,]
  df<-df[-1,]
  return(df)
}

#pobieranie notowan indeksow na kolejne dni, wraz z obsluga bledow kiedy wystepuje timeout
# akcje<-pblapply(dni,pobierz_akcje)
akcje<-vector("list",length(dni))
for(i in 1:length(dni)){
  tryCatch({
    akcje[[i]]<-pobierz_akcje(dni[i])
    print(i)
    flush.console()
  },
  error=function(err){
    cat("timeout: \n")
    print(err)
    akcje[[i]]<-pobierz_akcje(dni[i])
    flush.console()
  }
  )
}

#sprawdzenie brakow i proba dogrania na te dni
braki<-c()
for(i in 1:length(dni)){
  if(nrow(akcje[[i]])==0 | length(akcje[[i]])==0){
    braki[i] <- 1
  } else {
    braki[i] <- 0
  }
}
braki<-which(braki==1)

for(i in braki){
  print(i)
  akcje[[i]] <- pobierz_akcje(dni[i])
}

#zmiana listy na ramkę danych i dorzucenie daty
l_akcji <- sapply(akcje,nrow)
names(l_akcji) <- dni
daty <- unlist(sapply(1:length(dni), function(x) rep(names(l_akcji)[x],l_akcji[x])))
notowania <- do.call(rbind,akcje)
notowania <- data.frame(daty,notowania)
colnames(notowania) <- c("data", "nazwa", "ISIN", "waluta", "otwarcie", "maksimum", "minimum", "zamkniecie", 
                         "zmiana_kursu", "wolumen", "transakcje", "wartosc_obrotu")
notowania$data<-as.character(notowania$data)


#konwersja zmiennych znakowych do numerycznych
konwertuj_do_numerycznej<-function(kolumna){
  notowania[,kolumna] %>%
    gsub(",",".",., fixed=T) %>%
    stri_replace_all_charclass(., "\\p{WHITE_SPACE}","") %>%
    as.numeric() -> notowania[,kolumna]
}

notowania[,5:12]<-sapply(colnames(notowania)[5:12],konwertuj_do_numerycznej)


#zapis do bazy danych SQL
db <- dbConnect(SQLite(), dbname="notowania_gpw.sqlite")
dbSendQuery(db, "create table indeksy
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db, "indeksy", notowania, overwrite=T)
query <- dbGetQuery(db,"select * from indeksy")
dbDisconnect(db)


