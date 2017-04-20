library(dplyr)
library(ggplot2)
library(tibble)
library(TTR)
library(RSQLite)
library(lubridate)
library(RcppRoll)
library(quantmod)


#pobranie danych z bazy
db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
dane <- dbGetQuery(db, "select * from notowania")

#funkcja zwracajaca podsumowanie dotyczace akcji (ile notowan, data pierwszego i ostatniego notowania)
podsumowanie <- function(nazwy_akcji){
  query <- paste("select nazwa, count(*) l_dni, min(data) min, max(data) max from notowania
                 where nazwa in('",paste(nazwy_akcji,collapse="' , '"),"')
                 group by nazwa",sep="")
  pods <- dbGetQuery(db, query)
  return(pods)
}

#wektor unikalnych nazw akcji w bazie
akcje <- unique(dane$nazwa)

#podsumowanie dotyczace akcji
pods <- podsumowanie(akcje) %>% as_tibble()

#ostatni dzien notowan
maks_data <- max(pods$max)

#subset spolek debiutujacych przed 2016 rokiem i ktore nadal sa notowane
pods %>% 
  filter(max == maks_data, min < '2016-01-01') %>%
  arrange(desc(l_dni)) -> wybrane_akcje

saveRDS(wybrane_akcje, "wybrane_akcje.rds")
rm(pods)

dane %>% 
  select(-one_of('ISIN', 'waluta', 'zmiana_kursu', 'transakcje', 'wartosc_obrotu')) %>% 
  filter(nazwa %in% wybrane_akcje$nazwa) %>%
  filter(otwarcie != 0 | maksimum != 0 | minimum != 0) %>% 
  group_by(nazwa) %>% 
  #wyliczenie momentum
  do(mutate(., momentum = roll_meanr(ifelse(zamkniecie >= lag(zamkniecie), 1, -1), 5))) %>% 
  #wyliczenie volatility
  do(mutate(., volatility = roll_meanr(c(Delt(zamkniecie, k=1)), 5))) %>% 
  do(mutate(., return_5 = c(Delt(zamkniecie, k=5)))) %>% 
  do(mutate(., ret_5 = lead(return_5, 5))) %>% 
  View
  
  # `[`(complete.cases(.),)
  


# do policzenia danego wskaznika w pipie dla kazdej akcji
#do(mutate(., CCI = CCI(`[`(., c('maksimum', 'minimum', 'zamkniecie'))))) -> dane

dbDisconnect(db)
