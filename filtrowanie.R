library(dplyr)
library(ggplot2)
library(tibble)
library(TTR)

db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")

#pobranie danych z bazy
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
  arrange(desc(l_dni)) -> pods

dane %>% 
  filter(nazwa %in% pods$nazwa) %>%
  filter(otwarcie != 0 | maksimum != 0 | minimum != 0) %>% 
  group_by(nazwa) %>% 
  do(mutate(., CCI = CCI(`[`(., c('maksimum', 'minimum', 'zamkniecie'))))) -> dane


kghm <- filter(dane, nazwa == "KGHM")

ggplot(kghm, aes(x = as.Date(data))) +
  geom_line(aes(y = zamkniecie))





dbDisconnect(db)
