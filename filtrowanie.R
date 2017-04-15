library(dplyr)
library(ggplot2)
library(tibble)
library(TTR)
library(RSQLite)

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
  group_by(nazwa) 


library(lubridate)
install.packages("RcppRoll")
library(RcppRoll)
library(quantmod)


dane %>% 
  filter(nazwa=="KGHM") %>%
  select(data, zamkniecie) %>% 
  filter(year(data) == 2016) %>% 
  mutate(momentum = ifelse(zamkniecie > lag(zamkniecie), 1, -1)) %>% 
  mutate(stock_momentum = roll_meanr(momentum, 5)) %>% 
  mutate(arit_return = c(Delt(zamkniecie, k=1))) %>% 
  mutate(volatility = roll_meanr(arit_return, 5))



# do policzenia danego wskaznika w pipie dla kazdej akcji
#do(mutate(., CCI = CCI(`[`(., c('maksimum', 'minimum', 'zamkniecie'))))) -> dane





dbDisconnect(db)
