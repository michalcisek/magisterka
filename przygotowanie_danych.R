library(ggplot2)
library(tibble)
library(TTR)
library(RSQLite)
library(lubridate)
library(RcppRoll)
library(quantmod)
library(caret)
library(dplyr)


# Funkcje -----------------------------------------------------------------

#funkcja zwracajaca podsumowanie dotyczace akcji (ile notowan, data pierwszego i ostatniego notowania)
podsumowanie <- function(nazwy_akcji){
  query <- paste("select nazwa, count(*) l_dni, min(data) min, max(data) max from notowania
                 where nazwa in('",paste(nazwy_akcji,collapse="' , '"),"')
                 group by nazwa",sep="")
  pods <- dbGetQuery(db, query)
  return(pods)
}


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

#funkcja wyciagajaca z przynaleznosci tylko udzial w indeksie sektorowym danej spolki
dopasuj_sektor <- function(tekst, zbior_indeksow){
  
  sektor <- sapply(zbior_indeksow, function(x) grepl(x, tekst))
  sektor <- zbior_indeksow[which(sektor == T)]
  if (length(sektor) > 1){
    return(paste(sektor, collapse = " --- "))
  } else if (length(sektor) == 0) {
    return(NA)
  } else {
    return(sektor)
  }
}

#zwraca liste najmniej podobnych modeli z pakietu Caret na podstawie wsp. podob. Jaccarda
#nazwa - nazwa modelu po ktorym ma nastapic przeszukiwanie
#liczba - ile najmniej podobnych modeli ma być zwroconych
#typ - typ modeli po ktorych funkcja ma szukac, np. "Classification", "Regression"
max_model_roznice <- function(nazwa, liczba, typ){
  
  tag <- read.csv("tag_data.csv", row.names = 1)
  tag <- as.matrix(tag)
  
  Models <- tag[tag[,typ] == 1,]
  
  all <- 1:nrow(Models)
  
  start <- grep(paste("(", nazwa,")", sep=""), rownames(Models), fixed = TRUE)
  pool <- all[all != start]
  
  nextMods <- maxDissim(Models[start,,drop = FALSE], 
                        Models[pool, ], 
                        method = "Jaccard",
                        n = liczba)
  
  models <- rownames(Models)[c(start, nextMods)]
  print(models)
  
  regmatches(models, gregexpr("(?<=\\().*?(?=\\))", models, perl=T)) %>% 
    unlist -> models
  
  return(models)
}


# Przygotowanie notowan akcji ---------------------------------------------

#pobranie danych z bazy
db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
dane_akcje <- dbGetQuery(db, "select * from notowania")

#wstepna obrobka danych
dane_akcje %>% 
  #wyrzucenie niepotrzebnych kolumn
  select(-one_of('waluta', 'zmiana_kursu', 'transakcje', 'wartosc_obrotu')) %>% 
  #odfiltrowanie rekordow dla ktorych nie bylo ceny maks, min i otwarcia - czyli z dawnych lat
  filter(otwarcie != 0 | maksimum != 0 | minimum != 0) %>% 
  group_by(nazwa) -> dane_akcje

#wektor unikalnych nazw akcji w bazie
akcje <- unique(dane_akcje$nazwa)

#podsumowanie dotyczace akcji
pods <- podsumowanie(akcje) %>% as_tibble()

#ostatni dzien notowan
maks_data <- max(pods$max)

#subset spolek debiutujacych przed 2016 rokiem i ktore nadal sa notowane
pods %>% 
  filter(max == maks_data, min < '2016-01-01') %>%
  arrange(desc(l_dni)) -> wybrane_akcje

# saveRDS(wybrane_akcje, "wybrane_akcje.rds")
rm(pods)


#funkcja przygotowujaca notowania akcji - odfiltrowanie, wyliczenie zmiennych
przygotuj_dane_akcje <- function(dane, stock_param, ret){
  
  dane %>% 
    filter(nazwa %in% wybrane_akcje$nazwa) %>% 
    summarise(l_notowan = n()) %>% 
    filter(l_notowan > ret) -> pods
  
  dane %>% 
    filter(nazwa %in% pods$nazwa) %>% 
    #wyliczenie momentum
    do(mutate(., stock_momentum = roll_meanr(ifelse(zamkniecie >= lag(zamkniecie), 1, -1), stock_param))) %>% 
    #wyliczenie volatility
    do(mutate(., stock_volatility = roll_meanr(c(Delt(zamkniecie, k=1)), stock_param))) %>%  
    #wyliczenie opoznionych zwrotow
    do(mutate(., lagged_return = lead(c(Delt(zamkniecie, k=ret)), ret))) %>% 
    #wyliczenie zmiennej objasnianej
    mutate(target = factor(ifelse(lagged_return >= 0, 1, 0))) %>% 
    #wybranie tylko kompletnych wierszy
    `[`(complete.cases(.), ) -> got_dane
  
  return(got_dane)
}


dbDisconnect(db)
rm(db, akcje, maks_data)


# Przygotowanie notowan indeksow ------------------------------------------

#flaga czy scrapowac przynaleznosc danych akcji do indeksow (czasochlonne)
pobierz_przynaleznosc <- FALSE

#pobranie danych z bazy
db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
spolki <- dbGetQuery(db, "select distinct nazwa, ISIN from notowania")

#wybieramy tylko te spolki ktore przeszly filtrowanie
spolki <- spolki[which(spolki$nazwa %in% wybrane_akcje$nazwa), ]

#slownik indeksow
indeksy_sektorowe <- c('WIG-BANKI', 'WIG-BUDOW', 'WIG-CHEMIA', 'WIG-ENERG', 'WIG-GORNIC', 
             'WIG-INFO', 'WIG-LEKI', 'WIG-MEDIA', 'WIG-MOTO', 'WIG-NRCHOM', 
             'WIG-ODZIEZ', 'WIG-PALIWA', 'WIG-SPOZYW', 'WIG-TELEKOM')
indeks_najwiekszych <- c('WIG20')

#pobranie/zaladowanie przynaleznosci akcji do indeksow
if (pobierz_przynaleznosc){
  przynaleznosc <- sapply(spolki$ISIN, scrapuj_przynaleznosc)
  df <- data.frame(spolki, przynaleznosc)
  saveRDS(df, "przynaleznosc_indeks.rds")
  rm(df)
} else {
  przynaleznosc <- readRDS("przynaleznosc_indeks.rds")
}


#przypisanie spolce indeksu (ze wskazanego zbioru) do ktorego nalezy
zbior_indeksow <- indeks_najwiekszych

spolki$sektor <- sapply(przynaleznosc$przynaleznosc, function(x) dopasuj_sektor(x, zbior_indeksow = zbior_indeksow)) 
spolki$sektor <- unlist(spolki$sektor)
spolki[which(is.na(spolki$sektor)), "sektor"] <- "brak"

#pobranie notowan
dane_indeksy <- dbGetQuery(db, "select * from indeksy")
dbDisconnect(db)
rm(db, przynaleznosc)

przygotuj_dane_indeksy <- function(dane, index_param){
  
  dane %>% 
    select(-one_of('waluta', 'zmiana_kursu', 'wolumen', 'transakcje', 'wartosc_obrotu')) %>% 
    filter(nazwa %in% zbior_indeksow) %>% 
    group_by(nazwa) %>% 
    #wyliczenie momentum
    do(mutate(., index_momentum = roll_meanr(ifelse(zamkniecie >= lag(zamkniecie), 1, -1), index_param))) %>% 
    #wyliczenie volatility
    do(mutate(., index_volatility = roll_meanr(c(Delt(zamkniecie, k=1)), index_param))) %>% 
    #wybranie tylko kompletnych wierszy
    `[`(complete.cases(.), ) -> got_dane
  
  return(got_dane)  
}  



# Polaczenie notowan akcji i indeksow -------------------------------------
# index_params <- c(5, 10, 20, 90, 270)
# stock_params <- c(5, 10, 20, 90, 270)
# return_params <- c(1, 5, 10, 20, 90, 270)


stock_param <- 5
index_param <- 5
return_param <- 1
  
  
filt_akcje <- tbl_df(przygotuj_dane_akcje(dane_akcje, stock_param, return_param))
filt_indeksy <- tbl_df(przygotuj_dane_indeksy(dane_indeksy, index_param))
  
filt_akcje %>% 
  #dolaczenie do notowan akcji indeksu do jakiego nalezy
  left_join(., tbl_df(spolki), by=c("nazwa"="nazwa", "ISIN"="ISIN")) %>% 
  #dolaczenie do notowan akcji notowan indeksu do jakiego nalezy
  left_join(., filt_indeksy, by=c("sektor"="nazwa", "data"="data")) %>% 
  #pozbycie sie niepotrzebnych kolumn
  select(-one_of("ISIN.y","otwarcie.y", "maksimum.y", "minimum.y", "zamkniecie.y")) %>% 
  #zmiana indeksu na zmienna kategoryczna
  mutate(sektor = factor(sektor)) %>% 
  #odfiltrowanie spolek ktore nie naleza do wskazanego zbioru indeksow (np. indeksow sektorowych)
  filter(sektor != 'brak') %>% 
  #odfiltrowanie wierszy ktore zawieraja braki danych (na wszelki wypadke)
  `[`(complete.cases(.), ) -> final_dane

  
set.seed(56283)
tren <- createDataPartition(final_dane$target, p=0.6, list = F)

trening <- final_dane[tren, c("target", "stock_momentum", "stock_volatility", 
                              "index_momentum", "index_volatility")]
test <- final_dane[-tren, c("target", "stock_momentum", "stock_volatility", 
                              "index_momentum", "index_volatility")]
  

zalezna <- "target"
niezalezne <- c("stock_momentum", "stock_volatility", "index_momentum", "index_volatility")
  
# models <- max_model_roznice("xgbTree", 20, "Classification")
  

fitControl <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'all',
  classProbs = T)



knnGrid <- expand.grid(k = c(3,5,10,20,30,50,100,200))
set.seed(56283)
tryCatch({knn_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                             method = "knn",
                             trControl = fitControl,
                             tuneGrid = knnGrid,
                             preProcess = c("center", "scale"))
}, error = function(e){
  blad <- e
})

if(exists("knn_model", inherits = F)){
  saveRDS(knn_model, paste("knn_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu knn zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}
  
print(eval(wynik))
rm(knn_model)  


  
rfGrid <- expand.grid(mtry = c(1,2,3,4))
set.seed(56283)
tryCatch({rf_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                             method = "rf",
                             trControl = fitControl,
                             tuneGrid = rfGrid)
}, error = function(e){
  blad <- e
})

if(exists("rf_model", inherits = F)){
  saveRDS(rf_model, paste("rf_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu random forest zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))
rm(rf_model) 



set.seed(56283)
tryCatch({glm_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                            method = "glm",
                            trControl = fitControl,
                            family="binomial")
}, error = function(e){
  blad <- e
})

if(exists("glm_model", inherits = F)){
  saveRDS(glm_model, paste("glm_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu regresji logistycznej zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))
rm(glm_model) 




set.seed(56283)
tryCatch({lda_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                             method = "lda",
                             trControl = fitControl)
}, error = function(e){
  blad <- e
})

if(exists("lda_model", inherits = F)){
  saveRDS(lda_model, paste("lda_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu LDA zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))
rm(lda_model) 
  



set.seed(56283)
tryCatch({qda_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                             method = "qda",
                             trControl = fitControl)
}, error = function(e){
  blad <- e
})

if(exists("qda_model", inherits = F)){
  saveRDS(qda_model, paste("qda_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu QDA zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))
rm(qda_model) 
  



set.seed(56283)
tryCatch({cart_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                             method = "rpart",
                             trControl = fitControl,
                             tuneLength = 20)
}, error = function(e){
  blad <- e
})

if(exists("cart_model", inherits = F)){
  saveRDS(cart_model, paste("cart_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu CART zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))
rm(cart_model) 




set.seed(56283)
tryCatch({svmRadial_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                              method = "svmRadial",
                              trControl = fitControl,
                              preProc = c("center", "scale"),
                              tuneLength = 8)
}, error = function(e){
  blad <- e
})

if(exists("svmRadial_model", inherits = F)){
  saveRDS(svmRadial_model, paste("svmRadial_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu svmRadial zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))

  
set.seed(56283)
tryCatch({xgbTree_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                                   method = "xgbTree",
                                   trControl = fitControl,
                                   tuneLength = 8)
}, error = function(e){
  blad <- e
})

if(exists("xgbTree_model", inherits = F)){
  saveRDS(xgbTree_model, paste("xgbTree_model.rds", sep=""))
  wynik <- quote(paste("Wyniki modelu xgbTree zapisane!"))
} else{
  wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
}

print(eval(wynik))



rm(spolki, wybrane_akcje, pobierz_przynaleznosc)
rm(dane_akcje, dane_indeksy, filt_akcje, filt_indeksy)




svm.tune
# do policzenia danego wskaznika w pipie dla kazdej akcji
#do(mutate(., CCI = CCI(`[`(., c('maksimum', 'minimum', 'zamkniecie'))))) -> dane



