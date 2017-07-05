stacking <- function(stock_param, index_param, return_param){  
  
  #pobranie danych z bazy
  db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
  dane_akcje <- dbGetQuery(db, "select * from notowania")
  
  #wstepna obrobka danych
  usun <- which(colnames(dane_akcje) %in% c('waluta', 'zmiana_kursu', 'transakcje', 'wartosc_obrotu'))  
  dane_akcje <- dane_akcje[,-usun]
  
  dane_akcje %>% 
    #wyrzucenie niepotrzebnych kolumn
    # select(-one_of('waluta', 'zmiana_kursu', 'transakcje', 'wartosc_obrotu')) %>% 
    #odfiltrowanie rekordow dla ktorych nie bylo ceny maks, min i otwarcia - czyli z dawnych lat
    filter(otwarcie != 0 | maksimum != 0 | minimum != 0) %>% 
    group_by(nazwa) -> dane_akcje
  
  #wektor unikalnych nazw akcji w bazie
  akcje <- unique(dane_akcje$nazwa)
  
  #podsumowanie dotyczace akcji
  query <- paste("select nazwa, count(*) l_dni, min(data) min, max(data) max from notowania
                 where nazwa in('",paste(akcje, collapse="' , '"),"')
                 group by nazwa",sep="")
  pods <- dbGetQuery(db, query)
  pods %>% as_tibble() -> pods
  
  #ostatni dzien notowan
  maks_data <- max(pods$max)
  
  #subset spolek debiutujacych przed 2016 rokiem i ktore nadal sa notowane
  pods %>% 
    filter(max == maks_data, min < '2016-01-01') %>%
    arrange(desc(l_dni)) -> wybrane_akcje
  
  # saveRDS(wybrane_akcje, "wybrane_akcje.rds")
  rm(pods)
  
  
  #funkcja przygotowujaca notowania akcji - odfiltrowanie, wyliczenie zmiennych
  cci = 14
  roc = 10
  rsi = 14
  
  
  dane_akcje %>% 
    filter(nazwa %in% wybrane_akcje$nazwa) %>% 
    summarise(l_notowan = n()) %>% 
    filter(l_notowan > return_param) -> pods
  
  dane_akcje %>% 
    filter(nazwa %in% pods$nazwa) %>% 
    #wyliczenie momentum
    do(mutate(., stock_momentum = roll_meanr(ifelse(zamkniecie >= lag(zamkniecie), 1, -1), stock_param))) %>% 
    #wyliczenie volatility
    do(mutate(., stock_volatility = roll_meanr(c(Delt(zamkniecie, k=1)), stock_param))) %>%  
    #wyliczenie opoznionych zwrotow
    do(mutate(., lagged_return = lead(c(Delt(zamkniecie, k=return_param)), return_param))) %>% 
    #wyliczenie CCI
    do(mutate(., CCI = CCI(`[`(., c('maksimum', 'minimum', 'zamkniecie')), n = cci))) %>%
    #wyliczenie ROC
    do(mutate(., ROC = ROC(`[`(., c('zamkniecie')), n = roc))) %>% 
    #wyliczenie RSI
    do(mutate(., RSI = RSI(`[`(., c('zamkniecie')), n = rsi))) %>% 
    #wyliczenie WilliamsAD
    do(mutate(., Williams = williamsAD(`[`(., c('maksimum', 'minimum', 'zamkniecie'))))) %>% 
    #wyliczenie zmiennej objasnianej
    mutate(target = factor(ifelse(lagged_return >= 0, "wzrost", "spadek"))) %>% 
    #wybranie tylko kompletnych wierszy
    `[`(complete.cases(.), ) -> filt_akcje
  
  
  # dbDisconnect(db)
  rm(akcje, maks_data)
  
  
  # Przygotowanie notowan indeksow ------------------------------------------
  
  #flaga czy scrapowac przynaleznosc danych akcji do indeksow (czasochlonne)
  pobierz_przynaleznosc <- FALSE
  
  #pobranie danych z bazy
  # db <- dbConnect(SQLite(), dbname = "notowania_gpw.sqlite")
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
  
  spolki$sektor <- sapply(przynaleznosc$przynaleznosc, function(x) dopasuj_sektor(x, zbior_indeksow = zbior_indeksow)) 
  spolki$sektor <- unlist(spolki$sektor)
  spolki[which(is.na(spolki$sektor)), "sektor"] <- "brak"
  
  #pobranie notowan
  dane_indeksy <- dbGetQuery(db, "select * from indeksy")
  dbDisconnect(db)
  rm(db, przynaleznosc)
  
  usun <- which(colnames(dane_indeksy) %in% c('waluta', 'zmiana_kursu', 'wolumen', 'transakcje', 'wartosc_obrotu'))  
  dane_indeksy <- dane_indeksy[, -usun]
  dane_indeksy %>% 
    # select(-one_of('waluta', 'zmiana_kursu', 'wolumen', 'transakcje', 'wartosc_obrotu')) %>% 
    filter(nazwa %in% zbior_indeksow) %>% 
    group_by(nazwa) %>% 
    #wyliczenie momentum
    do(mutate(., index_momentum = roll_meanr(ifelse(zamkniecie >= lag(zamkniecie), 1, -1), index_param))) %>% 
    #wyliczenie volatility
    do(mutate(., index_volatility = roll_meanr(c(Delt(zamkniecie, k=1)), index_param))) %>% 
    #wybranie tylko kompletnych wierszy
    `[`(complete.cases(.), ) -> filt_indeksy
  
  
  filt_akcje %>% 
    #dolaczenie do notowan akcji indeksu do jakiego nalezy
    left_join(., tbl_df(spolki), by=c("nazwa"="nazwa", "ISIN"="ISIN")) %>% 
    #dolaczenie do notowan akcji notowan indeksu do jakiego nalezy
    left_join(., filt_indeksy, by=c("sektor"="nazwa", "data"="data")) -> filt_akcje 
  
  
  usun <- which(colnames(filt_akcje) %in% c("ISIN.y","otwarcie.y", "maksimum.y", "minimum.y", "zamkniecie.y"))  
  filt_akcje <- filt_akcje[, -usun]
  filt_akcje %>% 
    #zmiana indeksu na zmienna kategoryczna
    mutate(sektor = factor(sektor)) %>% 
    #odfiltrowanie spolek ktore nie naleza do wskazanego zbioru indeksow (np. indeksow sektorowych)
    filter(sektor != 'brak') %>% 
    #odfiltrowanie wierszy ktore zawieraja braki danych (na wszelki wypadke)
    `[`(complete.cases(.), ) -> final_dane
  
  
  set.seed(56283)
  tren <- createDataPartition(final_dane$target, p=0.6, list = F)
  
  trening <- final_dane[tren, c("target", "stock_momentum", "stock_volatility", 
                                "index_momentum", "index_volatility", "CCI", "ROC", "RSI", "Williams")]
  test <- final_dane[-tren, c("target", "stock_momentum", "stock_volatility", 
                              "index_momentum", "index_volatility", "CCI", "ROC", "RSI", "Williams")]
  
  zalezna <- "target"
  niezalezne <- c("stock_momentum", "stock_volatility", "index_momentum", "index_volatility", "CCI", "ROC", "RSI", "Williams")
  

  fitControl <- trainControl(
    method = "cv",
    number = 3,
    savePredictions = 'final',
    classProbs = T)
  
  baz_trening <- read.csv2(paste("wyniki_bazowe/pred_bazowe_modele_trening", stock_param, index_param, return_param, ".csv", sep="_"))
  baz_trening <- baz_trening[, -which(colnames(baz_trening) == "adaboost")]

  # korelacja <- cor(baz_trening, method = "kendall")
  # write.csv2(korelacja, paste("korelacja_bazowe/wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)
  
  # regresja logistyczna ----------------------------------------------------
  set.seed(56283)
  tryCatch({glm_model <- train(x = baz_trening, y = make.names(trening$target),
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
  
  #zapis wspolczynnikow regresji logistycznej
  wsp <- summary(glm_model)$coef
  write.csv2(wsp, paste("rl_wsp/glm_wspolczynniki", stock_param, index_param, return_param, ".csv", sep="_"), row.names = T)
  
  rm(glm_model)
  
  XGBoost -----------------------------------------------------------------
  set.seed(56283)
  xgbGrid <- expand.grid(nrounds = 100, eta = 0.3, gamma = 0, min_child_weight = 1, subsample = c(.5, 1), colsample_bytree = c(.6, .8, 1), max_depth = c(2, 4, 6))
  tryCatch({xgbTree_model <- train(x = baz_trening, y = make.names(trening$target),
                                   method = "xgbTree",
                                   trControl = fitControl,
                                   tuneGrid = xgbGrid)
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
  rm(xgbTree_model)
  
    
  # wczytanie zbioru testowego ----------------------------------------------
  baz_test <- read.csv2(paste("wyniki_bazowe/pred_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"))
  # baz_test <- baz_test[, seq(1, ncol(baz_test) - 1, by = 2)]
  baz_test <- baz_test[, seq(2, ncol(baz_test), by = 2)]

  kol <- colnames(baz_test)
  kol <- sapply(kol, function(x) substr(x, 1, nchar(x) - 7))
  colnames(baz_test) <- kol
  baz_test <- baz_test[, -which(colnames(baz_test) == "adaboost")]

  # Zapis predykcji (prawdopodobienstw) modeli bazowych ze zbioru testowego --------
  modele <- c("glm", "xgbTree")

  predyk <- list()
  wyniki <- list()
  for (i in modele){
    model <- readRDS(paste0(i, "_model.rds"))

    pred <- predict(object = model, baz_test)
    conf <- confusionMatrix(pred, test$target)
    stack <- round(c(conf$overall, conf$byClass), 15)
    stack <- data.frame(Method = paste0(toupper(i), " stacking"), t(stack))
    wyniki[[i]] <- stack

    pred <- predict(object = model, baz_test, type = "prob")
    predyk[[i]] <- pred
  }
  
  # zapis predykcji na zbiorze testowym -------------------------------------
  predyk <- do.call(cbind, predyk)
  write.csv2(predyk, paste("stacking_predykcje/pred_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)

  # zapis wynikow na zbiorze testowym ---------------------------------------
  wyniki_test <- read.csv2(paste("reguly_kombinacyjne/wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"),
                           stringsAsFactors = F)
  wyniki <- do.call(rbind, wyniki)

  wyniki_test <- rbind(wyniki_test, wyniki)
  write.csv2(wyniki_test, paste("stacking_wyniki/wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)
  
}
