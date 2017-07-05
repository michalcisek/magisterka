bazowe_modele <- function(stock_param, index_param, return_param){

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
    #odfiltrowanie wierszy ktore zawieraja braki danych (na wszelki wypadek)
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
  
  
  # KNN ---------------------------------------------------------------------
  set.seed(56283)
  knnGrid <- expand.grid(k = c(3,5,10,20,30,50,100,200))
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
    wynik <- quote(writeLines(as.character(blad)))
  }

  print(eval(wynik))
  rm(knn_model)


  # Random forest -----------------------------------------------------------
  set.seed(56283)
  rfGrid <- expand.grid(mtry = c(1,2,3))
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



  # regresja logistyczna ----------------------------------------------------
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




  # LDA ---------------------------------------------------------------------
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




  # QDA ---------------------------------------------------------------------
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




  # CART -----------------------------------------------------------
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
  
  # SVM Radial --------------------------------------------------------------
  set.seed(56283)

  sigmas <- kernlab::sigest(target~., data = trening, na.action = na.omit, scaled = TRUE)
  svmGrid <- expand.grid(sigma = mean(as.vector(sigmas[-2])), C = 2^((1:8) -3)[5:8])
  tryCatch({svmRadial_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                                     method = "svmRadial",
                                     trControl = fitControl,
                                     preProc = c("center", "scale"),
                                     tuneGrid = svmGrid)
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
  rm(svmRadial_model)
  
  # XGBoost -----------------------------------------------------------------
  set.seed(56283)
  xgbGrid <- expand.grid(nrounds = 100, eta = 0.3, gamma = 0, min_child_weight = 1, subsample = c(.5, 1), colsample_bytree = c(.6, .8, 1), max_depth = c(2, 4, 6))
  tryCatch({xgbTree_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
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
  
  # svmLinear ---------------------------------------------------------------
  set.seed(56283)
  tryCatch({svmLinear_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                                   method = "svmLinear",
                                   trControl = fitControl,
                                   tuneLength = 2)
  }, error = function(e){
    blad <- e
  })

  if(exists("svmLinear_model", inherits = F)){
    saveRDS(svmLinear_model, paste("svmLinear_model.rds", sep=""))
    wynik <- quote(paste("Wyniki modelu svmLinear zapisane!"))
  } else{
    wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
  }

  print(eval(wynik))
  rm(svmLinear_model)
  
  # svmPoly ---------------------------------------------------------------
  set.seed(56283)
  tryCatch({svmPoly_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                                     method = "svmPoly",
                                     trControl = fitControl,
                                     tuneLength = 2)
  }, error = function(e){
    blad <- e
  })

  if(exists("svmPoly_model", inherits = F)){
    saveRDS(svmPoly_model, paste("svmPoly_model.rds", sep=""))
    wynik <- quote(paste("Wyniki modelu svmPoly zapisane!"))
  } else{
    wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
  }

  print(eval(wynik))
  rm(svmPoly_model)
  
  # bagged CART ---------------------------------------------------------------
  set.seed(56283)
  tryCatch({treebag_model <- train(x = trening[, niezalezne], y = make.names(trening$target),
                                     method = "treebag",
                                     trControl = fitControl)
  }, error = function(e){
    blad <- e
  })
  
  if(exists("treebag_model", inherits = F)){
    saveRDS(treebag_model, paste("treebag_model.rds", sep=""))
    wynik <- quote(paste("Wyniki modelu treebag zapisane!"))
  } else{
    wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
  }
  
  print(eval(wynik))
  rm(treebag_model)
  
  # AdaBoost ---------------------------------------------------------------
  set.seed(56283)
  adaGrid <- expand.grid(method = "Real adaboost", nIter = 50)
  tryCatch({adaboost_model <- train(target ~ ., data = trening,
                                     method = "adaboost",
                                     trControl = fitControl,
                                     tuneGrid = adaGrid)
  }, error = function(e){
    blad <- e
  })
  
  if(exists("adaboost_model", inherits = F)){
    saveRDS(adaboost_model, paste("adaboost_model.rds", sep=""))
    wynik <- quote(paste("Wyniki modelu adaboost zapisane!"))
  } else{
    wynik <- quote(writeLines(c("---------------------------------", as.character(blad), "---------------------------------")))
  }
  
  print(eval(wynik))
  rm(adaboost_model)
  

  # Zapis wynikow modeli na zbiorze treningowym----------------------------------------------------
  modele <- c("knn", "rf", "glm", "lda", "qda", "cart", "svmRadial", "xgbTree", "treebag", "adaboost")

  wyniki <- list()
  for (i in modele){
    model <- readRDS(paste0(i, "_model.rds"))
    df <-  data.frame(method = model$method, model$results)
    col <- ncol(df)
    param <- (col - 5)
    
    if(param > 1){
      parametry <- apply(df[, 2:(param + 1)], 1, paste, collapse = "; ")
      df <- df[, -(2:(param + 1))]
      df$Parametry <- parametry
      wyniki[[i]] <- df
    } else {
      df$Parametry <- df[, 2]
      df <- df[, -2]
      wyniki[[i]] <- df
    }
    
  }
  
  wyniki <- do.call(rbind, wyniki)
  write.csv2(wyniki, paste("wyniki_bazowe_modele", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)
  
  
  # Zapis wynikow najlepszych modeli na zbiorze testowym --------------------
  modele <- c("knn", "rf", "glm", "lda", "qda", "cart", "svmRadial", "xgbTree", "treebag", "adaboost")
  
  wyniki <- list()
  for (i in modele){
    model <- readRDS(paste0(i, "_model.rds"))
    pred <- predict(object = model, test[ ,niezalezne])
    
    conf <- confusionMatrix(test$target, pred)  
    
    wyniki[[i]] <- c(Method = model$method, conf$overall, conf$byClass)
  }
  wyniki <- do.call(rbind, wyniki)
  write.csv2(wyniki, paste("wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)
  
  # Zapis predykcji (prawdopodobienstw) modeli bazowych ze zbioru treningowego --------
  modele <- c("knn", "rf", "glm", "lda", "qda", "cart", "svmRadial", "xgbTree", "treebag", "adaboost")
  
  wyniki <- list()
  for (i in modele){
    model <- readRDS(paste0(i, "_model.rds"))
    pred <- model$pred$wzrost[order(model$pred$rowIndex)]
    
    wyniki[[i]] <- pred
  }
  wyniki <- do.call(cbind, wyniki)
  write.csv2(wyniki, paste("pred_bazowe_modele_trening", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)
  
  
  # Zapis predykcji (prawdopodobienstw) modeli bazowych ze zbioru testowego --------
  modele <- c("knn", "rf", "glm", "lda", "qda", "cart", "svmRadial", "xgbTree", "treebag", "adaboost")
  
  wyniki <- list()
  for (i in modele){
    model <- readRDS(paste0(i, "_model.rds"))
    pred <- predict(object = model, test[ ,niezalezne], type = "prob")
    
    wyniki[[i]] <- pred
  }
  wyniki <- do.call(cbind, wyniki)
  write.csv2(wyniki, paste("pred_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)

}  
  


