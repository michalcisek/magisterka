reguly_kombinacyjne <- function(stock_param, index_param, return_param){  
  
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
  
  
  ### POBRANIE PRAWDOPODOBIENSTW ZE ZBIORU TESTOWEGO
  dane <- read.csv2(paste("wyniki_bazowe/pred_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"))
  
  if (any(grepl("adaboost", colnames(dane)) == T)){
    cols <- ncol(dane)
    dane <- dane[, -c(cols - 1, cols)]
  }
  
  
  #srednie prawdopodobienstwo
  wzrost <- dane[, seq(2, ncol(dane), by = 2)]
  wzrost$srednia <- apply(wzrost, 1, mean)
  wzrost$srednia_decyzja <- ifelse(wzrost$srednia >= 0.5, "wzrost", "spadek") %>% factor()
  
  conf <- table(wzrost$srednia_decyzja, test$target)
  conf1 <- confusionMatrix(conf)
  srednia <- round(c(conf1$overall, conf1$byClass), 15)
  
  srednia <- data.frame(Method = "Srednia", t(srednia))
  
  #majority vote
  wzrost <- dane[, seq(2, ncol(dane), by = 2)]
  wzrost <- apply(wzrost, 2, function(x) ifelse(x >= 0.5, "wzrost", "spadek")) %>% as.data.frame()
  wzrost$majority_wzrost <- apply(wzrost, 1, function(x) length(which(x == "wzrost"))) 
  
  klasyfikatory <- (ncol(dane)/2)
  if ((klasyfikatory %% 2) == 0){
    wzrost$majority <- ifelse(wzrost$majority_wzrost >= klasyfikatory/2, "wzrost", "spadek")
  } else {
    wzrost$majority <- ifelse(wzrost$majority_wzrost > klasyfikatory/2, "wzrost", "spadek")
  }
  
  conf <- table(wzrost$majority, test$target)
  conf1 <- confusionMatrix(conf)
  majority <- round(c(conf1$overall, conf1$byClass), 15)
  
  majority <- data.frame(Method = "Majority vote", t(majority))
  
  
  #srednie wazone prawdopodobienstwo
  wyn_trening <- read.csv2(paste("wyniki_bazowe/wyniki_bazowe_modele", stock_param, index_param, return_param, ".csv", sep="_"))
  
  wyn_test <- read.csv2(paste("wyniki_bazowe/wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"))
  wyn_test <- wyn_test[-which(wyn_test$Method == "adaboost"), ]
  
  wyn <- c(by(wyn_trening, wyn_trening$method, function(x) max(x$Accuracy)))

  met1 <- data.frame(method_test = as.character(wyn_test$Method))
  met2 <- data.frame(method = names(wyn), accuracy = wyn)
  wagi <- sqldf::sqldf("select a.*, b.accuracy from met1 a left join met2 b on a.method_test = b.method")
  if (sum(is.na(wagi$accuracy)) > 0){
    wagi[which(is.na(wagi$accuracy)), "accuracy"] <- 0
  }
  
  wagi <- wagi$accuracy/sum(wagi$accuracy)
  
  wzrost <- dane[, seq(2, ncol(dane), by = 2)]
  wzrost$srednia_wazona <- apply(wzrost, 1, function(x) weighted.mean(x, wagi))
  wzrost$srednia_decyzja <- ifelse(wzrost$srednia_wazona >= 0.5, "wzrost", "spadek") %>% factor()
  
  conf <- table(wzrost$srednia_decyzja, test$target)
  conf1 <- confusionMatrix(conf)
  srednia_wazona <- round(c(conf1$overall, conf1$byClass), 15)
  
  srednia_wazona <- data.frame(Method = "Srednia wazona", t(srednia_wazona))
  
  
  #mediana
  wzrost <- dane[, seq(2, ncol(dane), by = 2)]
  wzrost$mediana <- apply(wzrost, 1, median)
  wzrost$mediana_decyzja <- ifelse(wzrost$mediana >= 0.5, "wzrost", "spadek") %>% factor()
  
  conf <- table(wzrost$mediana_decyzja, test$target)
  conf1 <- confusionMatrix(conf)
  mediana <- round(c(conf1$overall, conf1$byClass), 15)
  
  mediana <- data.frame(Method = "Mediana", t(mediana))
  
  #maximum rule
  wzrost <- dane[, seq(2, ncol(dane), by = 2)]
  wzrost$maks <- apply(wzrost, 1, max)
  wzrost$maks_decyzja <- ifelse(wzrost$maks >= 0.5, "wzrost", "spadek") %>% factor()
  
  conf <- suppressWarnings(confusionMatrix(wzrost$maks_decyzja, test$target))

  maks <- round(c(conf$overall, conf$byClass), 15)
  
  maks <- data.frame(Method = "Regula maksimum", t(maks))
  
  #minimum rule
  wzrost <- dane[, seq(2, ncol(dane), by = 2)]
  wzrost$min <- apply(wzrost, 1, min)
  wzrost$min_decyzja <- ifelse(wzrost$min >= 0.5, "wzrost", "spadek") %>% factor()
  
  conf <- suppressWarnings(confusionMatrix(wzrost$min_decyzja, test$target))

  min <- round(c(conf$overall, conf$byClass), 15)
  
  min <- data.frame(Method = "Regula minimum", t(min))
  
  
  
  #zapis danych
  wyniki_test <- read.csv2(paste("wyniki_bazowe/wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), 
                           stringsAsFactors = F)
  wyniki_test$Method <- wyniki_test$Method %>% as.factor()
  wyniki_test[, -1] <- apply(wyniki_test[, -1], 2, as.numeric)
  
  
  zapis <- rbind(srednia, srednia_wazona, majority, mediana, maks, min)
  
  wyniki_test <- rbind(wyniki_test, zapis)
  write.csv2(wyniki_test, paste("reguly_kombinacyjne/wyniki_bazowe_modele_test", stock_param, index_param, return_param, ".csv", sep="_"), row.names = F)
}
