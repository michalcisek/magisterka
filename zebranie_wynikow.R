rm(list=ls())
library(ggplot2)
library(tibble)
library(TTR)
library(RSQLite)
library(lubridate)
library(RcppRoll)
library(quantmod)
library(caret)
library (plyr)
library(dplyr)

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


#zwraca liste najmniej podobnych modeli z pakietu Caret na podstawie wsp. podob. Jaccarda
#nazwa - nazwa modelu po ktorym ma nastapic przeszukiwanie
#liczba - ile najmniej podobnych modeli ma byc zwroconych
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

index_params <- c(5, 10, 20, 90, 270)
stock_params <- c(5, 10, 20, 90, 270)
return_params <- c(1, 5, 10, 20, 90, 270)

grid <- expand.grid(stock_param = stock_params, index_param = index_params, return_param = return_params)
# grid <- grid[c(1, 2, 3, 4, 5, 6, 7, 150, 144, 138, 132, 126, 125, 119, 113, 107, 101, 100, 94, 88, 82, 76, 
#                75, 69, 63, 57, 51, 50, 44, 38, 32, 26, 25, 19, 13),]



# Budowa modeli bazowych --------------------------------------------------
source("bazowe_modele.R")
sapply(1:nrow(grid), function(x) bazowe_modele(stock_param = grid$stock_param[x], 
                                               index_param = grid$index_param[x],
                                               return_param = grid$return_param[x]))


# Usuniecie wynikow svmRadial z tych wartosci grida gdzie wywalal bledy -------------------
sprawdz_braki_trening <- function(stock, index, return){
  pred_trening <- read.csv2(paste("pred_bazowe_modele_trening", stock, index, return, ".csv", sep="_"))
  braki <- apply(pred_trening, 2, function(x) sum(is.na(x)))
  return(braki)
}

braki <- lapply(1:nrow(grid), function(x) sprawdz_braki_trening(grid[x, 1], grid[x, 2], grid[x, 3]))
braki <- do.call(rbind, braki)
braki <- as.data.frame(braki)
usun <- which(braki$svmRadial > 0)

for(i in usun){
  pred_trening <- read.csv2(paste("stare/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  pred_trening <- pred_trening[, -which(colnames(pred_trening) == "svmRadial")]
  write.csv2(pred_trening, paste("stare/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"), row.names = F)
}



# Usuniecie predykcji adaboost (wychodza z poza przedzial) ----------------


# Zmergowanie wynikow nowych modeli ze starymi (merge csv-ek dla tych samych wartosci z grida --------
nierowne <- c()
for(i in 1:nrow(grid)){
  pred_trening1 <- read.csv2(paste("stare/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  pred_trening2 <- read.csv2(paste("pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  if (nrow(pred_trening1) != nrow(pred_trening2)) nierowne <- c(nierowne, i)
}


for(i in 1:nrow(grid)){
  if (i %in% nierowne) {
    next
  } else{
    pred_test1 <- read.csv2(paste("stare/pred_bazowe_modele_test", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    pred_test2 <- read.csv2(paste("pred_bazowe_modele_test", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    pred_test <- cbind(pred_test1, pred_test2)
    write.csv2(pred_test, paste("wyniki_bazowe/pred_bazowe_modele_test", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"), row.names = F)
    
    
    pred_trening1 <- read.csv2(paste("stare/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    pred_trening2 <- read.csv2(paste("pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    pred_trening <- cbind(pred_trening1, pred_trening2)
    write.csv2(pred_trening, paste("wyniki_bazowe/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"), row.names = F)
    
    
    wyniki_test1 <- read.csv2(paste("stare/wyniki_bazowe_modele_test", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    wyniki_test2 <- read.csv2(paste("wyniki_bazowe_modele_test", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    wyniki_test <- rbind(wyniki_test1, wyniki_test2)
    write.csv2(wyniki_test, paste("wyniki_bazowe/wyniki_bazowe_modele_test", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"), row.names = F)
    
    
    wyniki_trening1 <- read.csv2(paste("stare/wyniki_bazowe_modele", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    wyniki_trening2 <- read.csv2(paste("wyniki_bazowe_modele", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
    wyniki_trening <- rbind(wyniki_trening1, wyniki_trening2)
    write.csv2(wyniki_trening, paste("wyniki_bazowe/wyniki_bazowe_modele", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"), row.names = F)
  }
}



# Reguly kombinacyjne -----------------------------------------------------
nierowne <- c()
for(i in 1:nrow(grid)){
  pred_trening1 <- read.csv2(paste("stare/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  pred_trening2 <- read.csv2(paste("pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  if (nrow(pred_trening1) != nrow(pred_trening2)) nierowne <- c(nierowne, i)
}

source("reguly_kombinacyjne.R")

for(i in 1:nrow(grid)){
  if(i %in% nierowne){
    next
  } else{
    reguly_kombinacyjne(grid[i, 1], grid[i, 2], grid[i, 3])    
  }
}



# Stacking ----------------------------------------------------------------
nierowne <- c()
for(i in 1:nrow(grid)){
  pred_trening1 <- read.csv2(paste("stare/pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  pred_trening2 <- read.csv2(paste("pred_bazowe_modele_trening", grid[i, 1], grid[i, 2], grid[i, 3], ".csv", sep="_"))
  if (nrow(pred_trening1) != nrow(pred_trening2)) nierowne <- c(nierowne, i)
}

source("stacking.R")

for(i in 1:nrow(grid)){
  if(i %in% nierowne){
    next
  } else{
    stacking(grid[i, 1], grid[i, 2], grid[i, 3])    
  }
}


# zebranie wynikow dla tego samego okna predykcji do osobnych katalogow --------
return_param <- c(1, 5, 10, 20, 90, 270)

pliki <- list.files("stacking_wyniki")

pliki1 <- sapply(pliki, function(x) strsplit(x, "_"))
pliki1 <- sapply(pliki1, function(x) x[7])

my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

przenies <- function(pliki, ret){
  nazwy <- names(pliki)[which(pliki == ret)]
  sapply(nazwy, function(x) my.file.rename(paste0("stacking_wyniki/", x), paste0("stacking_wyniki/return_", ret, "/", x)))
}

sapply(return_param, function(x) przenies(pliki1, x))


# Zebranie wynikow dla poszczegolnych okien predykcji do jednego pliku --------
return_param <- c(1, 5, 10, 20, 90, 270)

koncowe_wyniki <- function(ret){
  sciezka <- paste0("stacking_wyniki/return_", ret)
  pliki <- list.files(sciezka)
  dane <- read.csv2(paste0(sciezka, "/", pliki[1]))
  
  dane <- data.frame(Stock_param = unlist(strsplit(pliki[1], "_"))[5], Index_param = unlist(strsplit(pliki[1], "_"))[6], dane)
  
  for (i in 2:length(pliki)){
    dane1 <- read.csv2(paste0(sciezka, "/", pliki[i]))
    
    dane1 <- data.frame(Stock_param = unlist(strsplit(pliki[i], "_"))[5], Index_param = unlist(strsplit(pliki[i], "_"))[6], dane1)
    dane <- rbind(dane, dane1)
  }
  
  write.csv2(dane, paste0("koncowe_wyniki/wyniki_", ret, "_.csv"), row.names = F)
}
sapply(return_param, koncowe_wyniki)

# Wizualizacja koncowych wynikow ------------------------------------------
sciezka <- paste0("koncowe_wyniki")
pliki <- list.files(sciezka)
dane <- read.csv2(paste0(sciezka, "/", pliki[1]))
dane <- data.frame(Return_param = unlist(strsplit(pliki[1], "_"))[2], dane)


for (i in 2:length(pliki)){
  dane1 <- read.csv2(paste0(sciezka, "/", pliki[i]))
  dane1 <- data.frame(Return_param = unlist(strsplit(pliki[i], "_"))[2], dane1)
  
  dane <- rbind(dane, dane1)
}

dane$group <- paste0(dane$Stock_param, "_", dane$Index_param, "_", dane$Return_param) %>% factor

library(corrplot)
library(ggplot2)
library(gridExtra)

for(i in levels(dane$group)){
  sub <- dane[which(dane$group == i), ]
  sub %>% 
    select(Method, Accuracy) -> sub
  sub <- sub[order(sub$Accuracy, decreasing = F), ]
  sub$Method <- factor(sub$Method, levels = sub$Method)
  

  png(paste0("koncowe_wyniki/skutecznosc", i, ".png"), width = 775, height = 475, units = "px")
  gg <- ggplot(sub, aes(x = Method, y = Accuracy, fill = Method)) +
    geom_bar(stat = "identity")+
    theme(legend.position = "none")+
    coord_flip()
  print(gg)
  dev.off()
  
  cor <- read.csv2(paste0("korelacja_bazowe/wyniki_bazowe_modele_test_", i, "_.csv"))
  row.names(cor) <- colnames(cor)
  
  png(paste0("koncowe_wyniki/korelacja_", i, ".png"), width = 775, height = 475, units = "px")
  corrplot(as.matrix(cor), type = "upper", method = "number")
  dev.off()
  
}

# Funkcja wizualizujaca i zapisujaca performance na zbiorze treningowym dla konkretych zmiennych z grida --------
wizualizacja_trening <- function(stock, index, return){
  dane <- read.csv2(paste("stare/wyniki_bazowe_modele", stock, index, return, ".csv", sep="_"))
  
}

sapply(1:nrow(grid), function(x) wizualizacja_trening(grid[x, 1], grid[x, 2], grid[x, 3]))



# funkcja wizualizujaca i zapisujaca performance na zbiorze testowym dla konkretnych zmiennych z grida --------
wizualizacja_test <- function(stock, index, return){
  dane <- read.csv2(paste("stare/wyniki_bazowe_modele_test", stock, index, return, ".csv", sep="_"))
  
  png(paste0("wizualizacje_test/", stock, "_", index, "_", return, ".png"), width = 775, height = 475, units = "px")
  gg <- ggplot(dane, aes(x = Method, y = Accuracy, fill = Method)) +
    geom_bar(stat = "identity")+
    theme(legend.position = "none")
  print(gg)
  dev.off()
  
}

sapply(1:nrow(grid), function(x) wizualizacja_test(grid[x, 1], grid[x, 2], grid[x, 3]))

