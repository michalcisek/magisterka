library(xtable)
set.seed(56283)
ak <- dane_akcje[sample(nrow(dane_akcje), 10), ]
print(xtable(ak), include.rownames = FALSE)

set.seed(56283)
ind <- dane_indeksy[sample(nrow(dane_indeksy), 10), ]
print(xtable(ind), include.rownames = FALSE)


data.frame(table(spolki$sektor))


factor(dane_indeksy$nazwa)



ret <- c(1, 5, 10, 20, 90, 270)

i <- 270

for(i in ret){
  dane <- read.csv2(paste0("koncowe_wyniki/wyniki_", i, "_.csv"))
  dane <- dane[, 1:4]
  dane <- dane[order(-dane$Accuracy), ]
  dane <- head(dane, 5)
  print(xtable::xtable(dane, digits = 7), include.rownames = F)
  
}



ret <- 270
sciezka <- paste0("rl_wsp")
pliki <- list.files(sciezka)

pl <- sapply(pliki, function(x) strsplit(x, "_"))
pl <- unlist(pl)
pl <- t(matrix(pl, nrow = 6))
pl <- as.data.frame(pl)

df <- pl[which(pl$V5 == ret),]


options(scipen = 999)
wsp <- list()
for(i in 1:nrow(df)){
  dane <- read.csv2(paste0("rl_wsp/", paste(df[i, 1], df[i, 2], df[i, 3], df[i, 4], df[i, 5], df[i, 6], sep="_")))
  dane <- dane[, c(1, 2, 5)]
  colnames(dane) <- c("Model bazowy", "Oszacowanie", "P-value")
  
  dane1 <- data.frame(t(dane), stringsAsFactors = F)
  colnames(dane1) <- dane$`Model bazowy`
  
  dane1 <- dane1[-1, ]
  dane1 <- apply(dane1, 2, as.numeric)
  
  wyn <- paste("\\makecell{", dane1[1, ], " \\\\ (", round(dane1[2, ], 5), ")}", sep = "")
  wyn <- data.frame(t(wyn))
  colnames(wyn) <- dane$`Model bazowy`
  wsp[[i]] <- wyn
}
wsp <- plyr::rbind.fill(wsp)

wsp <- data.frame(`Akcje parametr` = df$V3, `Indeks parametr` = df$V4, wsp)

options(xtable.sanitize.text.function=identity)
print(xtable::xtable(wsp), include.rownames = F, sanitize.rownames.function = identity)




ret <- 10

ist_mod_baz <- function(ret){
  sciezka <- paste0("rl_wsp")
  pliki <- list.files(sciezka)
  
  pl <- sapply(pliki, function(x) strsplit(x, "_"))
  pl <- unlist(pl)
  pl <- t(matrix(pl, nrow = 6))
  pl <- as.data.frame(pl)
  
  df <- pl[which(pl$V5 == ret),]
  
  
  options(scipen = 999)
  wsp <- list()
  for(i in 1:nrow(df)){
    dane <- read.csv2(paste0("rl_wsp/", paste(df[i, 1], df[i, 2], df[i, 3], df[i, 4], df[i, 5], df[i, 6], sep="_")))
    dane <- dane[, c(1, 5)]
    
    dane[, 2] <- ifelse(dane[, 2] < 0.05, T, F)
    
    
    dane1 <- data.frame(t(dane), stringsAsFactors = F)
    colnames(dane1) <- dane[, 1]
    dane1 <- dane1[-1, ]
    
    wsp[[i]] <- dane1
  }
  wsp <- plyr::rbind.fill(wsp)
  apply(wsp, 2, function(x) length(which(x == " TRUE")))/nrow(wsp)*100
}

ret <- c(1,5,10,20,90,270)
wr <- sapply(ret, ist_mod_baz)
plyr::rbind.fill(wr)
