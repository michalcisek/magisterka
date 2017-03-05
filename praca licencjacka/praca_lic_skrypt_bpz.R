# 1. WCZYTYWANIE DANYCH ---------------------------------------------------

rm(list=ls())

wig20<-read.csv("wig20.csv",header=T)
attach(wig20)
install.packages("Rcpp")
library(Rcpp)
install.packages("changepoint")
install.packages("quantmod")
install.packages("signal")
install.packages("mFilter")
install.packages("arules")
install.packages("ggplot2")
install.packages("ade4")
install.packages("cluster")
install.packages("fpc")
library(quantmod)
library(changepoint)
library(signal)
library(ifultools)
library(arules)
library(zoo)
library(mFilter)
library(ggplot2)
library(grid)
library(ade4)
library(cluster)
library(fpc)

# 2. CZYSZCZENIE SZEREGU --------------------------------------------------

#FILTR Savitzky-Golay

plotSavitzkyGolay<-function(data,p,n){
  sgol<<-sgolayfilt(data,p,n)
  plot(sgol,type="l",col="black",xlab="czas",ylab="wartosc",
       main=paste("Savitzky-Golay (", p ,",", n , ")",sep=""))
}

#FILTR Baxter-King
plotBaxterKing<-function(data,pl,pu){
  baxterking<-bkfilter(data,pl,pu)
  y<<-baxterking$trend
  plot(y,type="l",col="black",xlab="czas",ylab="wartosc",
       main=paste("Baxter-King (", pl ,",", pu , ")",sep=""))
}

#Wykresy podsumowujace wygladzanie
par(mfrow=c(2,2))
x<-c(seq(1:5523))
wig<-as.ts(Zamkniecie) 
plot(wig,type="l",col="black",main="WIG20",xlab="czas",ylab="wartosc")
plotSavitzkyGolay(Zamkniecie,3,25)
plotBaxterKing(Zamkniecie,2,100)

#Wybiera szereg uznany za najlepsze wygladzenie do dalszych operacji i zapisuje 
#go jako 'wygladzone'
#1.Savitzky-Golay
#2.Baxter-King
wybor<-function(x){
  ifelse(x==1,wygladzone<<-as.numeric(sgol),wygladzone<<-as.numeric(y))
}
wybor(1)
rm(sgol,y)

# 3.1. CHANGEPOINT PACKAGE -----------------------------------------------------

BinSeg<-cpt.mean(wygladzone,penalty="Hannan-Quinn",method = "BinSeg",Q=2000,
                 test.stat="CUSUM")
SegNeigh<-cpt.mean(wygladzone,penalty="SIC",method = "SegNeigh",Q=600,
                   test.stat="CUSUM")
PELT<-cpt.mean(wygladzone,method = "PELT", penalty = "Manual", 
               pen.value = "n^(3/2)")
#changepoint'y
segmentyBINSEG<-cpts(BinSeg)

ks.test(wygladzone,y='pnorm',alternative='two.sided')

# 3.2. IFULTOOLS PACKAGE --------------------------------------------------

#funkcja linearSegmentation pozwala na segment szeregu czasowego na segmenty o 
#wybranej szerokosci, nastepnie laczy te, gdzie kat pomiedzy segmentami jest 
#mniejszy niz angle.tolerance

plotLinearSegmentation<-function(x,data,p,q,aspect=T){
  segmenty<-as.numeric(linearSegmentation(x,data,n.fit=p,angle.tolerance=q,
                                          aspect=T))
  segmenty[length(segmenty)+1]=length(data)
  segmenty1<-c(rep(NA,length(segmenty)+1))
  segmenty1[2:length(segmenty1)]<-segmenty
  segmenty1[1]=1
  segmenty<-segmenty1
  rm(segmenty1)
  wartosci<-data[segmenty]
  plot(segmenty,wartosci,type="l",xlab="czas",ylab="wartosc",
       main=paste("linearSegmentation (", p ,",", q , ") \n",
                  length(segmenty)," segmenty",sep=""))
  segmentyLS<<-segmenty
}
x<-c(1:5235)
plotLinearSegmentation(x,wygladzone,5,0.1)

# 3.3. WLASNA FUNKCJA SEGMENTACJI -----------------------------------------


#laczy segmenty o podobnych trendach dla szeregu, ktory juz zostal wczesniej 
#podzielony
MergeSimilarTrends<-function(szereg,changepoints,threshold){ 
  changepoints[length(changepoints)+1]<-length(szereg)
  changepoints1<-c(rep(NA,length(changepoints)+1))
  changepoints1[2:length(changepoints1)]<-changepoints
  changepoints1[1]<-1
  changepoints<-changepoints1
  
  df<-data.frame(changepoint=changepoints,close=szereg[changepoints])
  
  trend<-rep(NA,length(changepoints))
  trend[1]=((df[1,2]-szereg[1])/szereg[1])*100
  for (i in 2:length(changepoints)){
    trend[i]=(((df[i,2]-df[i-1,2])/df[i-1,2])*100)
  }
  df<-data.frame(df,trend)
  
  similar<-rep(NA,length(changepoints))
  similar[1]='F'
  for (i in 2:length(changepoints)){
    ifelse(abs(df[i,3]-df[i-1,3])<=threshold,similar[i]<-'T',similar[i]<-'F')
  }
  df<-data.frame(df,similar)
  df$similar<-as.character(similar)
  
  rows_to_delete<-which(grepl('T',df$similar))-1
  df1<-df[-c(rows_to_delete),-c(3,4)]
  
  trend1<-rep(NA,length(df1[,1]))
  trend1[1]=((df1[1,2]-szereg[1])/szereg[1])*100
  for (i in 2:length(df1[,1])){
    trend1[i]=(((df1[i,2]-df1[i-1,2])/df1[i-1,2])*100)
  }
  df1<-data.frame(df1,Trend=trend1)
  
  DF<<-df1
  segmenty<<-df1$changepoint
}

#porownanie graficzne zastosowania Binary Segmentation dla roznych parametrow 
#z pierwotnym wyrownanym szeregiem
par(mfrow=c(2,2))
#x<-c(1:5523)
plot(x,wygladzone,type="l")
MergeSimilarTrends(wygladzone,segmenty,0.5)
segmentyLSost<-segmenty
plot(DF[,1],DF[,2],type="l",xlab="czas",
     ylab="wartosc",main=paste("MST-LinearSegmentation, 0.5 \n",
                               length(segmenty)," segmenty",sep=""))
DF_LS<-DF

MergeSimilarTrends(wygladzone,segmenty,0.5)
segmentyBINSEGost<-segmenty
plot(DF[,1],DF[,2],type="l",xlab="czas",
     ylab="wartosc",main=paste("MST-BinSeg, 0.5 \n",length(segmenty),
                               " segmenty",sep=""))
DF_BINSEG<-DF

rm(DF,segmenty,segmentyBINSEG,segmentyLS,x)
#Z FUNKCJI MergeSimilarTrends DOSTAJEMY DATA FRAME- DF (zawierajaca szczegolowe 
#dane, otrzymane po merge'u segmentow o podobnych trendach otrzymanych z funkcji 
#cpt.Mean) oraz osobny wektor 'segmenty', zawierajacy nowe changepoint'y

# 4. SYMBOLICZNA REPREZENTACJA SEGMENTOW ----------------------------------

#DODANIE CZASU TRWANIA
DF<-DF_LS
DF<-DF_BINSEG

Duration<-c(rep(NA,length(DF[,1])))
Duration[1]=0
for (i in 2:length(DF[,1])){
  Duration[i]<-DF[i,1]-DF[i-1,1] 
}
DF<-data.frame(DF,Duration)
#Statystyki czasu trwania
summary(DF[,4])
hist(DF[,4],xlim=c(0,30),freq=T,breaks=500,density=50,col="black")

#Statystyki trendu
summary(DF[,3])
hist(DF[,3],xlim=c(-12,20),freq=T,breaks=100,density=50,col="black")

#funkcja BinsTrend do wyznaczania ile procentowo znajduje sie segmentow w 
#zadanych przedzialach (x1-x4)
BinsTrend<-function(data,kolumna,x1,x2,x3,x4){
  range1<-((nrow(data[which(data[,kolumna] <= x1),])) / 
             length(data[,kolumna]) * 100)
  range2<-(nrow(data[which(data[,kolumna] > x1 & data[,kolumna] <= x2) ,])/
             length(data[,kolumna]) * 100)
  range3<-(nrow(data[which(data[,kolumna] > x2 & data[,kolumna] <= x3) ,])/
             length(data[,kolumna]) * 100)
  range4<-(nrow(data[which(data[,kolumna] > x3 & data[,kolumna] <= x4) ,])/
             length(data[,kolumna]) * 100)
  range5<-((nrow(data[which(data[,kolumna] > x4),])) /
             length(data[,kolumna]) * 100)
  w<-c(range1,range2,range3,range4,range5)
  return(w)
}
#dwie propozycje dobrania przedzialow trendu
BinsTrend(DF_BINSEG,3,-4,-1,1,5)
BinsTrend(DF,3,-5,-2,2,5)

#funkcja BinsTime analogiczna to poprzedniej, tylko ze dla przedzialow czasowych
BinsTime<-function(data,kolumna,x1,x2){
  range1<-((nrow(data[which(data[,kolumna] <= x1),])) / 
             length(data[,kolumna]) * 100)
  range2<-(nrow(data[which(data[,kolumna] > x1 & data[,kolumna] <= x2) ,])/ 
             length(data[,kolumna]) * 100)
  range3<-((nrow(data[which(data[,kolumna] > x2),])) / 
             length(data[,kolumna]) * 100)
  w<-c(range1,range2,range3)
  return(w)
}
#dwie propozycje dobrania przedzialow czasu
BinsTime(DF_BINSEG,4,5,20)
BinsTime(DF,4,5,10)

DF_BINSEG<-DF
DF_LS<-DF
rm(DF)

# 4.1. Klastrowanie BINSEG ------------------------------------------------

df1<-data.frame(DF_BINSEG$Duration,DF_BINSEG$Trend)
kluster<-kmeans(df1,9)

plot(df1,pch=16,cex=0.5,main="Klastrowanie - Binary Segmentation",
     xlab="Czas trwania",ylab="Trend")
kmeansRes<-factor(kluster$cluster)
s.class(df1,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))

DF_BINSEG$cluster <- kluster$cluster
for(i in seq_along(DF_BINSEG[,1])){
  DF_BINSEG[i,5]<-paste("kl_",DF_BINSEG[i,5],sep="")
}
#data frame odpowiedni dla algorytmu apriori
DF_BINSEG[,5]<-as.factor(DF_BINSEG[,5])
nr_segmentu<-c(1:length(DF_BINSEG[,1]))
DF_BINSEG1<-data.frame(nr_segmentu,klasa=DF_BINSEG[,5])

#PODZIAl NA ZBIOR TRENINGOWY I TESTOWY

podzial<-function(dane,procent){
  zmiana<-round(procent*nrow(dane))
  DF_BINSEG_trening<<-dane[1:zmiana,]
  DF_BINSEG_test<<-dane[(zmiana+1):nrow(dane),]
  
}
podzial(DF_BINSEG1,0.8)

# 4.2. Klastrowanie LS ----------------------------------------------------

df1<-data.frame(DF_LS$Duration,DF_LS$Trend)
kluster<-kmeans(df1,7)

plot(df1,pch=16,cex=0.5,main="Klastrowanie - Linear Segmentation",
     xlab="Czas trwania",ylab="Trend",
     ylim=c(-20,40), xlim=c(0,50))
kmeansRes<-factor(kluster$cluster)
s.class(df1,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))

DF_LS$cluster <- kluster$cluster
for(i in seq_along(DF_LS[,1])){
  DF_LS[i,5]<-paste("kl_",DF_LS[i,5],sep="")
}
#data frame odpowiedni dla algorytmu apriori
DF_LS[,5]<-as.factor(DF_LS[,5])
nr_segmentu<-c(1:length(DF_LS[,1]))
DF_LS1<-data.frame(nr_segmentu,klasa=DF_LS[,5])

#PODZIAl NA ZBIOR TRENINGOWY I TESTOWY

podzial<-function(dane,procent){
  zmiana<-round(procent*nrow(dane))
  DF_LS_trening<<-dane[1:zmiana,]
  DF_LS_test<<-dane[(zmiana+1):nrow(dane),]
  
}
podzial(DF_LS1,0.66)

# 5. WYZNACZANIE REGUL ASOCJACYJNYCH --------------------------------------

z <- read.zoo(DF_LS_trening, header = TRUE, FUN = identity)
lags <- as.data.frame(lag(z[,2], -4:0))
lags[,1]<-as.factor(lags[,1])
lags[,2]<-as.factor(lags[,2])
lags[,3]<-as.factor(lags[,3])
lags[,4]<-as.factor(lags[,4])
lags[,5]<-as.factor(lags[,5])
lags[,6]<-as.factor(lags[,6])

pojawienie<-5
a <- apriori(lags,parameter = list( supp=pojawienie/nrow(lags), 
                                    conf=0.5,minlen=3))
c<-subset(a, subset = rhs %pin% "lag0=")
inspect(c)

reguly_BINSEG<-as(c, "data.frame")
reguly_LS<-as(c,"data.frame")

# 6. OCENA ALGORYTMU ------------------------------------------------------

#dla regul LS
ocena_regul<-function(dane,lhs,e1,e2,e3,e4,e5){
  if (lhs==2){
    z <- read.zoo(dane, header = TRUE, FUN = identity)
    lags <- as.data.frame(lag(z[,2], -lhs:0))
    lags[,1]<-as.factor(lags[,1])
    lags[,2]<-as.factor(lags[,2])
    lags[,3]<-as.factor(lags[,3])
    
    lags_all<-lags[lags[,1]==paste("kl_",e3,sep="") & 
                     lags[,2]==paste("kl_",e4,sep=""),]
    lags_all<-na.omit(lags_all)
    zliczenie<-nrow(lags_all)
    poprawne<-nrow(lags_all[lags_all[,3]==paste("kl_",e5,sep=""),])
    wspolczynnik<-poprawne/zliczenie
    wynik<-data.frame(poprawne,zliczenie,wspolczynnik)
    return(wynik)
    
  } else if (lhs==3) {
    
    z <- read.zoo(dane, header = TRUE, FUN = identity)
    lags <- as.data.frame(lag(z[,2], -lhs:0))
    lags[,1]<-as.factor(lags[,1])
    lags[,2]<-as.factor(lags[,2])
    lags[,3]<-as.factor(lags[,3])
    lags[,4]<-as.factor(lags[,4])
    
    lags_all<-lags[lags[,1]==paste("kl_",e2,sep="") & 
                     lags[,2]==paste("kl_",e3,sep="") &
                     lags[,3]==paste("kl_",e4,sep=""),]
    lags_all<-na.omit(lags_all)
    zliczenie<-nrow(lags_all)
    poprawne<-nrow(lags_all[lags_all[,4]==paste("kl_",e5,sep=""),])
    wspolczynnik<-poprawne/zliczenie
    wynik<-data.frame(poprawne,zliczenie,wspolczynnik)
    return(wynik)    
    
  } else if (lhs==4){
    
    z <- read.zoo(dane, header = TRUE, FUN = identity)
    lags <- as.data.frame(lag(z[,2], -lhs:0))
    lags[,1]<-as.factor(lags[,1])
    lags[,2]<-as.factor(lags[,2])
    lags[,3]<-as.factor(lags[,3])
    lags[,4]<-as.factor(lags[,4])
    lags[,5]<-as.factor(lags[,5])
    
    lags_all<-lags[lags[,1]==paste("kl_",e1,sep="") &
                     lags[,2]==paste("kl_",e2,sep="") & 
                     lags[,3]==paste("kl_",e3,sep="") &
                     lags[,4]==paste("kl_",e4,sep=""),]
    lags_all<-na.omit(lags_all)
    zliczenie<-nrow(lags_all)
    poprawne<-nrow(lags_all[lags_all[,5]==paste("kl_",e5,sep=""),])
    wspolczynnik<-poprawne/zliczenie
    wynik<<-data.frame(poprawne,zliczenie,wspolczynnik)
    return(wynik) 
    
  }
}

reg<-matrix(c(0,0,5,7,7,
              0,0,2,3,2,
              0,0,2,2,2,
              0,0,2,7,7,
              0,3,7,7,7,
              0,1,1,7,7,
              0,7,7,1,7,
              0,2,2,2,2,
              0,7,2,2,2,
              0,2,2,7,7,
              0,7,2,7,7,
              2,2,2,2,2,
              2,2,2,7,7,
              2,2,7,7,7,
              7,2,2,2,2,
              7,7,2,2,2,
              7,2,7,7,7,
              7,7,2,7,7,
              7,7,7,7,7),ncol=5,byrow=T)


Ocena_LS<-matrix(rep(NA,57),nrow=19)
for (i in 1:nrow(reg)){
  wynik<-ocena_regul(DF_LS_test,5-sum(reg[i,]==0)-1,reg[i,1],reg[i,2],
                     reg[i,3],reg[i,4],reg[i,5])
  Ocena_LS[i,1]<-wynik[1,1]
  Ocena_LS[i,2]<-wynik[1,2]
  Ocena_LS[i,3]<-wynik[1,3]
}
colnames(Ocena_LS)<-c("Prawidlowo zaklasyfikowane","laczna liczba pojawien",
                      "Wspolczynnik dokladnosci")

#dla regul BINSEG

reg<-matrix(c(0,0,2,6,6, 
              0,0,4,1,7, 
              0,0,4,8,4, 
              0,0,8,4,4, 
              0,0,9,6,5, 
              0,0,5,5,4, 
              0,0,5,4,4, 
              0,0,4,6,6, 
              0,0,4,4,4, 
              0,4,8,4,4, 
              0,9,6,5,4, 
              0,6,9,6,6, 
              0,5,5,4,4, 
              0,5,4,4,4, 
              0,6,5,5,4, 
              0,4,5,6,6, 
              0,4,5,4,4, 
              0,4,4,6,6, 
              0,4,4,4,4),ncol=5,byrow=T)


Ocena_BINSEG<-matrix(rep(NA,57),nrow=19)
for (i in 1:nrow(reg)){
  wynik<-ocena_regul(DF_BINSEG_test,5-sum(reg[i,]==0)-1,reg[i,1],reg[i,2],
                     reg[i,3],reg[i,4],reg[i,5])
  Ocena_BINSEG[i,1]<-wynik[1,1]
  Ocena_BINSEG[i,2]<-wynik[1,2]
  Ocena_BINSEG[i,3]<-wynik[1,3]
}
colnames(Ocena_BINSEG)<-c("Prawidlowo zaklasyfikowane","laczna liczba pojawien"
                          ,"Wspolczynnik dokladnosci")