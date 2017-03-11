library(rvest)

web<-read_html("https://stooq.pl/t/?i=513&v=0&l=1")

#scrapowanie liczby dostepnych akcji
web %>%
  html_node("td#f13") %>% 
  html_text() %>%
  regmatches(.,regexpr("(?<=z )(.*)(?= \\| Pierwsza)",.,perl=T)) %>%
  as.numeric() -> liczba_akcji


scrapuj_nazwy_akcji<-function(strona){
  web<-read_html(paste("https://stooq.pl/t/?i=513&v=0&l=",strona,sep=""))
  
  web %>%
    html_node("#fth1") %>% 
    html_table() -> df
  return(as.character(df[,1]))
}

sapply(1:round(liczba_akcji/100, digits=0),scrapuj_nazwy_akcji) %>%
  do.call(c,.) -> akcje

pobierz_csv<-function(nazwa){
  download.file(paste("https://stooq.pl/q/d/l/?s=",nazwa,"&i=d",sep=""),
                paste("C:/Users/mcisek001/Documents/magisterka/dane/",nazwa,".csv",sep=""))
}
library(pbapply)
pbsapply(akcje,pobierz_csv)

strona<-1
nr="999"
ramka_ceny <- data.frame(Nr.=character(),
                 Data=character(), 
                 Otwarcie=character(),
                 Najwyższy=character(),
                 Najniższy=character(),
                 Zamknięcie=character(),
                 Wolumen=character(),
                 stringsAsFactors=FALSE) 

while(nr != "1"){
  web<-read_html(paste("https://stooq.pl/q/d/?s=06n&i=d&l=",strona,sep=""))
  web %>%
    html_nodes("#fth1 td") %>% 
    html_text() -> ramka_ceny1
  
  if(length(grep("Split",ramka_ceny1))>0) ramka_ceny1<-ramka_ceny1[-c(grep("Split",ramka_ceny1):(grep("Split",ramka_ceny1)-2))]
  
  ramka_ceny1 %>%  
    matrix(.,ncol=length(.)/7) %>%
    t %>%
    data.frame(.,stringsAsFactors = F) -> ramka_ceny1
  
  colnames(ramka_ceny1)<-ramka_ceny1[1,]
  ramka_ceny1<-ramka_ceny1[-1,]
  ramka_ceny<-rbind(ramka_ceny,ramka_ceny1)
  
  nr<-ramka_ceny1[nrow(ramka_ceny1),1]
  strona<-strona+1
}

