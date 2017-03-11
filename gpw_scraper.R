rm(list=ls())
library(rvest)
library(pbapply)

dni <- seq(as.Date("1991-04-16"),as.Date("2017-03-10"),by="days")
dni <- dni[-which(weekdays(dni) %in% c("sobota","niedziela"))]


pobierz_akcje<-function(date){
  
  web<-read_html(paste("https://www.gpw.pl/notowania_archiwalne_full?type=10&date=",date, sep=""))
  
  web %>%
    html_nodes("td , .left, th") %>% 
    html_text()  %>% 
    as.vector() %>%
    matrix(., nrow=11, ncol=length(.)/11) %>%
    t() %>%
    data.frame(.,stringsAsFactors=F) -> df
  
  colnames(df)<-df[1,]
  df<-df[-1,]
  return(df)
}
akcje<-pblapply(dni[6000:6759],pobierz_akcje)

pobierz_akcje("2016-12-26")

akcje<-vector("list",6759)
for(i in 6327:6759){
  akcje[[i]]<-pobierz_akcje(dni[i])
  print(i)
  flush.console()
}
##od 6500


