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

sapply(1:round(liczba_akcji/100),scrapuj_nazwy_akcji)
