library(sqldf)
library(RSQLite)


db <- dbConnect(SQLite(), dbname = "notowania_gpw_2000.sqlite")
s2000 <- dbGetQuery(db, "select * from notowania_2000")
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "notowania_gpw_2005.sqlite")
s2005 <- dbGetQuery(db, "select * from notowania_2005")
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "notowania_gpw_2010.sqlite")
s2010 <- dbGetQuery(db, "select * from notowania_2010")
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "notowania_gpw_2015.sqlite")
s2015 <- dbGetQuery(db, "select * from notowania_2015")
dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "notowania_gpw_2020.sqlite")
s2020 <- dbGetQuery(db, "select * from notowania_2020")
dbDisconnect(db)


dane <- rbind(s2000, s2005, s2010, s2015, s2020)
rm(s2000, s2005, s2010, s2015, s2020)

#zapis do bazy danych SQL
db <- dbConnect(SQLite(), dbname="notowania_gpw.sqlite")
dbSendQuery(db, "create table notowania
            (data date,
            nazwa varchar,
            ISIN varchar,
            waluta varchar,
            otwarcie decimal(20,5),
            maksimum decimal(20,5),
            minimum decimal(20,5),
            zamkniecie decimal(20,5),
            zmiana_kursu decimal(20,5),
            wolumen decimal(20,5),
            transakcje decimal(20,5),
            wartosc_obrotu decimal(20,5))",overwrite=T)

dbWriteTable(db, "notowania", dane, overwrite=T)
dbDisconnect(db)
