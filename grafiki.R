options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

library(randomForest)
library(reprtree)
library(dplyr)

titanic <- titanic::titanic_train
colnames(titanic) <- c("ID", "Przeżycie", "Klasa", "Nazwisko", "Płeć", "Wiek", 
                       "Rodzeństwo", "Rodzice", "Ticket", "Opłata", "Kabina", "Port")
titanic %>% 
  select(Przeżycie, Klasa, Płeć, Wiek, Rodzeństwo, Rodzice, Opłata, Port) -> titanic

titanic$Przeżycie <- as.factor(titanic$Przeżycie)
titanic$Klasa <- as.factor(titanic$Klasa)
titanic$Płeć <- as.factor(titanic$Płeć)
titanic$Port <- as.factor(titanic$Port)

model <- randomForest(Przeżycie ~ ., data=titanic, importance=TRUE, ntree=500, mtry = 2, do.trace=100, maxnodes = 7)


par(mfrow=c(3,2),
  oma = c(0,0,0,0) + 0.1,
  mar = c(0,0,0,0) + 0.1)
reprtree:::plot.getTree(model, k = 12)
reprtree:::plot.getTree(model, k = 243)
reprtree:::plot.getTree(model, k = 36)
reprtree:::plot.getTree(model, k = 453)
reprtree:::plot.getTree(model, k = 399)
reprtree:::plot.getTree(model, k = 136)


library(ggplot2)

x1 <- c(2, 2, 2, 3, 4, 6, 6, 6, 7, 7)
x2 <- c(1, 6, 8, 3, 5, 1, 3, 5, 4, 7)
x3 <- as.factor(c(1, 1, 2, 1, 1, 1, 2, 2, 2, 2))
dane <- data.frame(x1, x2, x3)

ggplot(dane, aes(x = x1, y = x2, shape = x3, color = x3))+
  geom_point(size = 8) +
  geom_hline(yintercept = 6.5, size = 1.5) +
  theme_classic() +
  xlim(c(1, 8))+
  ylim(c(1, 8))+
  theme(legend.position="none")

ggplot(dane, aes(x = x1, y = x2, shape = x3, color = x3))+
  geom_point(size = 8) +
  geom_hline(yintercept = 1.5, size = 1, linetype = "dashed", color = "green") +
  theme_classic() +
  xlim(c(1, 8))+
  ylim(c(1, 8))+
  theme(legend.position="none")

ggplot(dane, aes(x = x1, y = x2, shape = x3, color = x3))+
  geom_point(size = 8) +
  geom_vline(xintercept = 4.5, size = 1, linetype = "12345678", color = "red") +
  theme_classic() +
  xlim(c(1, 8))+
  ylim(c(1, 8))+
  theme(legend.position="none")


ggplot(dane, aes(x = x1, y = x2, shape = x3, color = x3))+
  geom_point(size = 8) +
  theme_classic() +
  geom_segment(aes(x = 0, xend = 4.5, y = 6.5,  yend = 6.5), size = 1, color = "purple", linetype = "F1")+
  geom_segment(aes(x = 4.5, xend = 8, y = 1.5,  yend = 1.5), size = 1, color = "purple", linetype = "F1")+
  geom_segment(aes(x = 4.5, xend = 4.5, y = 1.5,  yend = 6.5), size = 1, color = "purple", linetype = "F1")+
  theme(legend.position="none")



par(mfrow=c(1,1))

q <- seq(from=0, to=20, length.out = 150)
y <- 500 + 0.4 * (q-10)^3

set.seed(56283)
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise

dane <- data.frame(x = q, y = noisy.y)

ggplot(dane, aes(x, y))+
  geom_point()+
  geom_smooth(method = "lm", se = F, color = "red")+
  stat_smooth(aes(x, y), method = "lm",
              formula = y ~ poly(x, 25), se = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")







