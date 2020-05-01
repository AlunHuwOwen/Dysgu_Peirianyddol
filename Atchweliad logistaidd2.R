install.packages("stats")
library(stats)
data <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/data_logistic.csv")
View(data)
str(data)
rhifau <- c(1:1000)
rhifauhyfforddi <- sample(x = rhifau, size = 700, replace = FALSE)
hyfforddi <- data[rhifauhyfforddi,] 
rhifauprofi <- setdiff(rhifau, rhifauhyfforddi)
profi <- data[rhifauprofi,]
atchweliad <- glm(Clefyd_Siwgr ~  Pwysau, family = binomial, data = hyfforddi)
atchweliad$coefficients
canlyniad <- round(predict(object = atchweliad, newdata = profi, type = "response"), digits = 0)
zzz <- round(predict(object = atchweliad, newdata = ymarfer, type = "response"), digits = 0)
canlyniad
head(test)
canlyniad
profi

1-(sum((profi[,6]-unname(canlyniad))**2)/length(profi[,6]))
1-(sum((ymarfer[,6]-unname(zzz))**2)/length(hyffordi[,6]))
str(atchweliad)
unname(round(predict(object = atchweliad, newdata = data.frame( Taldra = 160, Pwysau = 92, MaintGwasg = 34, Oed = 20, Rhyw = "Benyw"), type = "response"), digits = 0))
plot(atchweliad)
plot(x = x, y = 1/(1 + exp(unname(atchweliad$coefficients[1]+unname(atchweliad$coefficients[2] * x)))), col = "white", xlab = "Pwysau", ylab = "Tebygolrwydd fod person gyda diabetes")
x <- 50:150
lines(x = x, y = abs( 1/(1 + exp((unname(atchweliad$coefficients[1]+unname(atchweliad$coefficients[2] * x))))) - 1) )                            
points(x = x1, y = y1, col = "red")
points(x = 121, y = 1, col ="red")
predict(object = atchweliad, newdata = test, type = "response")
atchweliad2 <- glm(Diabetes ~ Pwysau, family = binomial, data = ymarfer)
canlyniad2 <- round(predict(object = atchweliad, newdata = profi, type = "response"), digits = 0)
profi[,2]
data[,2]
x1 <- c()
x1 <- c(data[,2])
x1 <- c(x1, rnorm(n = 50, mean = 130, sd = 6))
x1 <- c(x1, rnorm(n = 50, mean = 100, sd = 8))
x1 <- c(x1, rnorm(n=50, mean = 70, sd =6))

y1 <- c() 
y1 <- c(data[,6])
y1 <- c(y1, rbinom(n = 50,size = 1, prob = 0.99 ))
y1 <- c(y1, rbinom(n = 50,size = 1, prob = 0.5 ))
y1 <- c(y1, rbinom(n = 50,size = 1, prob = 0.01 ))
points(x1,y1)
plot(x1,y1)
abline(lm(formula = y1 ~ x1 ))
abline(v=seq(from = 50, to = 150, by = 10), col = "green" )
lines(50:60, rep(un,11), col = "green")
lines(60:70, rep(dau,11), col = "green")
lines(700:60, rep(un,11), col = "green")
polygon(x = c(50,50,60,60), y = c(un,0,0,un), col = "green")
polygon(x = c(60,60,70,70), y = c(dau,0,0,dau), col = "green")
polygon(x = c(70,70,80,80), y = c(tri,0,0,tri), col = "green")
polygon(x = c(80,80,90,90), y = c(pedwar,0,0,pedwar), col = "green")
polygon(x = c(90,90,100,100), y = c(pump,0,0,pump), col = "green")
polygon(x = c(100,100,110,110), y = c(chwe,0,0,chwe), col = "green")
polygon(x = c(110,110,120,120), y = c(saith,0,0,saith), col = "green")
polygon(x = c(120,120,130,130), y = c(wyth,0,0,wyth), col = "green")


text(x = 60, y = 0.5, labels = "0.07", col = "blue")
text(x = 80, y = 0.5, labels = "0.33", col = "blue")
text(x = 100, y = 0.5, labels = "0.70", col = "blue")
text(x = 120, y = 0.5, labels = "1", col = "blue")
text(x = 140, y = 0.5, labels = "1", col = "blue")
barplot(height = c(un,dau,tri,pedwar,pump,chwe,saith,wyth),space=0)

group5060 <- which(x1<=60 & x1 >= 50)
loggroup5060 <- y1[group5060]
un <- sum(loggroup5060)/length(loggroup5060)

group6070 <- which(x1<=70 & x1 >= 60)
loggroup6070 <- y1[group6070]
dau <- sum(loggroup6070)/length(loggroup6070)

group7080 <- which(x1<=80 & x1 >= 70)
loggroup7080 <- y1[group7080]
tri <- sum(loggroup7080)/length(loggroup7080)

group8090 <- which(x1<=90 & x1 >= 80)
loggroup8090 <- y1[group8090]
pedwar <- sum(loggroup8090)/length(loggroup8090)

group90100 <- which(x1<=100 & x1 >= 90)
loggroup90100 <- y1[group90100]
pump <- sum(loggroup90100)/length(loggroup90100)

group100110 <- which(x1<=110 & x1 >= 100)
loggroup100110 <- y1[group100110]
chwe <- sum(loggroup100110)/length(loggroup100110)

group110120 <- which(x1<=120 & x1 >= 110)
loggroup110120 <- y1[group110120]
saith <- sum(loggroup110120)/length(loggroup110120)

group120130 <- which(x1<=130 & x1 >= 120)
loggroup120130 <- y1[group120130]
wyth <- sum(loggroup120130)/length(loggroup120130)
wyth <-1

group130140 <- which(x1<=140 & x1 >= 130)
loggroup130140 <- y1[group130140]
naw <- sum(loggroup130140)/length(loggroup130140)
naw
plot(glm(x1~y1))

which.max(y1[x])
y1 <- y1[-2]
which(x1>120)
length(y1)
x1 <- x1[-which(x1>120)]
y1 <- y1[-which(x1>120)]
y1
