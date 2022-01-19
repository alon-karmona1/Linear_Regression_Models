library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
library(rmarkdown)
library(simmer)
library(simmer.plot)

summary(X28)
cov(X28)
plot(X28)

## remove because of correlation between explanerscor.test(x= X28$`NBA……_YEARS`, y=X28$AGE)
plot(x= X28$AGE, y=X28$`NBA……_YEARS`, xlab = "AGE", ylab = "nba years")

## Check the significance of each variable
fit = lm(formula = X28$SALARY_MILLIONS~X28$AGE+X28$AVG_MINUTES_PLAYED+X28$PERCENT_WIN_GAME_PLAY+X28$PTS+X28$TWITTER_FOLLOWERS+X28$HEIGHT+X28$DRAFT+X28$`NBA……_YEARS`, data = X28)
summary(fit) 


## Everyone do them a pirson test takes out a number from minus 1
cor.test(x= X28$AGE, y=X28$SALARY_MILLIONS)
cor.test(x= X28$AVG_MINUTES_PLAYED, y=X28$SALARY_MILLIONS)
cor.test(x= X28$PERCENT_WIN_GAME_PLAY, y=X28$SALARY_MILLIONS)
cor.test(x= X28$PTS, y=X28$SALARY_MILLIONS)
cor.test(x= X28$TWITTER_FOLLOWERS, y=X28$SALARY_MILLIONS)
cor.test(x= X28$HEIGHT, y=X28$SALARY_MILLIONS)
cor.test(x= X28$POSITION, y=X28$SALARY_MILLIONS)
cor.test(x= X28$DRAFT, y=X28$SALARY_MILLIONS)
cor.test(x= X28$`NBA_YEARS`, y=X28$SALARY_MILLIONS)
cor.test(x= X28$AFRO_AMERICAN, y=X28$SALARY_MILLIONS)

# removed age height and skin-color

## A chart of each variable to see if needs to remove

plot(x= X28$AGE, y=X28$SALARY_MILLIONS, xlab = "AGE", ylab = "player salary ")
plot(x= X28$AVG_MINUTES_PLAYED, y=X28$SALARY_MILLIONS, xlab = "AVG_MINUTES_PLAYED", ylab = "player salary ")
plot(x= X28$PERCENT_WIN_GAME_PLAY, y=X28$SALARY_MILLIONS, xlab = "PERCENT_WIN_GAME_PLAY", ylab = "player salary ")
plot(x= X28$PTS, y=X28$SALARY_MILLIONS, xlab = "PTS", ylab = "player salary ")
plot(x= X28$TWITTER_FOLLOWERS, y=X28$SALARY_MILLIONS, xlab = "TWITTER_FOLLOWERS", ylab = "player salary ")
plot(x= X28$HEIGHT, y=X28$SALARY_MILLIONS, xlab = "HEIGHT", ylab = "player salary ")
plot(x= X28$POSITION, y=X28$SALARY_MILLIONS, xlab = "POSITION", ylab = "player salary ")
plot(x= X28$DRAFT, y=X28$SALARY_MILLIONS, xlab = "DRAFT", ylab = "player salary ")
plot(x= X28$`NBA……_YEARS`, y=X28$SALARY_MILLIONS, xlab = "NBA_YEARS", ylab = "player salary ")
plot(x= X28$AFRO_AMERICAN, y=X28$SALARY_MILLIONS, xlab = "AFRO_AMERICAN", ylab = "player salary ")


## A scatter plot of each variable explains to see if needs to remove

fit = lm(formula = X28$SALARY_MILLIONS~X28$`NBA……_YEARS`, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$AGE, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$AVG_MINUTES_PLAYED, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$PERCENT_WIN_GAME_PLAY, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$PTS, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$TWITTER_FOLLOWERS, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$HEIGHT, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$POSITION, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$DRAFT, data = X28)
summary(fit)
fit = lm(formula = X28$SALARY_MILLIONS~X28$AFRO_AMERICAN, data = X28)
summary(fit)

# Examining the transformation of continuous variables into categorical variables

hist(X28$AVG_MINUTES_PLAYED, col = "blue", prob = TRUE,  main = "histogram for AVG_MINUTES_PLAYED")
lines(density(X28$AVG_MINUTES_PLAYED),lwd = 2)

hist(X28$PERCENT_WIN_GAME_PLAY, col = "blue",prob = TRUE, main = "histogram for PERCENT_WIN_GAME_PLAY")
lines(density(X28$PERCENT_WIN_GAME_PLAY),lwd = 2)

hist(X28$PTS, col = "blue", prob = TRUE, main = "histogram for PTS")
lines(density(X28$PTS),lwd = 2)

hist(X28$TWITTER_FOLLOWERS, col = "blue", prob = TRUE, breaks=15, main = "histogram for TWITTER_FOLLOWERS")
lines(density(X28$TWITTER_FOLLOWERS),lwd = 2)

hist(X28$`NBA……_YEARS`, col = "blue",   prob = TRUE, main = "histogram for N…BA_YEARS")
lines(density(X28$`NBA……_YEARS`),lwd = 2)


Ntype_Color <- ifelse((X28$TWITTER_FOLLOWERS<1),"red","blue")
plot(x=X28$AVG_MINUTES_PLAYED, y = X28$SALARY_MILLIONS, main = 'AVG_MINUTES_PLAYED with SALARY by TWITTER_FOLLOWERS', col = Ntype_Color)
abline(lm(X28$SALARY_MILLIONS~X28$AVG_MINUTES_PLAYED, subset = Ntype_Color == 'red'), col = "red")
abline(lm(X28$SALARY_MILLIONS~X28$AVG_MINUTES_PLAYED, subset = Ntype_Color == 'blue'), col = "blue")

Ntype_Color <- ifelse((X28$TWITTER_FOLLOWERS<1),"red","blue")
plot(x=X28$PERCENT_WIN_GAME_PLAY, y = X28$SALARY_MILLIONS, main = 'PERCENT_WIN_GAME_PLAY with SALARY by TWITTER_FOLLOWERS', col = Ntype_Color)
abline(lm(X28$SALARY_MILLIONS~X28$PERCENT_WIN_GAME_PLAY, subset = Ntype_Color == 'red'), col = "red")
abline(lm(X28$SALARY_MILLIONS~X28$PERCENT_WIN_GAME_PLAY, subset = Ntype_Color == 'blue'), col = "blue")

Ntype_Color <- ifelse((X28$TWITTER_FOLLOWERS<1),"red","blue")
plot(x=X28$PTS, y = X28$SALARY_MILLIONS, main = 'PTS with SALARY by TWITTER_FOLLOWERS', col = Ntype_Color)
abline(lm(X28$SALARY_MILLIONS~X28$PTS, subset = Ntype_Color == 'red'), col = "red", lwd = 5)
abline(lm(X28$SALARY_MILLIONS~X28$PTS, subset = Ntype_Color == 'blue'), col = "blue", lwd = 5)

Ntype_Color <- ifelse((X28$TWITTER_FOLLOWERS<1),"red","blue")
plot(x=X28$`NBA……_YEARS`, y = X28$SALARY_MILLIONS, main = 'NBA YEARS with SALARY by TWITTER_FOLLOWERS', col = Ntype_Color)
abline(lm(X28$SALARY_MILLIONS~X28$`NBA……_YEARS`, subset = Ntype_Color == 'red'), col = "red")
abline(lm(X28$SALARY_MILLIONS~X28$`NBA……_YEARS`, subset = Ntype_Color == 'blue'), col = "blue")


## Replace the database table with the new columns of the categorical: position, Tweet and Draft


 X28C$Dra <- factor(X28C$Dra, labels=c("1-10","11-30", "31-60"))
 X28C$Pos<-factor(X28C$Pos, labels=c("1-3","4-5"))
 X28C$Twi<-factor(X28C$Twi, labels=c("moremillion","lessmillion"))
 fit<-lm(X28C$SALARY_MILLIONS ~ X28C$AVG_MINUTES_PLAYED+X28C$PERCENT_WIN_GAME_PLAY+X28C$PTS+X28C$NBA_YEARS+X28C$Twi+X28C$Pos+X28C$Dra+X28C$Twi:X28C$NBA_YEARS+X28C$Pos:X28C$PERCENT_WIN_GAME_PLAY+X28C$Dra:X28C$NBA_YEARS+X28C$Dra:X28C$PERCENT_WIN_GAME_PLAY, data = X28C)
  summary(fit)
  
 Fm<-lm(X28C$SALARY_MILLIONS ~ X28C$AVG_MINUTES_PLAYED+X28C$PERCENT_WIN_GAME_PLAY+X28C$PTS+X28C$NBA_YEARS+X28C$Twi+X28C$Pos+X28C$Dra+X28C$Twi:X28C$NBA_YEARS+X28C$Pos:X28C$PERCENT_WIN_GAME_PLAY+X28C$Dra:X28C$NBA_YEARS+X28C$Dra:X28C$PERCENT_WIN_GAME_PLAY, data = X28C)
 Em <- lm(X28C$SALARY_MILLIONS~1)
 AIC(Fm)
 BIC(Fm)
 summary(Fm)
 
 ##backward process model
 
 backwardModelAIC <- step(Fm,direction = "backward", test = "F", scope = list(upper=Fm))
 summary(backwardModelAIC)

 
 backwardModelBIC <- step(Fm,direction = "backward", k= log(30), test = "F", scope = list(upper=Fm))
 summary(backwardModelBIC)

 
 
 ##forward process model

 forwardModelAIC <- step(Em,direction = "forward", test = "F", scope = list(upper=Fm))
 summary(forwardModelAIC)
 
 forwardModelBIC <- step(Em,direction = "forward", k= log(30), test = "F", scope = list(upper=Fm))
 summary(forwardModelBIC)
 
 ##both process model
bothModelAIC <- step(Fm,direction = "both", test = "F", scope = list(upper=Fm))
 summary(bothModelAIC)
 
 bothModelBIC <- step(Fm,direction = "both",k= log(30), test = "F", scope = list(upper=Fm))
 summary(bothModelBIC)
 
 predictedValue <- fitted(backwardModelAIC)
residuals<- residuals(backwardModelAIC) 
seResiduals <- sqrt(var(residuals))
standResiduals <- (residuals/seResiduals)

datasetVar <- as.data.frame(cbind(predictedValue,standResiduals))
plot(x=datasetVar$predictedValue, y= datasetVar$standResiduals,xlab = "predicted value" ,ylab = "normalized residuals",main = "NR VS PV")                   
abline(0,0)

## Normalization check
qqnorm(datasetVar$standResiduals)
abline(0,1)
hist(datasetVar$standResiduals,prob = T, main= "normalization residuals",xlab = "normalization residuals")
lines(density(datasetVar$standResiduals,na.rm = T))

## Shapiro KS test

ks.test(x=datasetVar$standResiduals,y="pnorm", alternative = "two.sided",exact = NULL)
shapiro.test(datasetVar$standResiduals)
v<- shapiro.test(datasetVar$standResiduals)
print(v[2])



## Experience for the T test
model<-cbind(1, backwardModelAIC$rate.of.positive.words.in.the.text,backwardModelAIC$headline.Subjectivity)

modelt<-t(model)

xtx<-as.matrix(modelt)%*%as.matrix(model)
inversextx<-ginv(xtx)
View(inversextx%*%xtx)
ruti<-c(1, 32.9, 0.58,29.1,2,0,0)
rutit<-t(ruti)

MSEfactor<-(1+rutit%%inversextx%%ruti)




x <- data[,c(1,2,3,4,5,6,7)]
x<- mutate (x,b0=1)
x<-x[c(7,1,2,3,4,5,6)]
xt<-t(x)
x<-as.matrix(x)
xt<-as.matrix(xt)
xtx <- (xt%*%x)
inversextx <- solve(xtx)
c<-as.matrix(c())
