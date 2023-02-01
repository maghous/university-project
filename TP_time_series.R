library(fpp3)
data(pelt)
autoplot(pelt,vars(Hare,Lynx),col="red")
moro<-mutate(pelt,Lynx=Lynx/1000)
autoplot(moro,Lynx,col="red")
#le nombre minumumde différenciations 
unitroot_ndiffs(pelt$Lynx)
acf(moro$Lynx)
pacf(moro$Lynx)
#On peut dire que c'esy modele AR(2)
forecast(moro$Lynx,h=10)
#PREVISION
library(datasets)
library(forecast)
data<-EuStockMarkets
plot.ts(data[,1],col=1)
plot.ts(data[,2],col=2)
plot.ts(data[,3],col=3)
plot.ts(data[,4],col=4)
X<-data[,"CAC"]
NewX<-window(X,start=1991,end=1998)
plot.ts(NewX,col=4)
#LISSAGE EXPONENTIELLE
xsmooth<-HoltWinters(NewX,alpha=0.1,beta=F,gamma=F)
#on varie alpha eto on affiche les prévisoon
#Pour alpha=0.1
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction pour alpha=0.1")
#Pour alpha=0.2
xsmooth<-HoltWinters(NewX,alpha=0.2,beta=F,gamma=F)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction pour alpha=0.1")
#Pour alpha=0.9
xsmooth<-HoltWinters(NewX,alpha=0.9,beta=F,gamma=F)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction
#On remarque que plus onn augmente alpha les graphes ça colle entre eux
#la méthode de lissage holt-winters non saisonière" 
xsmooth<-HoltWinters(NewX,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=10)
plot(xsmooth,xpred,main="prediction")
#on compare les différents prévisions obtenus
X[time(X)>=1998][1:10]
w=0
z=window(X,start=c(1998,1))
for (i in 1:169) {
  if(abs(z[i] - predict(xsmooth,n.ahead = 169)[i])<100){
    w=w+1
  }
  else(break)
}
##################################
##################################
arima.sim(105,model=list(ar=c(1,-0.5,1/3)))
ts.plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
acf(AirPassengers)
pacf(AirPassengers)
#on suppose qu'il suit un processus AR
AR<-arima(AirPassengers,order=c(1,0,0))
print(AR)
ts.plot(AirPassengers)
arfit<-AirPassengers - residuals(AR)
points(arfit,type="l",col=10,lty=2)
#prévision 
ts.plot(AirPassengers, xlim = c(1949, 1961))
AR_forecast <- predict(AR, n.ahead = 10)$pred
AR_forecast_se <- predict(AR, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
#on suppose qui'il suit un processus MA
MA <- arima(AirPassengers, order = c(0,0,1))
ts.plot(AirPassengers)
mafit<-AirPassengers - residuals(MA)
points(mafit,type="l",col=2,lty=2)
#prévision 
ts.plot(AirPassengers, xlim = c(1949, 1961))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)
print(c(AIC(MA),BIC(MA)))
print(c(AIC(AR),BIC(AR)))
#donc on en déduit que c'est un processus ar de de paramètre 2


################################################################
#################################################################

#TP 1 : Prévision

###################################################################
###################################################################

x=c(4,3,7,1.7,3,8,4,5,12,9,12)
is.ts(x)
y=as.ts(x)
y
time(y)
is.ts(y)
####
t=tsp(y)
t
plot.ts(y,col="red")
```
```{r}
plot.ts(y,col="red",type="b")
```
```{r}
z=ts(y,freq=4)
time(z)
plot.ts(z,col="red")
```
```{r}
frequency(z)
length(z)
m=tapply(z, cycle(z), mean)
m
```

```{r}
set.seed(123)
x=rnorm(100)
plot.ts(x,col="red")
```
```{r}
shapiro.test(x)
acf(x)
lag.plot(x)
```
#exercice 3 

```{r}
n=160
i=1:n
t=i/n
ep=rnorm(n)
X=1 + 5*t + 2*cos((2*pi*n*t)/8)+ep
plot.ts(X,col="red",main="plot(X)")
```

```{r}
#plot fonction d'autocorrÃ©lation
acf(X)
```

```{r}
Z=ts(X,frequency=12)
cc1=decompose(ts(X,frequency = 2),type = "additive")
plot(cc1)
```
```{r}
cc2=stl(ts(X,frequency = 2),s.window = 8)
plot(cc2)
```
```{r}
plot(cc1$trend)
```
```{r}
plot(cc2$time.series[,"trend"])
```
```{r}
plot(cc1$seasonal)

```

```{r}
plot(cc2$time.serie[,"seasonal"])
```
```{r}
hist(X,breaks = 20)
```


#filtrage d'une sÃ©rie temporelle :filtre des diffÃ©rences
```{r}
fil=diff(X,lag=1,differences =1)
plot(fil)
lag.plot(fil)
plot.ts(fil)
```
```{r}
acf(fil)
length(fil) #longeur de cette sÃ©rie 
```

```{r}
fil1=diff(X,lag=8,differences =1)
lag.plot(fil1)
plot.ts(fil1)
acf(fil1)
length(fil1)
#quand on augmente l'ordre d de diffÃ©renciation la langeur de la sÃ©rie baisse
```
```{r}
Y= 1 + 2*t + 10 *t^2 + 2*cos((2*pi*n*t)/8)+ep
fil2=diff(Y,lag=1,differences =1)
acf(fil2)
lag.plot(fil2)
plot.ts(fil2)
```

```{r}
fil3=diff(Y,lag=8,differences =1)
acf(fil3)
lag.plot(fil3)
plot.ts(fil3)
```
#filtres de  moyenne mobile
```{r}
y=filter(X,sides = 1, method = "convolution",filter = rep(1, 8)/8)
y1=filter(X, method = "recursive",filter = rep(1, 8)/8)
acf(na.omit(y1))
acf(na.omit(y))

################################################################
#################################################################

#TP 2 : Prévision

###################################################################
###################################################################

#Exercice 1 Test de blancheur

```{r cars}
epsn=rnorm(100)
r=acf(epsn)
#acf(epsn)
```

## part2
```{r}
yn=epsn[1:99] + epsn[2:100]
ryn=acf(yn)
rho=ryn$acf[2]
lag.plot(yn,2)
```
## part3
```{r}
epsu=runif(100,-sqrt(3),sqrt(3))
var=sd(epsu)**2
hist(epsu,breaks = 10,col="blue")
```

## part 4
```{r}
Box.test(epsn)
Box.test(yn)
```
```{r}
a1=Box.test(epsn,lag=1)
a2=Box.test(epsn,lag=2)
a3=Box.test(epsn,lag=3)
a4=Box.test(epsn,lag=4)
a5=Box.test(epsn,lag=5)

print(a1$p.value)
print(a2$p.value)
print(a3$p.value)
print(a4$p.value)
print(a5$p.value)
```
```{r}
a11=Box.test(epsn,lag=1,type='Ljung-Box')
a12=Box.test(epsn,lag=2,type='Ljung-Box')
a13=Box.test(epsn,lag=3,type='Ljung-Box')
a14=Box.test(epsn,lag=4,type='Ljung-Box')
a15=Box.test(epsn,lag=5,type='Ljung-Box')

print(a11$p.value)
print(a12$p.value)
print(a13$p.value)
print(a14$p.value)
print(a15$p.value)
```

```{r}
b1=Box.test(yn,lag=1)
b2=Box.test(yn,lag=2)
b3=Box.test(yn,lag=3)
b4=Box.test(yn,lag=4)
b5=Box.test(yn,lag=5)

print(b1$p.value)
print(b2$p.value)
print(b3$p.value)
print(b4$p.value)
print(b5$p.value)
```

```{r}
b11=Box.test(yn,lag=1,type='Ljung-Box')
b12=Box.test(yn,lag=2,type='Ljung-Box')
b13=Box.test(yn,lag=3,type='Ljung-Box')
b14=Box.test(yn,lag=4,type='Ljung-Box')
b15=Box.test(yn,lag=5,type='Ljung-Box')

print(b11$p.value)
print(b12$p.value)
print(b13$p.value)
print(b14$p.value)
print(b15$p.value)
```


#Exercice 2 simulation d'un processus MA(q)
```{r}
t=1:1000
epsilon=rnorm(1000,0,1)
X<-function(z){
  p={}
  for(i in length(z)){
    p[i+1]=p[i] + z[i] -1/3 *z[i-1]
  }
  p
}
```
#Exercice 4 simulation automatique 


```{r}
set.seed(219)
X1=arima.sim(n=200,list(ma=c(-.3,.6)),sd=1)
ARMAacf(ma=c(-.3,.6),lag.max=20)
ARMAacf(ar=c(.9),lag.max=20,pacf = T)
ARMAtoMA(ar=c(.9),ma=c(-.3,.6),lag.max = 20)




################################################################
#################################################################

#TP 3 : Analyse des séries temporelles.Vlidation d'un modèle ARMA

###################################################################
###################################################################
#Exercice 1

#QUESTION 1
library(datasets)
X=AirPassengers
summary(AirPassengers)
#Question2
plot.ts(X,col="red")
monthplot(X)
acf(X)
pacf(X)
boxplot(X~cycle(X),col="red")
#Question 3
lag.plot(X,4)
lag.plot(X,2)
#Question 4
#question a
t=time(X)
model=lm(X~t)
summary(model)

#question b 
ts.plot(X,col="red")
abline(reg=lm(X~time(X)))
#Supprimer la tendance et tracer la s???rie obtenue
B=X-model$coefficients[1]-model$coefficients[2]*t
plot.ts(B,col="red")
print(mean(B)) # presue nulle

#Question c
acf(B)
pacf(B)

#Question d

model=auto.arima(z)
plot.ts(model$residuals,col="red")
print(model$arma)

#Question E
eps<-model$residuals
Box.test(eps,lag=20) # donc c'est un bruit de moyenne 0 et variance 1

#Question 5

#Question a

Y<-diff(X,lag=T,differences = 1)
plot.ts(Y,col='red')

#Question b

acf(Y)
pacf(Y) # on peut dire que c'est un processus arma de paramètre 3 et 2
#Question c
model=auto.arima(Y)
print(model$arma)
print(model$coef)

#Question d 
eps<-model$residuals
Box.test(eps,lag=20) #c'est un bb


#Question 6

New_X<-decompose(X,type="additive")

```{r}
plot(new_X$seasonal,col="red")
abline(h=0)
plot(new_X1$seasonal,col="red")
abline(h=0)
```

### b 

```{r}
eps=new_X$random
acf(na.omit(eps))
```
### c
```{r}
x_ds=filter(X,rep(1/12,12),method="convolution")
Y=diff(x_ds[6:(144-6)],lag=1,differences = 2)
acf(Y)
pacf(Y)
MOYENNE=mean(Y)
tt=time(Y)
model1=lm(Y~tt)
plot.ts(Y)
abline(model1,lty=20,col="red")

```

## Exercice donnÃ©es de la temÃ©rature global
### prÃ©pation des donnÃ©es et analyse visuelle 
#### 1
```{r}
attach(monthly_csv)
data=as.data.frame(monthly_csv[monthly_csv$Source == "GISTEMP",])
data=data[order(data$Date),]
#yser=data.frame(y,row.names = NULL)
X=ts(data$Mean,start = 1910,end = 2012,frequency = 12)
#X=ts(yser$Mean[(30*12 + 1):n],start = 1910,end = 2012,frequency = 12)
plot(X,col="red")
```
#### 2
```{r}
acf(X)
pacf(X)
lag.plot(X,20)
```
```{r}
monthplot(X,col="red")
boxplot(X~cycle(X))
```
```{r}
lag.plot(X,4)
```

## determination de la tendance et de la saisonalitÃ©
### 1
```{r}
toto=decompose(X,type="additive")
wi=filter(X, 1 , method ="convolution",filter=(1/12) *rep(1,12))

a=rep(0,102)
a1=rep(0,102)
a2=rep(0,102)
a3=rep(0,102)
a4=rep(0,102)
a5=rep(0,102)
a6=rep(0,102)
a7=rep(0,102)
a8=rep(0,102)
a9=rep(0,102)
a10=rep(0,102)
a11=rep(0,102)
for (i in 1:102) {
  a[i]=wi[12*i+1]
  a1[i]=wi[12*i+2]
  a2[i]=wi[12*i+3]
  a3[i]=wi[12*i+4]
  a4[i]=wi[12*i+5]
  a5[i]=wi[12*i+6]
  a6[i]=wi[12*i+7] 
  a7[i]=wi[12*i+8]
  a8[i]=wi[12*i+9]
  a9[i]=wi[12*i+10]
  a10[i]=wi[12*i+11]
  a11[i]=wi[12*i+12]
}
mt=c(mean(na.omit(a)),mean(na.omit(a1)),mean(na.omit(a2)),mean(na.omit(a3)),mean(na.omit(a4)),mean(na.omit(a5)),mean(na.omit(a6)),mean(na.omit(a7)),mean(na.omit(a8)),mean(na.omit(a9)),mean(na.omit(a10)),mean(na.omit(a11)))


```

```{r}
wi=filter(X,(1/12) *rep(1,12) , method ="convolution")
XPRIME=X-wi
```

### 3
```{r}
a=rep(0,102)
a1=rep(0,102)
a2=rep(0,102)
a3=rep(0,102)
a4=rep(0,102)
a5=rep(0,102)
a6=rep(0,102)
a7=rep(0,102)
a8=rep(0,102)
a9=rep(0,102)
a10=rep(0,102)
a11=rep(0,102)
for (i in 1:102) {
  a[i]=XPRIME[12*i+1]
  a1[i]=XPRIME[12*i+2]
  a2[i]=XPRIME[12*i+3]
  a3[i]=XPRIME[12*i+4]
  a4[i]=XPRIME[12*i+5]
  a5[i]=XPRIME[12*i+6]
  a6[i]=XPRIME[12*i+7] 
  a7[i]=XPRIME[12*i+8]
  a8[i]=XPRIME[12*i+9]
  a9[i]=XPRIME[12*i+10]
  a10[i]=XPRIME[12*i+11]
  a11[i]=XPRIME[12*i+12]
}
st=c(mean(na.omit(a)),mean(na.omit(a1)),mean(na.omit(a2)),mean(na.omit(a3)),mean(na.omit(a4)),mean(na.omit(a5)),mean(na.omit(a6)),mean(na.omit(a7)),mean(na.omit(a8)),mean(na.omit(a9)),mean(na.omit(a10)),mean(na.omit(a11)))

```
```{r}
st1=rep(st[1],102)
st2=rep(st[2],102)
st3=rep(st[3],102)
st4=rep(st[4],102)
st5=rep(st[5],102)
st6=rep(st[6],102)
st7=rep(st[7],102)
st8=rep(st[8],102)
st9=rep(st[9],102)
st10=rep(st[10],102)
st11=rep(st[11],102)
st12=rep(st[12],102)


stchap = matrix(data = c(st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12), ncol = 12)
as.vector(stchap)
```



```{r}
epss=XPRIME  - as.vector(stchap)
plot(epss,col="red")
```




### 1
```{r}
res1=decompose(X,type="additive")
tendance=res1$trend
saison=res1$seasonal
eps1=res1$random
plot(res1)
```


```{r}
res2=stl(X,s.window=12,s.degree = 0)
tendance=res2$time.series[,"trend"]
saison=res2$time.series[,"seasonal"]
eps2=res2$time.series[,"remainder"]
plot(eps2)
```
### 2

```{r}
monthplot(epss)
boxplot(epss)
```

### 3
```{r}
acf(saison)
acf(tendance)
acf(eps)
acf(X)
```

## Ajustement de'un modÃ¨le ARIMA pour les rÃ©sidus 

### 1
```{r}
arma=auto.arima(eps1)
arma1=auto.arima(eps2)
```

### 2
```{r}
shapiro.test(arma$residuals) 
shapiro.test(arma1$residuals) 

```
### 3 
```{r}
library(seasonal)
res3=seas(eps)
summary(res3)
view(res3)
```





# Exercice 3 Analyse des donnÃ©es de PNB AM2RICAIN 
###
```{r}
attach(gnpus)
X=ts(gnpus)
plot(X)
```
### 2 

```{r}
Box.test(X,lag=20)
```

### 3 

```{r}
acf(X,2)
acf(X,4)
acf(X,6)
acf(X,12)
pacf(X)

```

### 4
```{r}
modelar=ar(X)
modelar$ar #c donc c'est un ar(3)
modelar
```

### 5 


```{r}
plot(modelar$resid,col="red")
pacf(na.omit(modelar$resid))
shapiro.test(modelar$resid)
```



auto.arima(X)

################################################################
#################################################################

#TP 4 : Prévision

###################################################################
###################################################################
# Exercice 1
# Question 1
Y=EuStockMarkets
plot.ts(Y,col="red")
X<-EuStockMarkets[,"CAC"]
plot.ts(X,col="red")
# Question 2
x=window(X,end=c(1997,260))
plot.ts(x,col="red")
# Questions 3 et 4
xsmooth=HoltWinters(x,alpha=NULL,beta=FALSE,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction")
xsmooth$alpha
# Recherche jusqu'où on peut aller avec erreur inférieure à 100 points
xpred <- predict(xsmooth,n.ahead=length(X)-length(x))
erreur=window(X,start=c(1998,1))-xpred
plot(erreur)
erreur[erreur<100] # 12 pas
# Question 5
xsmooth<-HoltWinters(x,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction")
# Recherche jusqu'où on peut aller avec erreur inférieure à 100 points
xpred <- predict(xsmooth,n.ahead=length(X)-length(x))
erreur=window(X,start=c(1998,1))-xpred
plot(erreur)
erreur<100 # 12 pas
erreur[erreur<100]

# Exercice 2
# Questions 1 à 3
X=arima.sim(n=105,list(ar=c(1,-1/2,1/3)),sd=1)
x=window(X,end=100)
model=arima(x,order=c(3,0,0))
pred<-predict(model,n.ahead=5)$pred
erreur=X[101:105]-pred

# Question 4
erreur=array(rep(0,50*5),c(50,5))
for (i in 1:50)
{
  X=arima.sim(n=105,list(ar=c(1,-1/2,1/3)),sd=1)
  x=window(X,end=100)
  model=arima(x,order=c(3,0,0))
  pred<-predict(model,n.ahead=5)$pred
  erreur[i,]=X[101:105]-pred
}
biais=colMeans(erreur)
variance=colMeans(erreur^2)
# Question 5
for (i in 1:50)
{
  X=arima.sim(n=505,list(ar=c(1,-1/2,1/3)),sd=1)
  x=window(X,end=500)
  model=arima(x,order=c(3,0,0))
  pred<-predict(model,n.ahead=5)$pred
  erreur[i,]=X[501:505]-pred
}
biais=colMeans(erreur)
variance=colMeans(erreur^2)

# Exercice 3
Y=scan('sanfran.dat',skip=1)
Yts<-ts(Y,start=c(1932,1),frequency=12)
plot.ts(Yts,col="red")
acf(Yts)
pacf(Yts)
# saisonnalité de période 12
# avec filtre différence
Z=diff(Yts,lag=12,differences=1)
Zextrait <- window(Z,start=c(1933,1),end=c(1963,12))
par(mfrow=c(1,1))
plot.ts(Zextrait,col="red")
par(mfrow=c(2,1))
acf(Zextrait,na.action=na.pass) # suggère d'utiliser un MA(12)
pacf(Zextrait,na.action=na.pass) # suggère d'utiliser un AR(12) ou AR(24)
# modèle AR
model_AR=arima(Zextrait,order=c(12,0,0))
model_AR2=arima(Zextrait,order=c(24,0,0))
acf(model_AR$residuals)
Box.test(model_AR$residuals,lag=20)
acf(model_AR2$residuals)
Box.test(model_AR2$residuals,lag=20)
predAR=predict(model_AR,n.ahead=length(Z)-length(Zextrait))
par(mfrow=c(1,1))
plot.ts(Z,xlim=c(1963,1967))
lines(predAR$pred,col=3)
predAR2=predict(model_AR2,n.ahead=length(Z)-length(Zextrait))
lines(predAR2$pred,col=6)
model_MA=arima(Z,order=c(0,0,12))
acf(model_MA$residuals)
Box.test(model_MA$residuals,lag=20)
erreur1=window(Z,start=c(1964,1))-predAR$pred
erreur2=window(Z,start=c(1964,1))-predAR2$pred
plot(erreur1)
lines(erreur2,col=3)
# modèle MA
model_MA=arima(Zextrait,order=c(0,0,12))
acf(model_MA$residuals)
Box.test(model_MA$residuals,lag=20)
predMA=predict(model_MA,n.ahead=length(Z)-length(Zextrait))
par(mfrow=c(1,1))
plot.ts(Z,xlim=c(1963,1967))
lines(predMA$pred,col=3)
erreur3=window(Z,start=c(1964,1))-predMA$pred
plot(erreur1)
lines(erreur2,col=3)
lines(erreur3,col=6)
# Modèle SARIMA
Ytsextrait=window(Yts,start=c(1933,1),end=c(1963,12))
model_SARIMA=arima(Ytsextrait,order=c(2,0,0),seasonal=list(order=c(2,0,0),period=12))
acf(model_SARIMA$residuals)
Box.test(model_SARIMA$residuals,lag=20)
predSARIMA=predict(model_SARIMA,n.ahead=length(window(Yts,start=c(1933,1)))-length(Ytsextrait))
par(mfrow=c(1,1))
plot.ts(Yts,xlim=c(1963,1967))
lines(predSARIMA$pred,col=3)
erreur4=window(Yts,start=c(1964,1))-predSARIMA$pred
plot(erreur4)












