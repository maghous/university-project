library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(plyr)
library(kableExtra)
library(knitr)
library(readr)
#modifier la taille de graphe
fig<-function(x,y){
  options(repr.plot.width=x,repr.plot.width=y)
}
#ggtitle: title of plot
#xlab plot xaxis tile
#scale_x_discret() controlling discret x axis variable
#xlim limiting x axis
#theme_minimal() minimal theme
df<-read.csv("train.csv")
attach(df)
#basic scatter plot
fig(20,14)
ggplot(df,aes(Age,Fare))+geom_point()+ggtitle('Age vs Fare')+theme_classic()
ggplot(df,aes(Age,Fare))+geom_point()+ggtitle('Age vs Fare')+theme_economist()
ggplot(df,aes(Age,Fare))+geom_point()+ggtitle('Age vs Fare')+theme_excel_new()
#scatter plot - category 
fig(20,14)
ggplot(df,aes(Age,Fare))+geom_point(aes(color=Sex))+ggtitle('Age vs Fare against sex')+theme_classic()
ggplot(df,aes(Age,Fare))+geom_point(aes(color=Sex))+ggtitle('Age vs Fare against sex')+theme_economist()
ggplot(df,aes(Age,Fare))+geom_point(aes(color=Sex))+ggtitle('Age vs Fare against sex')+theme_excel_new()

df1<-read.csv("world-happiness-report-2021.csv")
attach(df1)
view(df1)
names(df1)
summary(df1)
str(df1)
sum(is.na(df1))
length(unique(df1$ï..Country.name))
length(unique(df1$Regional.indicator))

ma<-cor(df1[3:ncol(df1)])
corrplot(ma, method="circle", type='upper', tl.cex=0.3, tl.col = 'black')
corrplot(ma, method="square", type='full', tl.cex=0.3, tl.col = 'black')







