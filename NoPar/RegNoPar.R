library(ggplot2)
library(dplyr)
library(ggstance)
library(gam)
library(locfit)
library(hexbin)
library(RColorBrewer)
library(latticeExtra)


avo <- read.csv("/Desktop/NoPar/avocado.csv", header = TRUE)

# Todos los Abuacates ---------------------------------------------

avoTot <- avo %>%
  select(Date, AveragePrice, Total.Volume, X4046, X4225,X4770, type, year, region) %>%
  filter(region == "TotalUS") %>%
  arrange(Date) %>%
  select(-c(region,year))

with(avo,plot(AveragePrice, Total.Volume))
title(main = "Todos")

# Abuacates Inorgánicos ---------------------------------------------

avoIn <- avo %>%
  select(Date, AveragePrice, Total.Volume, X4046, X4225,X4770, type, year, region) %>%
  filter(region == "TotalUS") %>%
  filter(type == "conventional") %>%
  arrange(Date) %>%
  select(-c(region,year))


rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
hexbinplot(Total.Volume ~ AveragePrice, avoIn, colramp=rf, main = 'Inorgánicos')

#Genera un modelo locfit que se basa en polinomios

modeloIn1 <- locfit(Total.Volume ~ AveragePrice, avoIn)
modeloIn2 <- locfit(Total.Volume ~ lp(AveragePrice, deg = 3), avoIn)

#Calculamos el nn adecuado para el modelo con validación cruzada

alfa<-seq(0.20,1,by=0.01)
n1 <-length(alfa)
g <-matrix(nrow=n1,ncol= 4)
for(k in 1:n1){
  g[k,]<-gcv(Total.Volume ~ lp(AveragePrice,nn=alfa[k]), avoIn)
  }
plot(g[,4],type="o",pch=16)
h <- alfa[which.min(g[,4])]

#Calculamos el grado adecuado para el modelo con validación cruzada

beta<-seq(1,20,by=1)
n1 <-length(beta)
g <-matrix(nrow=n1,ncol= 4)
for(k in 1:n1){
  g[k,]<-gcv(Total.Volume ~ lp(AveragePrice,nn=h,deg=beta[k]), avoIn)
}
plot(g[,4],type="o",pch=16)
d <- beta[which.min(g[,4])]


#Calculamos el último modelo y sacamos el plot

modeloIn3 <- locfit(Total.Volume ~ lp(AveragePrice,nn = h, deg = d), avoIn)

hexbinplot(Total.Volume ~ AveragePrice, avoIn, colramp=rf, main = 'Inorgánicos', panel=function(x, y, ...)
{
  panel.hexbinplot(x, y, ...)
  panel.lines(modeloIn3, lwd = 3, col = "firebrick1")
  #panel.lines(modeloIn1, lwd = 3, col = "blue")
  #panel.lines(modeloIn2, lwd = 3, col = "purple")
})


#Hacemos la predicción
pred <- predict(modeloIn3, newdata = data.frame(AveragePrice = 1.317175),se.fit=T)


# #Abuacates Orgánicos ---------------------------------------------

avoOrg <- avo %>%
  select(Date, AveragePrice, Total.Volume, X4046, X4225,X4770, type, year, region) %>%
  filter(region == "TotalUS") %>%
  filter(type == "organic") %>%
  arrange(Date) %>%
  select(-c(region,year))

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
hexbinplot(Total.Volume ~ AveragePrice, avoOrg, colramp=rf, main = 'Orgánicos')

#Genera un modelo locfit que se basa en polinomios

modeloOrg1 <- locfit(Total.Volume ~ AveragePrice, avoOrg)
modeloOrg2 <- locfit(Total.Volume ~ lp(AveragePrice, deg = 3), avoOrg)

#Calculamos el nn adecuado para el modelo con validación cruzada

alfa<-seq(0.20,1,by=0.01)
n1 <-length(alfa)
g <-matrix(nrow=n1,ncol= 4)
for(k in 1:n1){
  g[k,]<-gcv(Total.Volume ~ lp(AveragePrice,nn=alfa[k]), avoOrg)
}
plot(g[,4],type="o",pch=16)
h <- alfa[which.min(g[,4])]

#Calculamos el grado adecuado para el modelo con validación cruzada

beta<-seq(1,20,by=1)
n1 <-length(beta)
g <-matrix(nrow=n1,ncol= 4)
for(k in 1:n1){
  g[k,]<-gcv(Total.Volume ~ lp(AveragePrice,nn=h,deg=beta[k]), avoOrg)
}
plot(g[,4],type="o",pch=16)
d <- beta[which.min(g[,4])]


#Calculamos el último modelo y sacamos el plot

modeloOrg3 <- locfit(Total.Volume ~ lp(AveragePrice,nn = h, deg = d), avoOrg)

hexbinplot(Total.Volume ~ AveragePrice, avoOrg, colramp=rf, main = 'Orgánicos', panel=function(x, y, ...)
{
  panel.hexbinplot(x, y, ...)
  panel.lines(modeloOrg3, lwd = 3, col = "firebrick1")
  #panel.lines(modeloOrg1, lwd = 3, col = "purple")
  #panel.lines(modeloOrg2, lwd = 3, col = "green")
})

#Hacemos la predicción
pred <- predict(modeloOrg3, newdata = data.frame(AveragePrice = 1.556895),se.fit=T)

