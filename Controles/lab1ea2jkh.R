library(gridExtra)
library(ggplot2) 
library(readr) 
library(dplyr)
library(vtreat) 
library(broom) 
library(caret) 
library(glmnet) 
library(Hmisc)
library(factoextra)
library(class)
library(corrplot)
library(stats)
library(tidyverse) 
library(ggpubr) 
library(arsenal) 
library(randomForest) 
library(caret) 
library(class) 
library(MASS)
library(e1071)
#Cargar data
system("ls ../input")
data <- read_csv("/Users/claudiameneses/desktop/jgi.csv")
datos <- data.frame(data[c(1,2,3,4)])

View(datos)
#Omitir datos que no están completos tienen al menos un NA
sum(!complete.cases(datos))
datos <- na.omit(datos)
summary(datos)
datosacp <- datos[c(2,3,4)]
View(datosacp)
cor_matrix <- cor(datosacp, method = "pearson", use = "complete.obs")
cor_matrix
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

scaled_datos <- scale(datosacp)
#Dispersión
pairs(scaled_datos,main="Dispersión",col="coral3",upper.panel = NULL)   

acp<-prcomp(scaled_datos,center = TRUE, scale. = TRUE)
#proporción de la varianza y varianza acumulada
summary(acp)
acp$rotation
acp$x
CP1<-acp$x[,1]
CP2<-acp$x[,2]
plot(CP1,CP2,main="CP uno y dos",col="blueviolet", xlab = "Componente Principal 1", ylab = "Componente Principal 2")

#Representación bidimensional de las dos primeras componentes.
fviz_pca_biplot(acp, repel = FALSE, col.var = "mediumvioletred", col.ind = "#696969")
fviz_pca_var(acp, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

datos[c(8, 11, 14),]
datos[c(37, 31),]
datos[c(4, 13, 21, 25, 30),]

dest <- c(8,11,14,37,31,4,13,21,25,30)

datos[-dest,]
