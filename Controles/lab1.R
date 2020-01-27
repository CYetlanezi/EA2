library(gridExtra)#Compare graph
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(vtreat) #'data.frame' statiscally procesor
library(broom) #Convert Statistical Analysis Objects into Tidy Tibbles
library(caret) #Calssificaton and regretion training
library(glmnet) #Lasso & Elastic - NEt Regularized Generalied models
library(Hmisc)#Correlation matrix
library(factoextra)
library(class)
library(corrplot)
library(stats)
#Cargar data
system("ls ../input")
data <- read_csv("/Users/claudiameneses/desktop/prestige.csv")
datos <- data.frame(data[c(1,2,3,4,5,6)])
View(datos)

#Resumen de data
summary(datos)

#Correlaciones
datos_num <- data.frame(data[c(2,3,4,5)])
cor_matrix <- cor(datos_num, method = "pearson", use = "complete.obs")
cor_matrix
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Estandarizar datos
scaled_datos <- scale(datos_num)

#Dispersi?n
pairs(scaled_datos,main="Dispersión",col="coral3",upper.panel = NULL)   

#PCA
acp<-prcomp(scaled_datos,center = TRUE, scale. = TRUE)
#proporci?n de la varianza y varianza acumulada
summary(acp)
CP1<-acp$x[,1]
CP2<-acp$x[,2]
plot(CP1,CP2,main="CP uno y dos",col="blueviolet", xlab = "Componente Principal 1", ylab = "Componente Principal 2")


#indice
#ind <- data.frame(data$ocupacion,CP1)
#View(ind)

#Representaci?n bidimensional de las dos primeras componentes.
fviz_pca_biplot(acp, repel = FALSE, col.var = "mediumvioletred",habillage=data$tipo, col.ind = "#696969")
fviz_pca_var(acp, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
