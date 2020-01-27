  library(gridExtra)#Compare graph
  library(ggplot2) # Data visualization
  library(readr) # CSV file I/O, e.g. the read_csv function
  library(dplyr)
  library(caret) #Calssificaton and regretion training
  library(Hmisc)#Correlation matrix
  library(factoextra)
  library(class)
  library(corrplot)
  library(stats)
  #Cargar data
  system("ls ../input")
  #data <- read_csv("/Users/claudiameneses/desktop/prestige.csv")
  data <- read.table("http://allman.rhon.itam.mx/~ebarrios/EstApl2-2019/controles/control2/consumoAguaElect.dat", header = T)
  datos <- data.frame(data[c(1,2,3)])
  View(datos)
  
  data <-datos[-c(1)]
  pairs(data)
  summary(data)
  ggplot(datos, aes(x=c.elec, y = c.agua))+geom_point()+ ggtitle("Relación entre consumo eléctrico de agua") +
    xlab("Consumo eléctrico") + ylab("Consumo de agua")
  cor_matrix <- cor(data, method = "pearson", use = "complete.obs")
  cor_matrix
  corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  
  #Regresión
  regresion <- lm(c.agua ~ c.elec, data = datos)
  summary(regresion)
  plot(regresion)
  
  #Residuales
  plot(data$c.elec, regresion$residuals, main="Residuales", xlab="Consumo eléctrico", ylab="Residuales ", pch=19)
  abline(lm(regresion$residuals ~ c.elec, data = datos), col="red") 
  var(regresion$residuals)
  
  #plot(datos$c.elec, datos$c.agua, xlab='Consumo de electricidad', ylab='Consumo de agua')
  #abline(regresion)
  ggplot(datos, aes(x=c.elec, y = c.agua))+geom_point()+geom_smooth(method = lm, se = FALSE) 
  residuos <- rstandard(regresion)
  valores.ajustados <- fitted(regresion)
  plot(valores.ajustados, residuos)
  qqnorm(residuos)
  
  
  
  #Regresión
  regresion <- lm(c.agua ~ c.elec, data = datos)
  summary(regresion)
  plot(regresion)
  
  #Box - Cox
  library(MASS)
  bc <- boxcox(regresion)
  lamda <- bc$x[which(bc$y == max(bc$y))]
  lamda
  bcmodel <- lm( (c.agua)^ lamda ~ c.elec, data = datos)
  summary(bcmodel)
  plot(bcmodel)
  ##IC
  aux <- bc$x[bc$y > max(bc$y) - 1/2 * qchisq(.90,1)]
  sort(aux)
  aux
  












