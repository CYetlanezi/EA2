# Lista 9.

# Ajuste el modelo.

datos <- read.table("/Users/claudiameneses/desktop/EA2/lista09.dat", header=TRUE)[,-1]

mod1 <- lm(y~x1+x2+x3, data=datos)
summary(mod1)

#Todos los coeficientes son significativos menos el ultimo x3.
# La r^2 ajustada castiga, pues aportas menos con mas regresores.
# Procedemos a graficar los residuales.

plot(mod1$fitted.values,residuals(mod1))
abline(h=0, lty="dotted", col="red")
# Al ver la grafica nos percatamos que posiblemente tenga un problema de heteroscedasticidad.
# Los residuales parecen expandirse a medida que aumenta la respuesta ajsutada.

#note la variabilidad con respecto a las regiones.

boxplot(datos$y~as.factor(datos$region))
#La variabilidad es evidente por tipo de region. 

plot(residuals(mod1)~datos$region)
print(influence.measures(mod1))
# La variabilidad de los residuales depende mucho del tipo de region.
# A partir de los residuales estime la varianza de los errores por region. 
# la varianza muestral.....

#Ajuste sin Alasaka, pues es un dato demasiado atipico.

mod2 <- lm(y~x1+x2+x3, data=datos[-49,])
summary(mod2)
#el modelo cambio bastante.
 
#MINIMOS CUADRADOS PONDERADOS.

#Calculamos la varianza muestral... 

res <- residuals(mod2)
res_1 <- res[datos$region==1]
n1 <- length(res_1)
var_1 <- var(res_1)
var_1 <- rep(var_1,n1)


res_2 <- res[datos$region==2]
n2 <- length(res_2)
var_2 <- var(res_2)
var_2 <- rep(var_2,n2)

res_3 <- res[datos$region==3]
n3 <- length(res_3)
var_3 <- var(res_3)
var_3 <- rep(var_3, n3)

res_4 <- res[datos$region==4][-length(res[datos$region==4])]
n4 <- 13
var_4 <- na.omit(var(res_4))
var_4 <- rep(var_4, n4)

vars <- c(var_1,var_2,var_3,var_4)
wi <- sqrt(1/vars)
#ponderaciones

R=diag(x=wi)
w <- (solve(R)%*%datos$y)[-49]
Z <- (solve(R)%*%model.matrix(mod1))[-49,]
mcp <- lm(w~0+Z[,1]+Z[,2]+Z[,3]+Z[,4])
#Bueno lo intente jajajajaja.
#Sale algo parecido a lo que le sale barrios, supongo que no estoy tan mal...

#Si comparo los modelos probablemente salga que el de minimos cuadrados ponderados se ajusto perfecto.

