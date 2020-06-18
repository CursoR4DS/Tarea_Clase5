rm(list=ls())

#setear el directorio de trabajo
setwd("C:/Users/user/Desktop/R_CTIC/Tarea_Clase5_R4DS")
getwd()
dir()


#### REGRESIÓN ####

edad <- c(56,42,72,36,63,47,55,47,38,42)
presion <- c(148,126,159,118,149,130,151,142,114,141)

# edad
# [1] 56 42 72 36 63 47 55 47 38 42
# presion
# [1] 148 126 159 118 149 130 151 142 114 141

plot(presion,edad)

#donde presion es el eje X, y edad el eje Y
reg_lin <- lm(edad ~ presion)
reg_lin

# Call:
#   lm(formula = edad ~ presion)
# 
# Coefficients:
#   (Intercept)      presion  
# -43.5440       0.6774

# edad = ???43,5440 + 0,6774presion

summary (reg_lin)

# Call:
#   lm(formula = edad ~ presion)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.9676 -2.9835 -0.0973  3.8623  7.8394 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -43.5440    17.6126  -2.472 0.038571 *  
#   presion       0.6774     0.1271   5.328 0.000704 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.742 on 8 degrees of freedom
# Multiple R-squared:  0.7802,	Adjusted R-squared:  0.7527 
# F-statistic: 28.39 on 1 and 8 DF,  p-value: 0.000704

plot(presion,edad)
abline(reg_lin)

xmin <- 0.9 * min(presion)
xmax <- 1.1 * max(presion)
ymin <-0.9 * min(edad)
ymax <- 1.1 * max(edad)

png(filename="Pob_Mujeres10.png")

plot(presion,edad,main="Edad ~ Presion Sanguinea",sub="POb. : 10mujeres",
     xlab="Presion",ylab="Edad",
     xlim=c(xmin,xmax),ylim=c(ymin,ymax))
dev.off()  

abline(reg_lin)
dev.off()

#jpeg("Pob_Mujeres10.png")

#para modificar algunos parametros del archivo imagen
#ggsave("Pob_Mujeres10.png",width=16, height=9, dpi=72)

plot(reg_lin)

#### Test de normalidad de Kolmogorov-Smirnov ####

ks.test(reg_lin$residuals,"pnorm")

# One-sample Kolmogorov-Smirnov test
# 
# data:  reg_lin$residuals
# D = 0.3935, p-value = 0.06608
# alternative hypothesis: two-sided

# El p-valor que se obtiene (0.06608) es menor que 0.1.
# Sin embargo para una significación del 5 % no se debe rechazar la hipótesis nula.

# H0 ??? ??0 = 0 vs H1 ??? ??0 diferente a 0
# H0 ??? ??1 = 0 vs H1 ??? ??1 diferente a 0

#### Test de Durbin-Watson ####

library(lmtest)
dwtest(edad ~ presion)

# Durbin-Watson test
#
# data:  edad ~ presion
# DW = 1.9667, p-value = 0.5879
# alternative hypothesis: true autocorrelation is greater than 0

# En este caso, con un p-valor de 0.5879 no podemos rechazar la hipótesis
# de que los residuos son independientes.


#### REGRESION MULTIPLE ####

gastos<-c(1000,580,520,500,600,550,400)
ingresos<-c(50000,2500,2000,1900,3000,4000,2000)
tamaño<-c(7,4,3,3,6,5,2)
hijosU<-c(3,1,1,0,1,2,0)

datos2<- data.frame(gastos,ingresos,tamaño,hijosU)

# A continuación ajustamos el modelo de regresión lineal múltiple
reg_lin_mul<-lm(gastos ~ ingresos+tamaño+hijosU)
summary(reg_lin_mul)

# Call:
#   lm(formula = gastos ~ ingresos + tamaño + hijosU)
# 
# Residuals:
#   1       2       3       4       5       6       7 
# 1.216  48.164  29.125  15.209 -10.134 -35.402 -48.178 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 3.590e+02  6.291e+01   5.706   0.0107 *
#   ingresos    7.247e-03  1.802e-03   4.021   0.0276 *
#   tamaño      3.734e+01  2.046e+01   1.825   0.1655  
# hijosU      5.359e+00  4.061e+01   0.132   0.9034  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 48.57 on 3 degrees of freedom
# Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9353 
# F-statistic: 29.93 on 3 and 3 DF,  p-value: 0.009772

# El p-valor asociado al contraste (0.009772) es menor que a = 0.05 ,
# por lo que rechazamos la hipótesis nula. Esto implica que al menos una
# de las variables independientes contribuye de forma significativa a la
# explicación de la variable respuesta
# Para las variables tamaño familiar y número de hijos en la Universidad,
# los p-valores son 0.1655 y 0.9034, respectivamente. Ambos mayores
# que 0.05, por lo que no rechazamos la hipótesis nula de significación
# de ambas variables. Estas variables no son válidas para predecir los
# gastos alimentación mensual de una familia y por tanto se pueden
# eliminar del modelo

#### LIBRERIA PRESTIGE ####
library(car)
head(Prestige,5)
newdata=Prestige[,c(1:2)]
summary(newdata)
modelo=lm(income~education,data=newdata)
plot(newdata$education,newdata$income,main="Educacion~Income")
abline(modelo)
summary(modelo)

# Call:
#   lm(formula = income ~ education, data = newdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5493.2 -2433.8   -41.9  1491.5 17713.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2853.6     1407.0  -2.028   0.0452 *  
#   education      898.8      127.0   7.075 2.08e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3483 on 100 degrees of freedom
# Multiple R-squared:  0.3336,	Adjusted R-squared:  0.3269 
# F-statistic: 50.06 on 1 and 100 DF,  p-value: 2.079e-10

#### MODELO DE REGRESION LOGISTICA ####

# https://www.kaggle.com/ludobenistant/hr-analytics
# 
# We can't find that page: DATA NO DISPONIBLE








