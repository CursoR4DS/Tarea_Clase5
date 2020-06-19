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

datos <- read.csv("HumanResourcesAnalytics.csv",T)
muestra <- dim(datos)[1]
datos <- datos[sample(muestra,100,replace=TRUE),]
class(datos)
str(datos)
head(datos)
View(datos)
colnames(datos)=c("nivel_satisfaccion","ultima_evaluacion","numero_proyectos","promedio_horas_mensuales","antiguedad"
                 ,"accidente","abandona","promocionado","departamento","salario")

help("sample")
# sample toma una muestra del tamaño especificado de los elementos de x usando con o sin reemplazo

datos.modelo <- subset(datos,select=c(abandona,nivel_satisfaccion,ultima_evaluacion))
datos.modelo$abandona <- factor(datos.modelo$abandona)
head(datos.modelo)
plot(datos.modelo$nivel_satisfaccion,datos.modelo$abandona)

help("subset")

table(datos.modelo$abandona)
# 0  1 
# 79 21 

summary(datos.modelo$nivel_satisfaccion)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0900  0.4825  0.6550  0.6318  0.8400  1.0000 
summary(datos.modelo$ultima_evaluacion)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3800  0.5900  0.7400  0.7349  0.8625  1.0000 



#### FUNCION GLM ####

modelo.logit <- glm(abandona ~ultima_evaluacion + nivel_satisfaccion, data=datos.modelo, family="binomial")

# La principal diferencia con la función lm para ajustar modelos lineales es que le tenemos que
# proporcionar la familia de la distribución. En nuestro caso, como es una
# variable dicotómica, la familia es la binomial

summary(modelo.logit)

# Call:
#   glm(formula = abandona ~ ultima_evaluacion + nivel_satisfaccion, 
#       family = "binomial", data = datos.modelo)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.2259  -0.6786  -0.5355  -0.3805   2.1015  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)          -0.456      1.252  -0.364  0.71581
# ultima_evaluacion     1.225      1.555   0.788  0.43069
# nivel_satisfaccion   -3.036      1.052  -2.887  0.00389
# 
# (Intercept)          
# ultima_evaluacion    
# nivel_satisfaccion **
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 102.791  on 99  degrees of freedom
# Residual deviance:  93.446  on 97  degrees of freedom
# AIC: 99.446
# 
# Number of Fisher Scoring iterations: 4


# La variable ultima_evaluacion no es significativa en
# el modelo ,el p-valor debe ser mucho mayor de 0.05, mientras que la
# variable nivel_satisfaccion es moderadamente significativa si es que
# su p-valor entre 0.01 y 0.05.

#### Ejemplo Modelo de Regresion Logistica ####

exp(coefficients(modelo.logit))
# (Intercept)  ultima_evaluacion 
# 0.63384548         3.40555702 
# nivel_satisfaccion 
# 0.04803069

log.odds <- predict(modelo.logit, data.frame(nivel_satisfaccion=0.6,
                                             ultima_evaluacion=0.75))
log.odds
#         1 
# -1.358443 

# La probabilidad de abandonar la empresa sería:
exp(log.odds)/(1-exp(log.odds))
#        1 
# 0.3460051 


q <- seq(from=0, to=20,by=0.1)
y <- 500 + 0.4*(q-10)^3
noise <- rnorm (length(q), mean=10, sd=80)
noisy.y <- y+noise
plot(q, noisy.y, col='deepskyblue4',xlab='q' , main='Observeddata')
lines(q,y,col='firebrick1',lwd=3)
model <- lm(noisy.y ~ poly(q,3))

confint(model, level=0.95)

# 2.5 %     97.5 %
# Intercept)     507.2712  529.75856
# poly(q, 3)1   1825.4506 2144.26438
# poly(q, 3)2   -238.6370   80.17677
# poly(q, 3)3    721.5212 1040.33495
