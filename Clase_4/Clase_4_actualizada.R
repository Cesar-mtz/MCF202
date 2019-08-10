#César A. Martínez Gauna
#09/08/2019
#Clase_4_correlación_regresión

# Ejercicio 1 -------------------------------------------------------------

# Establecimiento de hipótesis --------------------------------------------
#H0.- No existen diferencias estadísticamente significativas en las variables diametro y altura.
#H1.- Sí existen diferencias estadísticamente significativas en las variables diametro y altura.

#Importa datos de Ebanos 
ebanos <- read.csv("C:/MCF2019/MCF2/MCF202/ebanos.csv", header= T)
plot(ebanos$diametro, ebanos$altura, pch=19, col= "blue",
     xlab = "Diametro",
     ylab = "Altura")


library(pastecs)
stat.desc(ebanos$altura, basic = FALSE, norm= TRUE)
#Con el comando de stat.desc se aplica a la variable altura debido a que es la variable dependiente.


# Prueba Shapiro-Wilk para normalidad de datos ----------------------------
shapiro.test(log(ebanos$altura))
shapiro.test(ebanos$altura)
# De acuerdo con el resultado obtenido con la prueba de shapiro, se observa el valor obtenido
# de p-value de (0.008242), al compararlo con el valor estándar de alfa (0.05) es menor así que
# aceptamos H1 obteniendo diferencias significativas.

shapiro.test(ebanos$diametro)
#Si existen diferencias significativas según la prueba de shapiro que nos da un valor de 
#p-value de 1.215e-05.

cor.test(ebanos$diametro, ebanos$altura)

#Al ralizar la correlacion aceptamos nos damos cuenta que p-value (2.2e-16) es menor que (0.05)
#indicando que la hipotesis alternativa H1 es aceptada, es decir, si hay
#diferencias significativas entre las variables diámetro y altura.


# EJERCICIO 2 Correlación -------------------------------------------------------------

library(repmis)
erupciones <- source_data("https://dl.dropboxusercontent.com/s/liir6sil7hkqlxs/erupciones.csv=1")

plot(erupciones$waiting, erupciones$eruptions, pch= 19, col= "red",
     xlab = "Tiempo de espera (min)",
     ylab = "Duración en (min")

library(pastecs)
stat.desc(erupciones$eruptions, basic = FALSE, norm = TRUE)

shapiro.test(erupciones$eruptions)

#Segun la prueba de shapiro los datos no son de distribucion normal ya que se encuentran 
#por debajo de el alfa establecido 0.05. 

shapiro.test(log(erupciones$eruptions))
shapiro.test(erupciones$waiting)

cor.test(erupciones$eruptions, erupciones$waiting)
#Si hay una correlacion es significativa porque esta por debajo de 0.05 por lo cual se acepta H1


# Regresión Lineal  -------------------------------------------------------

#Hipotesis general: Que el tiempo de espera nos ayudara a predecir la duracion de la
#proxima erupción

#H0= No es significativa para predecir la próxima erupción.
#H1= sí es significativa para predecir la próxima erupción.

#Comando "lm" para realizar la regresión líneal.
lm.erup <- lm(erupciones$eruptions ~ erupciones$waiting)

#Grafica
plot(erupciones$waiting, erupciones$eruptions, pch= 19, col= "green",
     xlab = "Tiempo de espera (min)",
     ylab = "Duración en (min")

abline(lm.erup, col= "black")

text(52, 4.5, "Y = -1.87 + 0.07*x")
text(52, 4, "r^2 = 0.81")

lm.erup
summary(lm.erup)

length(erupciones$eruptions)

sqrt(0.90)
(0.90)^2

#Para saber la duración en tiempo de espera del 60 min.
y.60 <- -1.87 + 0.07*60
y.60


# Datos de regresión ------------------------------------------------------

#Comando para asígnar la fórmula a un nuevo directorio para no escribir toda la fórmula.
espera <-erupciones$waiting
duracion <- erupciones$eruptions

res <- resid(lm.erup)
res
sum(res)

pre <- fitted(lm.erup)
res.2 <- res^2

cuadro <- round(data.frame(espera, duracion, pre, res,
                           res.2),4)
SSE <- sum(cuadro$res.2)
SSE

SSE <- sum((duracion - pre)^2)                
SSE

vari <- SSE/(length(erupciones$waiting)-2)
vari                               

# Prueba de hipotesis de la regresión -------------------------------------

an.erup <- anova(lm.erup)
an.erup

#aceptamos la hipótesis alternativa que el modelo de regresion aplicado son 
#significativos, entonces podemos decir que la regresión se puede aplicar.
