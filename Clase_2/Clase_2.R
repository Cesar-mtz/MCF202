# César A. Martínez Gauna
# 06/08/2019
# Clase 2


# Importar datos Excel ----------------------------------------------------

Tvivero <- read.csv("C:/MCF202-2019/MCF202/DATOS/Tvivero.csv", header = T)
summary(Tvivero)


# Prueba t una muestra  ---------------------------------------------------

par(mfrow=c(1,1))
boxplot(Tvivero$IE)

t.test(Tvivero$IE, mu = 0.85)

#la media observada  no es diferente estadisticamente ya que el valor
# de p es mayor que el alfa establecido (0.05). Además la media-teorética se 
#encuentra dentro del rango de los valores de intervalos de confianza.

t.test(Tvivero$IE, mu = 0.90)
# La media observada estadisticamente es diferente a la media teorética 
# por lo cual aceptamos la hipótesis H1 con valor de p (0.01)
#es menor ue el valor de alfa establecido (0.05)



# Prueba t student independiente ------------------------------------------

boxplot(Tvivero$IE ~ Tvivero$Tratamiento, col = "red",
        xlab = "Tratamiento",
        ylab = "IE")


# Prueba para ver si hay varianza igual -----------------------------------

shapiro.test(Tvivero$IE)


# Prueba de Varianza ------------------------------------------------------

var.test(Tvivero$IE ~ Tvivero$Tratamiento)

#La varianza de ambos tratamientos son iguales asi lo prueba el valor de p
#obtenido mediante una prueba de varianza


t.test(Tvivero$IE ~ Tvivero$Tratamiento, var.equal = T)
#Se rechaza la H0 se acepta la H1 ya que el valor de p (0.004)
#es menor a 0.05 significando que si influye en el IE en las plantas
