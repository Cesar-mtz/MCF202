# César A. Martínez Gauna
# 07/08/2019
# Clase_3


# Hipótesis ---------------------------------------------------------------

#Ho No existe diferencia significativa entre las variables Fotografia y Araña
#H1 Sí existe diferencia significativa entre las variables Fotografia y Araña


# COMPARACION DE LAS MEDIAS -----------------------------------------------
Grupo <- gl(2, 12, labels = c("fotografia", "araña"))
Ansiedad <- c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45,40, 50, 40, 35, 50,
              55, 65, 55, 50, 35, 30, 50,60,39)
Datos <- data.frame(Grupo, Ansiedad)
head(Datos)
summary(Datos)


# ANALISIS DE LAS MUESTRAS DEPENDIENTES -----------------------------------
boxplot(Datos$Ansiedad ~ Datos$Grupo, col = "lightblue", ylab = "Nivel de ansiedad")

tapply(Datos$Ansiedad, Datos$Grupo, mean)


# PRUEBA DE SHAPIRO -------------------------------------------------------
shapiro.test(Datos$Ansiedad)

library(pastecs)


# EJERCICIO 2. ----------------------------------------------------------------------

#Ho Es la media es significativamente igual a 80
#H1 Es la media es significativamente menor a 80

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)

summary(costal)
mean(costal)
# De acuerdo con el resultado de la media con la cual está indicando que 
# es estadisticamente menor a 80, es decir, aceptamos la H1.


# Determinar el numero de observaciones -----------------------------------
n <-length(costal)
n

# Determinar la media -----------------------------------------------------
costa.media <- mean(costal)



# Determinar la Desviacion estándar ---------------------------------------
costa.sd <- sd(costal)
sd(costal)
#con el primer comando se le asigna el valor de la SD, es decir, como un nuevo identificador



# Fórmula para obener el valor de t ---------------------------------------
costa.se <- costa.sd/sqrt(n)

# valor de T
costa.T <- (costa.media - 80)/costa.se


# Determinar valor de p ---------------------------------------------------
pt(costa.T, df = n-1)


t.test(costal, mu= 80, alternative = "less")
t.test(costal, mu= 80, alternative = "greater")
