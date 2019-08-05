# Cesar Martinez Gauna
# 05/08/2019
# Clase 0

##Contenido del curso

##Script

# Pasos básicos -----------------------------------------------------------

2 + 2
a <- 2
a + a
a + 5

# Importar datos ----------------------------------------------------------


diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)

diametro
# Medidas de tendencia central
mean(diametro)
median(diametro)

#MEdias de dispersión
sd(diametro)
var(diametro)


# Gráfica -----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col = "lightblue", main= "Diámetro",
        xlab="D (cm)")

# Importar excel ----------------------------------------------------------

DB_alturas <- read.csv("C:/MCF202-2019/MCF202/ALTURAS/alturas.csv", header = T)
head(DB_alturas)
