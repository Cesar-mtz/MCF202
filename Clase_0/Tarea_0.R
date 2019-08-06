# César A. Martínez Gauna
# 06/08/2019
# Tarea 1


# Importar Datos de Excel ----------------------------------------------------------

cuadro1 <- read.csv("C:/MCF202-2019/MCF202/Clase_0/cuadro1.csv", header = T)
head(cuadro1)


# Determinar Altura.subset -----------------------------------------------------------

H.media <- subset(cuadro1, Altura <= mean(cuadro1$Altura))

H.16 <- subset(cuadro1, Altura < 16.5)


# Variable subset para Vecinos----------------------------------------------------------

Vecinos.3 <- subset(cuadro1, Vecinos <= 3)
Vecinos.4 <- subset(cuadro1, Vecinos > 4)


# Variable subset para Diámetro ---------------------------------------------------------

DBH.media <- subset(cuadro1, Diametro < mean(cuadro1$Diametro))
DBH.16  <- subset(cuadro1, Diametro > 16)


# Aplicar subset a las siguientes Especies ----------------------------------------------------------

# Subset para la Especie Cedro --------------------------------------------


Cedro <- cuadro1[(cuadro1$Especie == "C"),]


Diametro.Cedro16.9 <- subset(Cedro, Diametro <= 16.9)
Altura.Cedro18.5 <- subset(Cedro, Altura > 18.5)


# Subset para las Especies restantes Tsuga y Douglasia --------------------


Especies.Restante <- cuadro1[!(cuadro1$Especie == "C"),]
Diametro.Restante16.9 <- subset(Especies.Restante, Diametro <= 16.9)
Alturas.Restante18.5 <- subset(Especies.Restante, Altura > 18.5)



# Histogramas para Altura -------------------------------------------------------------

hist(cuadro1$Altura)
hist(H.media$Altura)
hist(H.16$Altura)


# Histogramas para vecinos -----------------------------------------------------


hist(cuadro1$Vecinos)
hist(Vecinos.3$Vecinos)
hist(Vecinos.4$Vecinos)

# histogramas para Diámetro ----------------------------------------------------

hist(cuadro1$Diametro)
hist(DBH.media$Diametro)
hist(DBH.16$Diametro)

# Determinar la media y desv. estándar para la variable Altura ---------------------------------------------

mean(cuadro1$Altura)
sd(cuadro1$Altura)

mean(H.media$Altura)
sd(H.media$Altura)

mean(H.16$Altura)
sd(H.16$Altura)

# # Determinar la media y desv. estándar para la variable Vecinos --------------------------------------------

mean(cuadro1$Vecinos)
sd(cuadro1$Vecinos)

mean(Vecinos.3$Vecinos)
sd(Vecinos.3$Vecinos)

mean(Vecinos.4$Vecinos)
sd(Vecinos.4$Vecinos)

# Determinar la media y desv. estándar para la variable  Diámetro -------------------------------------------

mean(cuadro1$Diametro)
sd(cuadro1$Diametro)

mean(DBH.media$Diametro)
sd(DBH.media$Diametro)

mean(DBH.16$Diametro)
sd(DBH.16$Diametro)


