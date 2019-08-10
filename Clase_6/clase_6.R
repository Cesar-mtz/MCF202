#César Martinez
#09/08/2019
#clase__6extra

#instalar libreria gapminder
#H0 linea de regresion no existe diferencias significativas
#H1 linea de regresion sí existe diferencias significativas


library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")
head(edad)
str(edad)

#identificar columna SP como factor

edad$SP <- factor(edad$SP)
str(edad)
#ya me lo identifico como 2 tratamientos


# SEPARAR POR FACTORES ----------------------------------------------------

ariz <-subset(edad, SP == "arizonica")

ariz.lm <- lm(ariz$EDAD ~ ariz$DAP)
summary(ariz.lm)
#aqui te saldra resultado diferente porque solo es un factor

dura <-subset(edad, SP == "durangensis")


# ver si las lineas de regresion son iguales dos factores------------------------------
cov.edad <- lm(edad$EDAD ~ edad$DAP + edad$SP)
summary(cov.edad)
#el valor de alfa es de la palabra "estimate"

plot(edad$DAP[edad$SP == "arizonica"], edad$EDAD[edad$SP == "arizonica"],
              col = "red", pch =16, xlim=c(0,50), ylim=c(0,130))
abline(cov.edad$coefficients[1], cov.edad$coefficients[2], col= "red")
text(30,20, "Ya = -7.65 + 1.98* x")


points(edad$DAP[edad$SP == "arizonica"], edad$EDAD[edad$SP == "durangensis"],
       col = "blue", pch =16, xlim=c(0,50), ylim=c(0,130))

abline(cov.edad$coefficients[1] + cov.edad$coefficients[3], 
       cov.edad$coefficients[2], col= "darkgreen", lty = "dashed")
text(19,100, "Yd = 11.41 + 1.98* x")

