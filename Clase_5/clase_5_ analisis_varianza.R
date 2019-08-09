# César A. Martínez Gauna 
# 09/08/2019
# Clase_5 "Análisis de Varianza"

#proviene del ANOVA.- comparar el efecto del tratamiento en mínimo 3 grupos

#no existe diferencia entre las medias de los tratamientos
#si existe diferencia en al menos una de las medias de los tratamientos

arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10,13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)

y.ton <- c(arena, arcilla, limo)
suelo <-gl(3, 10, 30, labels=c("arena", "arcilla", "limo"))

prod <- data.frame(suelo, y.ton)
head(prod)

tapply(prod$y.ton, prod$suelo, mean)
tapply(prod$y.ton, prod$suelo, var)

shapiro.test(prod$y.ton)
bartlett.test(prod$y.ton, prod$suelo)
#determinar homogeneidad entre varianza aunque haya salido el doble en mean son homogeneo

fligner.test(prod$y.ton, prod$suelo)
#determinar homogeneidad entre varianza

#Ya comprobada la homogeneidad se procede al boxplot (inspeccion visual de datos)

boxplot(prod$y.ton ~ prod$suelo, xlab = "Tipo de suelo", 
       ylab = "Ton/ha", col = "green")

aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)

par(mfrow=c(2,2))
plot(aov(prod$y.ton ~ prod$suelo))
par(mfrow=c(1,1))
#normal QQ en la grafica representa los valores residuales si son muy separados no se puede, juntos son normales

TukeyHSD(aov.suelo, conf.level = 0.95)
#cuando en arcilla-arena lwr hacia upr cruza a 0 o sea -2.19 a 5.39 no hay diferencia sig


plot(TukeyHSD(aov.suelo))
summary(aov.suelo)
summary.lm(aov.suelo)
#este ultimo es compracion para el modelo lineal.


