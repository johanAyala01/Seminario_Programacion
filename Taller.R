#• Haga un análisis descriptivo del conjunto de datos.

library(ggplot2)
library(dplyr)
library(MASS)
library(readxl)
library(psych)

setwd("~/Maestria Inteligencia de negocios/Segundo Semestre/Aprendizaje estadistico/Taller 3")
Datos <- read_excel("Lactancia.xlsx")
Prop_Lacta=Datos %>% group_by(Edad,AP) %>% summarise(N=n(),Lacta=sum(Lactancia==1))
Prop_Lacta$Propo=Prop_Lacta$Lacta/Prop_Lacta$N
qplot(Edad,Propo,col=factor(AP),data=Prop_Lacta)


attach(Datos)

summary(Datos)
describe(Datos)


tab <- xtabs(formula = ~ Lactancia + AP, data = Datos) # Revisar balanceo de clases
tab.rel <- round( prop.table(x = tab, margin = 1), 3)

print( tab ) 
print( tab.rel )


par(mfrow = c(1, 2))
boxplot(Edad ~ Lactancia, data = Datos, col = c("red","blue"),
        main = "Edad por Lactancia", xlab = "Admisión")
boxplot(AP ~ Lactancia, data = Datos, col = c("red","blue"),
        main = "PA por Lactancia", xlab = "Admisión")

  #• Proponga un modelo estadístico para explicar la probabilidad de que un niño esté lactando en función
#de su edad (en días) y del modelo de atención primaria (MF/MS). Describa las componentes aleatoria
#y sistemática.(respuesta y explicativas)



model1 <- glm(Lactancia ~ Edad+AP , data = Datos, family = "binomial")

summary(model1)
with(data = model1, 
     expr = pchisq(q = null.deviance - deviance, df = df.null - df.residual, 
                   lower.tail = F))



#• Estime e INTERPRETE los parámetros del modelo propuesto.

round(x = confint(model1), digits = 5)
round(x = exp( cbind( coef(model1), confint(model1) ) ), digits = 5)
#• ¿La muestra proporciona evidencia estadísticamente significativa de que la probabilidad que un niño
#esté lactando depende de su edad? Justifique su respuesta.

#• ¿La muestra proporciona evidencia estadísticamente significativa de que la probabilidad que un niño
#esté lactando depende del modelo de atención primaria (MF/MS)? Justifique su respuesta.

#• ¿El efecto de la edad del niño sobre la probabilidad de que esté lactando depende del modelo de atención
#primaria? Justifique su respuesta.
library(mfx)
logitmfx(formula = Lactancia ~ AP + Edad , data = Datos)

#• Estime la probabilidad de que un niño de 150 días de nacido y que está bajo el modelo de atención
#primaria MF esté lactando.

D <- with(data = Datos, 
          expr = data.frame(Edad = 150.000000, AP = 0,Lactancia=1))

print( D )
D$prediction <- predict(model1, newdata = D, type = "response")
print( D )

#• Haga un gráfico del modelo estimado junto con los datos. Comente.
Datos$prediction <- predict(model1, newdata = Datos, type = "response")

ggplot(Datos, aes(x = Edad, y = Lactancia, color=AP)) +
  geom_point()+
  geom_point(data = Datos, aes(y = Datos$prediction))

