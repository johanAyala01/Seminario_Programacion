## carga de la base de datos

install.packages("devtools")
devtools::install_github("nebulae-co/saber")
install.packages("esquisse")

## Activar las librerias 

library("tidyverse")
library("esquisse")
library("knitr")
library("magrittr")
library("saber")

library(corrplot)

library(RColorBrewer)
library(Hmisc)
library(car)
library(psych)
library(ggplot2)
library(scales)


### abrir las bases de datos
data("SB11_20061")
datos = SB11_20061
head(datos)



### contar columnas vacias
apply(X = is.na(datos), MARGIN = 2, FUN = sum)

### caracteristicas generales 

#1
p = ncol(datos)
print(p)

## 3
a=sapply(X = datos, FUN = class)
f=0
g=0
for (e in 1:length(a)) { if (a[e]=='integer'|a[e]=='numeric'){f=f+1} else {g=g+1}}


## 5
d=datos %>% select(ESTU_RESIDE_DEPT_PRESENTACION, FISICA_PUNT) %>% 
  arrange(desc(FISICA_PUNT))
head(d,2)
## 7

d=datos %>% select(ESTU_RESIDE_DEPT_PRESENTACION, FILOSOFIA_PUNT) %>% 
  arrange(desc(FILOSOFIA_PUNT))
head(d,1)

## 9
datos <- mutate_at(datos, c("MATEMATICAS_PUNT"), ~replace(., is.na(.), 0))
mean(datos$MATEMATICAS_PUNT)

##11

datos <- mutate_at(datos, c("ECON_SN_COMPUTADOR"), ~replace(., is.na(.), 0))
d=table(datos$ECON_SN_COMPUTADOR)
(1-d[1]/dim(datos)[1])*100
### 2.5%

##12

datos <- mutate_at(datos, c("ESTU_TRABAJA"), ~replace(., is.na(.), 0))
d=table(datos$ESTU_TRABAJA)
(1-d[1]/dim(datos)[1])*100
##1.3%

## 13

d=table(datos$COLE_NATURALEZA)
(d[2]/dim(datos)[1])*100
## 59.12984 

###14


p10.value = quantile(x = datos$ESTU_EDAD, probs = 0.11)
p10.value
## 16
datos <- mutate_at(datos, c("ESTU_RESIDE_MPIO_PRESENTACION"), ~replace(., is.na(.), 0))

sort(table(datos$ESTU_RESIDE_MPIO_PRESENTACION))
### cali 19545
### Pasto 4991
## Bogotá 4421
## Palmira 2855
## Buenaventura 2263

## 15

p15.value = quantile(x = datos$ESTU_EDAD, probs = 0.14)
p15.value


## 17 
hist(datos$MATEMATICAS_PUNT, main = "Histograma del puntaje de lenguaje ", freq = FALSE, 
col = heat.colors(7)); rug(MATEMATICAS_PUNT, col = "red")


##18
datos <- mutate_at(datos, c("ESTU_ESTRATO"), ~replace(., is.na(.), 0))
a=table(datos%>%select(ESTU_ESTRATO)%>%filter(ESTU_ESTRATO >0))
barplot(a, col = brewer.pal(3, "Set3"), main = "Barra de Medias",
        density = 60, angle = 90, border = "blue", lwd = 1, las = 2)

### 19

boxplot(datos$ESTU_EDAD)

##20
as<-datos%>%select(LENGUAJE_PUNT,MATEMATICAS_PUNT,CIENCIAS_SOCIALES_PUNT,FILOSOFIA_PUNT,BIOLOGIA_PUNT,
                     QUIMICA_PUNT,FISICA_PUNT)
correlacion<-round(cor(as), 1)

corrplot(correlacion, method="number", type="upper")

##21

pairs(LENGUAJE_PUNT ~ MATEMATICAS_PUNT + CIENCIAS_SOCIALES_PUNT + FILOSOFIA_PUNT+BIOLOGIA_PUNT+
        QUIMICA_PUNT+FISICA_PUNT, 
      data=datos, 
      main="Matriz de dispersion por puntajes en áreas del conocimiento")

