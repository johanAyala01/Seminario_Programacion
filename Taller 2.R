## primer punto bondad de ajuste
library('readxl')
datos <- read_xlsx("C:\\Users\\jsaj_\\Documents\\Maestria Inteligencia de negocios\\Segundo Semestre\\Aprendizaje estadistico\\Taller 2\\Libro1.xlsx")
alpha = 0.01
n=220
k=4
p = c(0.22, 0.21, 0.18, 0.39) # proporciones hipotéticas
f = table(datos)
round(f / n, 2)
e = n * p
conteos = cbind(f, e)
colnames(conteos) =  c("Frecuencia", "Esperado")

est.prueba = sum( (f - e)^2 / e )

# el valor calculado del estadístico de prueba es chi^2 = 33.85

# PASO 3
# establecer la región critica
percentil.chi = qchisq(p = alpha, df = k - 1, lower.tail = F)
# 7.814728
# La región critica esta compuesta de todos los valores SUPERIORES a 7.81.
# La región critica es RC = { x : x > 7.81 }

# calculo valor p (PASO 3)
valor.p = pchisq(q = est.prueba, df = k - 1, lower.tail = FALSE)

valor.p


prop.table(table(datos))*100

## punto 2 
datos <- read_xlsx("C:\\Users\\jsaj_\\Documents\\Maestria Inteligencia de negocios\\Segundo Semestre\\Aprendizaje estadistico\\Taller 2\\Libro2.xlsx")

alpha = 0.05
tabla = table(datos$pais, datos$puntuacion)
n  = sum(tabla)   # tamaño de la muestra
tabla.abs = table(datos$pais, datos$puntuacion)
tabla.rel = round(100 * tabla.abs / n, 2)

# 3 filas y 4 columnas con las FRECUENCIAS ABSOLUTAS


n  = sum(tabla)   # tamaño de la muestra
#n = length(ENFOQUE)
nf = nrow(tabla)  # 3 numero de filas
nc = ncol(tabla)  # 4 numero de columnas

f = tabla                   # frecuencias observadas

perfil.f = rowSums(tabla)   # perfil fila: totales por fila
perfil.c = colSums(tabla)   # perfil columna: totales por columna

tabla.f = matrix(data = rep(perfil.f, nc), nrow = nf, ncol = nc, byrow = FALSE)
tabla.c = matrix(data = rep(perfil.c, nf), nrow = nf, ncol = nc, byrow = TRUE)


e = tabla.f * tabla.c / n  # frecuencias esperadas

est.prueba = sum( (f - e)^2 / e )

percentil.chi = qchisq(p = alpha, df = (nf-1)*(nc-1), lower.tail = FALSE)


valor.p = pchisq(q = est.prueba, df = (nc - 1)*(nf - 1), lower.tail = FALSE)
valor.p

### punto 3
datos <- read_xlsx("C:\\Users\\jsaj_\\Documents\\Maestria Inteligencia de negocios\\Segundo Semestre\\Aprendizaje estadistico\\Taller 2\\Libro3.xlsx")
# normalidad
datos=data.frame(datos)
attach(datos)

shapiro.test(datos[tipo == "a",1])
shapiro.test(datos[tipo == 'b',1])
shapiro.test(datos[tipo == 'c',1])


#homocedasticidad

bartlett.test(comida ~ tipo, datos)

## independencia
library(lmtest)
dwtest(comida ~ tipo)
##anova

ANOVA1=aov(comida ~ tipo, datos)
summary(ANOVA1)


TukeyHSD(ANOVA1)


