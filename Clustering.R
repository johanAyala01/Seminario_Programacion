head(USArrests)

n <- nrow(USArrests)  # numero de individuos
p <- ncol(USArrests)  # numero de variables
# ¿Toda la tabla esta completa?
any(is.na(USArrests) )

### librerias

library(MASS)
library(mclust)  ## model-based clustering
library(fpc)     ## diagnosticos

## Estadística descriptiva

# algunas medidas estadísticas
summary(USArrests)

# prisma de dispersogramas
pairs(USArrests, pch = 16, col = "red", 
      gap = 0, xaxt = "n", yaxt = "n")

# Escalar / estandarizar los datos usando la escala de función R:
# (xi-mean(x))/sd(x)
# USArrests = scale(USArrests)

### Parallel coordinate plot

parcoord(USArrests, cex.axis = 1.4, col = "red")


dm.e  <- dist(x = USArrests, method = "euclidean")
round(as.matrix(dm.e)[1:5, 1:5], 2)
dm.m  <- dist(x = USArrests, method = "manhattan")
round(as.matrix(dm.m)[1:5, 1:5], 2)
library(factoextra)
dm.cp <- get_dist(x = USArrests, method = "pearson", stand = TRUE)
round(as.matrix(dm.cp)[1:5, 1:5], 3)

fviz_dist(dm.cp, 
          gradient = list(low = "green", mid = "red", high = "blue"))
# hclust : agrupamiento jerárquico
# Describe el árbol producido por el proceso de   agrupación.
fit_sing <- hclust(d = dm.m, method = "single")
fit_aver <- hclust(d = dm.m, method = "average") 
fit_comp <- hclust(d = dm.m, method = "complete")
fit_ward <- hclust(d = dm.m, method = "ward.D")
cor.s = cor(x = dm.m, cophenetic(fit_sing))
cor.a = cor(x = dm.m, cophenetic(fit_aver))
cor.c = cor(x = dm.m, cophenetic(fit_comp))
cor.w = cor(x = dm.m, cophenetic(fit_ward))
Resul = c(cor.s, cor.a, cor.c, cor.w)
names(Resul) = c("single", "average", "complete", "ward.D")
Resul



plot(fit_aver, hang = -1, cex = 0.7)

# dendrograma con ggplot
fviz_dend(x = fit_comp, k = 4, cex = 0.5) +
  geom_hline(yintercept = 3.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=4")

# dendrograma arbol
library("igraph")
fviz_dend(x = fit_comp,
          k = 4,
          k_colors = 1:4,
          color_labels_by_k = TRUE,
          cex = 0.8,
          type = "phylogenic",
          repel = TRUE )
# Repel: evitar la superposición de etiquetas de texto o no

cluster <- cutree(fit_aver, k = 4)
table(cluster)


fviz_nbclust(x = USArrests, FUNcluster = kmeans, 
             method = "wss", k.max = 15, nstart = 50)
# "wss": para el total dentro de la suma del cuadrado


pc   <- princomp(USArrests)  # componentes principales
# representacion usando componentes principales con 4 clusters
labs <- cutree(fit_aver, k = 4)
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(USArrests), 
     col = labs, cex = 0.7)
########
fviz_cluster(list(data = USArrests, cluster = cluster),
             ellipse.type = "convex") +
  labs(title = "Hierarchical clustering",
       subtitle = "Distancia euclídea, Lincage average, K=4") +
  theme(legend.position = "bottom") + theme_bw()


# Clustering de K medias


# estandarizacion
rge <- sapply(USArrests, function(x) diff(range(x)))
# diff: Diferencias rezagadas
X   <- sweep(x = USArrests, MARGIN = 2, STATS = rge, FUN = "/")
# sweep: Devuelve una matriz obtenida de una matriz 
# de entrada mediante el barrido de una estadística de resumen
sapply(X, var)


pctp   <- 0
within <- kmeans(x = X, centers = 1)$totss
for (k in 2:8) {
  clk    <- kmeans(X, centers = k)     
  # ajustar clustering de k medias
  wi     <- sum(clk$withinss) # WGSS
  #withinss: Vector de suma de cuadrados dentro de un grupo, un componente por grupo.
  pcte   <- clk$betweenss / clk$totss
  # betweenss: La suma de cuadrados entre grupos
  pctp   <- c(pctp, pcte)
  within <- c(within, wi)
  # % de variabilidad explicado
}

par(mfrow = c(1, 2))
plot(pctp, type = "b", pch = 16, cex = 2, cex.lab = 1.1, lwd = 2, ylim = c(0,1),
     col = "blue",xlab = "K", ylab = "% variabilidad explicado")
plot(within, type = "b", pch = 16, cex = 2, cex.lab = 1.1, lwd = 2,
     col = "red",xlab = "K", ylab = "WGSS")

### INTERPRETACION

pc   <- princomp(USArrests)             
# componentes principales
labs <- kmeans(X, centers = 4)$cluster  
# membresias (labels)

# Gráficos
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(USArrests), col = labs, cex = 0.7)
##############
fit_kmean = kmeans(x = X, centers = 5, nstart = 50)

fviz_cluster(list(data = USArrests, cluster = fit_kmean$cluster),
             ellipse.type = "convex") +
  labs(title = "K-means clustering",
       subtitle = "K=5") +
  theme(legend.position = "bottom") + theme_bw()


## Dispersograma (prisma)

pairs(X, pch = 16, gap = 0, xaxt = "n", yaxt = "n", col = labs)




