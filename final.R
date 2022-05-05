library(readxl)
library(MASS)
library(mclust)  ## model-based clustering
library(fpc)     ## diagnosticos
library(factoextra)


base=read.csv("C:\\Users\\jsaj_\\Downloads\\FG2000_2017.csv",sep=';')
rownames(base)<-base$Company.Name
any(is.na(base))
nrow(base)  # numero de individuos
ncol(base) 
summary(base)
base[is.na(base)] <- 0


base <-base%>%mutate(Country=as.numeric(as.factor(base$Country)))


base=base%>%select('Country','Sales','Profits','Assets','Value')
pairs(base)

parcoord(base, cex.axis = 1.4, col = "red")


#dm.e  <- dist(x = base, method = "euclidean")
#round(as.matrix(dm.e)[1:5, 1:5], 2)
dm.m  <- dist(x = base, method = "manhattan")
round(as.matrix(dm.m)[1:5, 1:5], 2)

#dm.cp <- get_dist(x = base, method = "pearson", stand = TRUE)
#round(as.matrix(dm.cp)[1:5, 1:5], 3)


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
### eleccion de linkage average porque es el que da la mayor correlacion

plot(fit_aver, hang = -1, cex = 0.7)


#fviz_dend(x = fit_comp,
#          k = 6,
#          k_colors = 1:4,
#          color_labels_by_k = TRUE,
#          cex = 0.8,
#          type = "phylogenic",
#          repel = TRUE )

fviz_nbclust(x = base, FUNcluster = kmeans, 
             method = "wss", k.max = 50, nstart = 100)

### se toman 6 clusters

cluster <- cutree(fit_aver, k = 6)
table(cluster)

lista = c()
for (a in 1:2000){
  lista=c(lista,a)
  
}
  
### dividir por grupos
division=data.frame(rownames(base),cluster,row.names =lista)
agrupados=split(division,division$cluster)
agrupados



pc   <- princomp(base)  # componentes principales
# representacion usando componentes principales con 4 clusters
labs <- cutree(fit_aver, k = 6)
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(base), 
     col = labs, cex = 0.7)

fviz_cluster(list(data = base, cluster = cluster),
             ellipse.type = "convex") +
  labs(title = "Hierarchical clustering",
       subtitle = "Distancia euclídea, Lincage average, K=6") +
  theme(legend.position = "bottom") + theme_bw()

### funcion normalizada 

rge <- sapply(base, function(x) diff(range(x)))
# diff: Diferencias rezagadas
X   <- sweep(x = base, MARGIN = 2, STATS = rge, FUN = "/")
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

pc   <- princomp(base)             
# componentes principales
labs <- kmeans(X, centers = 6)$cluster  
# membresias (labels)

par(mfrow = c(1, 1))
# Gráficos
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(base), col = labs, cex = 0.7)
##############
fit_kmean = kmeans(x = X, centers = 6, nstart = 50)

fviz_cluster(list(data = base, cluster = fit_kmean$cluster),
             ellipse.type = "convex") +
  labs(title = "K-means clustering",
       subtitle = "K=6") +
  theme(legend.position = "bottom") + theme_bw()


## Dispersograma (prisma)

pairs(X, pch = 16, gap = 0, xaxt = "n", yaxt = "n", col = labs)


