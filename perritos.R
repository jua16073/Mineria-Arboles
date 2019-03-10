setwd("D:/mineria/hoja3/")
# X setwd
setwd("Documents/Mineria/Mineria Arboles/")
getwd()
# 

entrenamiento = read.csv("train.csv") 
prueba = read.csv("test.csv")
entrenamiento1 <- data.frame(entrenamiento)
prueba1 <- data.frame(prueba)
total <- merge(entrenamiento1, prueba1, all = TRUE)

head(total, n = 1)

library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(dendextend)
library(ggplot2)
library(dplyr)


datos <- total
# Seleccionando Varibales

Variables <- c("Age", "Quantity", "Fee", "VideoAmt", "PhotoAmt")
perrosVariables <- datos[Variables]


# Grafica de codo
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(perrosVariables, i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

# Kmeans
km <-kmeans(perrosVariables, 3)
plotcluster(perrosVariables, km$cluster)
fviz_cluster(km, data = perrosVariables,geom = "point", ellipse.type = "convex")

# Jerarquico
hc<-hclust(dist(perrosVariables)) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila

hc.cut<-hcut(perrosVariables, k=3, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")

# Cluster Fuzzy
fcm<-cmeans(perrosVariables,3)
perrosVariables$FCGrupos<-fcm$cluster
#perrosVariables<-cbind(total,fcm$membership)
perrosVariables$FCGrupos


# Silueta
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(perrosVariables))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal

#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(perrosVariables))
mean(silch[,3]) #0.51, no es la mejor partición pero no está mal

#Método de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(perrosVariables))
mean(silfcm[,3]) #0.54, no es la mejor partición pero no está mal
