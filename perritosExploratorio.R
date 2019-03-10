library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(ggplot2)
library(dplyr)
library(fitdistrplus)

setwd("D:/mineria/hoja3/")
entrenamiento = read.csv("train.csv") 
prueba = read.csv("test.csv")
entrenamiento1 <- data.frame(entrenamiento)
prueba1 <- data.frame(prueba)
total <- merge(entrenamiento1, prueba1, all = TRUE)
total <- within(total,rm("Description"))

#resumen de los datos del dataset
summary(total)

#Analizando edad del dataset
fit.norm <- fitdist(total$Age, "norm")
plot(fit.norm)
mean(total$Age)
sd(total$Age, na.rm = TRUE)

#correlacion entre edad y velocidad de adopcion

cor(entrenamiento$AdoptionSpeed, entrenamiento$Age, method = c("pearson"))
hist(entrenamiento$Age, breaks = 20)
axis(1, at=seq(0 , 150, by=10))

#analizando raza principal del dataset

tablaRazas <- table(total$Breed1)
frecuenciasRazas = as.data.frame(tablaRazas)
head(frecuenciasRazas[order(frecuenciasRazas$Freq, decreasing = TRUE),],10)

#analizando sexo del animal

tablaSexo <- table(total$Gender)
frecuenciasSexo = as.data.frame(tablaSexo)
head(frecuenciasSexo[order(frecuenciasSexo$Freq, decreasing = TRUE),],3)

#analizando colores indenpedientes del animal

tablaColor1 <- table(total$Color1)
frecuenciaColor1 = as.data.frame(tablaColor1)
head(frecuenciaColor1[order(frecuenciaColor1$Freq,decreasing = TRUE),],7)

tablaColor3 <- table(total$Color3)
frecuenciaColor3 = as.data.frame(tablaColor3)
head(frecuenciaColor3[order(frecuenciaColor3$Freq,decreasing = TRUE),],7)

#variable de tamanio del animal

tablaTamanio <- table(total$MaturitySize)
frecuenciaTamanio = as.data.frame(tablaTamanio)
head(frecuenciaTamanio[order(frecuenciaTamanio$Freq, decreasing = TRUE),], 4)

#variable largo de pelo

tablaPelo <- table(total$FurLength)
frecuenciaPelo = as.data.frame(tablaPelo)
head(frecuenciaPelo[order(frecuenciaPelo$Freq, decreasing = TRUE),],7)

#variable vacunacion

tablaVacunas <- table(total$Vaccinated)
frecuenciaVacunas = as.data.frame(tablaVacunas)
head(frecuenciaVacunas[order(frecuenciaVacunas$Freq,decreasing = TRUE),], 4)

#variable desparasitado

tablaDesparasitado <- table(total$Dewormed)
frecuenciaDesparasitado = as.data.frame(tablaDesparasitado)
head(frecuenciaDesparasitado[order(frecuenciaDesparasitado$Freq, decreasing = TRUE),],3)

#variable esterelizado

tablaEsteril <- table(total$Sterilized)
frecuenciaEsteril = as.data.frame(tablaEsteril)
head(frecuenciaEsteril[order(frecuenciaEsteril$Freq, decreasing = TRUE),],3)

#variable salud

tablaSalud <- table(total$Health)
frecuenciaTabla = as.data.frame(tablaSalud)
head(frecuenciaTabla[order(frecuenciaTabla$Freq, decreasing = TRUE),],3)

#variable cantidad
fit <- fitdist(total$Quantity, "norm")
plot(fit)
mean(total$Quantity)
sd(total$Quantity, na.rm = TRUE)

#correlacion entre cantidad de animales y velocidad de adopcion

cor(entrenamiento$AdoptionSpeed, entrenamiento$Quantity, method = c("spearman"))
hist(entrenamiento$Quantity, breaks = 50)
axis(1, at=seq(0 , 10, by=1))

#variable fee
fit1 <- fitdist(total$Fee, "norm")
plot(fit1)
mean(total$Fee)
sd(total$Fee, na.rm = TRUE)

#correlacion entre cantidad de animales y velocidad de adopcion

cor(entrenamiento$AdoptionSpeed, entrenamiento$Fee, method = c("spearman"))
hist(entrenamiento$Fee, breaks = 50)
axis(1, at=seq(0 , 1000, by=100))
cor(entrenamiento$Age, entrenamiento$Fee, method = c("spearman"))
cor(entrenamiento$Quantity, entrenamiento$Fee, method = c("spearman"))

#variable estado

tablaEstado <- table(total$State)
frecuenciaEstado = as.data.frame(tablaEstado)
head(frecuenciaEstado[order(frecuenciaEstado$Freq, decreasing = TRUE),],10)

#ID DEL rescatista

tablaResc <- table(total$RescuerID)
frecuenciaResc = as.data.frame(tablaResc)
head(frecuenciaResc[order(frecuenciaResc$Freq,decreasing = TRUE),],10)

#videos del animal
fit3 <- fitdist(total$VideoAmt, "norm")
plot(fit3)
mean(total$VideoAmt)
sd(total$VideoAmt, na.rm = TRUE)



cor(entrenamiento$AdoptionSpeed, entrenamiento$VideoAmt, method = c("spearman"))
hist(entrenamiento$VideoAmt, breaks = 50)
axis(1, at=seq(0 , 1000, by=100))
cor(entrenamiento$Age, entrenamiento$VideoAmt, method = c("spearman"))
cor(entrenamiento$Quantity, entrenamiento$VideoAmt, method = c("spearman"))

#fotos del animal
fotos <- data.frame(total$PhotoAmt)
fotos <- fotos[!is.na(fotos)]
fit3 <- fitdist(as.integer(fotos), "norm")
plot(fit3)
mean(fotos)
sd(fotos, na.rm = TRUE)

cor(entrenamiento$AdoptionSpeed, entrenamiento$PhotoAmt, method = c("spearman"))
hist(entrenamiento$PhotoAmt, breaks = 50)
axis(1, at=seq(0 , 1000, by=100))
cor(entrenamiento$Age, entrenamiento$PhotoAmt, method = c("spearman"))
cor(entrenamiento$Quantity, entrenamiento$PhotoAmt, method = c("spearman"))
