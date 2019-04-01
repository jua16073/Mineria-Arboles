library(caret)
library(corrplot)
library(dummy)
library(plyr)
library(randomForest)
library(e1071)

setwd("D:/mineria/hoja3/Mineria-Arboles/")
entrenamiento = read.csv("train.csv") 
prueba = read.csv("test.csv")
entrenamiento1 <- data.frame(entrenamiento)
prueba1 <- data.frame(prueba)
total <- merge(entrenamiento1, prueba1, all = TRUE)
entrenamiento1$AdoptionSpeed <- as.factor(entrenamiento1$AdoptionSpeed)

entrenamiento1$Velocidad <- revalue(entrenamiento1$AdoptionSpeed, c("0" = "1", "1"= "1", "2"="1", "3"="0","4"="0")) 


porciento <- 0.7
set.seed(456)
trainRowsNumber <- sample(1:nrow(entrenamiento1), porciento*nrow(entrenamiento1))
train <- entrenamiento1[trainRowsNumber, ]
train<- train[complete.cases(train),]
test <- entrenamiento1[-trainRowsNumber, ]
test <- test[complete.cases(test),]
train1 <- within(train, rm("Name", "PetID", "Description", "RescuerID", "AdoptionSpeed"))

t <- sapply(train1,as.numeric)
correl1 <- cor(t, method = "spearman")
corrplot(correl1)

#Regresion Logistica
modelo <- glm(train1$Velocidad~., data = train1, family = binomial())
modelo
plot(modelo)
prediccion <- predict(modelo,newdata = test, type="response")
prediccion<-ifelse(prediccion>=0.5,0,1)
cfm <- confusionMatrix(as.factor(test$Velocidad),as.factor(prediccion))
cfm

#arbol

modeloRF1<-randomForest(train1$Velocidad~.,data=train1)
prediccionRF1<-predict(modeloRF1,newdata = test)
cfmRandomForest <- confusionMatrix(as.factor(test$Velocidad), as.factor(prediccionRF1))
cfmRandomForest

#Bayes
modeloBayes<-naiveBayes(train1$Velocidad~.,data=train1)
predBayes<-predict(modeloBayes, newdata = test)
cfm<-confusionMatrix(as.factor(test$Velocidad), as.factor(predBayes))
cfm
modelo

