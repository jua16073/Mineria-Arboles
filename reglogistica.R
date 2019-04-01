library(caret)
library(corrplot)
library(dummy)
library(plyr)

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
train1 <- within(train, rm("Name", "PetID", "Description", "RescuerID"))
modelo <- glm(train$Velocidad~Age+Breed1, data = train1, family = binomial())
modelo
prediccion <- predict(modelo,newdata = test, type="response")
as.factor(prediccion)
prediccion<-ifelse(prediccion>=0.5,1,0)
cfm <- confusionMatrix(as.factor(test$Velocidad),as.factor(prediccion))
cfm
