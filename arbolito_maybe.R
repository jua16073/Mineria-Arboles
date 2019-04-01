setwd("D:/mineria/hoja3/Mineria-Arboles/")
getwd()

entrenamiento = read.csv("train.csv") 
prueba = read.csv("test.csv")
entrenamiento1 <- data.frame(entrenamiento)
prueba1 <- data.frame(prueba)
total <- merge(entrenamiento1, prueba1, all = TRUE)

library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

#porcentaje para el test
porciento <- 70/100

# Seed para que el test sea el mismo siempre
set.seed(456)

trainRowsNumber <- sample(1:nrow(entrenamiento1), porciento*nrow(entrenamiento1))
train <- entrenamiento1[trainRowsNumber, ]
test <- entrenamiento1[-trainRowsNumber, ]

head(entrenamiento1, n = 1)

# Con Random Forest
modeloRF1<-randomForest(AdoptionSpeed~State+Breed1+Age+Dewormed+Color1+Health,data=train)
prediccionRF1<-predict(modeloRF1,newdata = test[c("State","Breed1","Age","Vaccinated","Dewormed","Color1","Health")])
testCompleto<-test
testCompleto$predRF<-prediccionRF1
testCompleto$predRF
testCompleto$predRF <- as.factor(round(testCompleto$predRF, 0))
testCompleto$AdoptionSpeed <- as.factor(round(testCompleto$AdoptionSpeed, 0))
cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$AdoptionSpeed)
cfmRandomForest

