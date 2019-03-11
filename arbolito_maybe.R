setwd("Documents/Mineria/Mineria Arboles/")
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

#Breed1
#Type
#MaturitySize
#FurLength
#Color1
#Dewormed+Vaccinated+Fee+State
dt_model <- rpart(AdoptionSpeed~Age+Color1, train, method="Class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)
