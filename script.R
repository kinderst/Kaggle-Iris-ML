#Iris dataset

allData <- read.csv("Iris.csv")

set.seed(1)
train=sample(1:nrow(allData),75)
attach(allData)
allData$Species=as.factor(allData$Species)

#LDA
library(MASS)
lda.fit=lda(Species~SepalLengthCm+SepalWidthCm+PetalLengthCm+PetalWidthCm,data=allData,subset=train)
lda.fit
lda.pred=predict(lda.fit,allData$Species)
lda.class=lda.pred$class
table(lda.class,allData$Species)
mean(lda.class==allData$Species)

#KNN
library(class)
train.X=cbind(SepalLengthCm,SepalWidthCm,PetalLengthCm,PetalWidthCm)[train,]
test.X=cbind(SepalLengthCm,SepalWidthCm,PetalLengthCm,PetalWidthCm)[-train,]
train.Species=allData$Species[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Species,k=3)
table(knn.pred,allData[-train,]$Species)
mean(knn.pred==allData[-train,]$Species)

