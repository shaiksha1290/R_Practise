library(caret)
library(ggplot2)
data(faithful)

train_d <- createDataPartition(y = faithful$waiting,p = 0.7,list=FALSE)

g <- faithful[train_d,];x <- faithful[-train_d,]
head(g)

prcomp(x = g)
preProcess(x = log10(g),method = 'pca',pcaComp = 2)
train(g$waiting ~ . , method = "glm" , preProcess = "pca" , data = g)

linear_m <- lm(g$eruptions ~ g$waiting,data =g)

linear_m$coefficients

ggplot(aes(x = g$waiting,y=g$eruptions),data = g) +
  geom_point() +
  geom_line(x = g$waiting,y = predict(linear_m,data = g),color="red")

#Clac RMSE

sqrt(sum(linear_m$fitted.values - g$eruptions)^2)


#lm on ltiple covariates
library(ISLR)
data("Wage")

wage_a <- createDataPartition(Wage$wage,p=0.7,list =FALSE)
wage_train <- Wage[wage_a,]
wage_test <- Wage[-wage_a,]

featurePlot(x = wage_train[,c("education","age","jobclass")],
            y = wage_train$wage,
            plot = "pairs")

qplot(x=wage_train$age,y=wage_train$wage,color = wage_train$education)
 


#Bagging
library(ElemStatLearn)
data("ozone",package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

l <- matrix(NA,nrow = 10,ncol = 155)
 
for (var in 1:10) {
  ss <- sample(1:dim(ozone)[1],replace = T)
  ozone0 <- ozone[ss,];ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temparatue ~ ozone, data = ozone0,span=0.2)
  l[i,] <- predict(loess0,newdata = data.frame(ozone = 1:155))
  
}
