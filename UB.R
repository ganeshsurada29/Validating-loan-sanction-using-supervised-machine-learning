UBraw <- read.csv("UniversalBank.csv")
#Loaded into dataframe UBrawand dropped the columns ZipCode and USERID
UBraw
UBraw <- UBraw[,-1]
UBraw <- UBraw[,-4]

#Initializing the three dummy variables to zero
UBraw$Education_1 <- 0
UBraw$Education_2 <- 0
UBraw$Education_3 <- 0

#Initializing the respective dummy variable to 1
for (i in 1:nrow(UBraw)){
  if(UBraw$Education[i]==1){
    UBraw$Education_1[i] <- 1
  }
  if(UBraw$Education[i]==2){
    UBraw$Education_2[i] <- 1
  }
  if(UBraw$Education[i]==3){
    UBraw$Education_3[i] <- 1
  }
  
}

#dropping the education dummy variable
UBraw <- UBraw[,-6]

set.seed(40)
UBrawtraindata <- sample(row.names(UBraw), 0.6*dim(UBraw)[1])
UBrawvaliddata <- setdiff(row.names(UBraw), UBrawtraindata)

UBtrain <- UBraw[UBrawtraindata,]
UBvalid <- UBraw[UBrawvaliddata,]

#To initialise the UB train and valid normalized data set
UBtrain.norm <- UBtrain
UBvalid.norm <- UBvalid
UBraw.norm <- UBraw

#Using the preProcess function, the data is normalized and the corresponding training and validation normalized datasets are created.
library(caret)
norm <- preProcess(UBtrain[, -7], method=c("center", "scale"))
UBtrain.norm[, -7] <- predict(norm, UBtrain[, -7])
UBvalid.norm[, -7] <- predict(norm, UBvalid[, -7])
UBraw.norm[, -7] <- predict(norm, UBraw[, -7])
UBtest.norm <- predict(norm, UBtest)

#Applying K-NN algorithm
library(FNN)
nn <- knn(train = UBtrain.norm[, -7], test = UBtest.norm, cl = UBtrain.norm[, 7], k = 1)
nn
row.names(UBtrain)[attr(nn, "nn.index")]

library(caret)
#We create an empty accuracy matrix 
accuracy <- data.frame(k = seq(1, 100, 1), accuracy = rep(0, 100))

# compute knn for different k and accuracy on validation.
for(i in 1:100) {
  knn.pred <- knn(UBtrain.norm[, -7], UBvalid.norm[, -7],
                  cl = UBtrain.norm[, 7], k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, UBvalid.norm[, 7])$overall[1]
}
accuracy

#nn determines the value of k used in the model
nn <- knn(train = UBtrain.norm[, -7], test = UBtest.norm, cl = UBtrain.norm[, 7], k = 3)
nn
confusionMatrix(knn.pred, UBvalid.norm[, 7])$overall[1]


#Naive bayes algorithm
library(rpivotTable)
rpivotTable(UBrawtrain, rows = c("CreditCard","Personal.Loan"), cols = "Online")

rpivotTable(UBrawtrain, rows = c("Personal.Loan"), cols = "Online")
rpivotTable(UBrawtrain, rows = c("Personal.Loan"), cols = "CreditCard")

nb <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = UBrawtrain)
nb

