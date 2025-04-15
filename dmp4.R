install.packages("FNN")
library(FNN)
housing.df <- read.csv("BostonHousing.csv")
set.seed(123)
train.index <- sample(row.names(housing.df),
                      0.6*dim(housing.df)[1])
valid.index <- setdiff(row.names(housing.df), train.index)
train.df <- housing.df[train.index, -14]
valid.df <- housing.df[valid.index, -14]

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
housing.norm.df <-housing.df
# use preProcess() from the caret package to normalize Income and Lot_Size.
install.packages("caret")
library(caret)
norm.values <- preProcess(train.df, method=c("center","scale"))
train.norm.df <- as.data.frame(predict(norm.values, train.df))
valid.norm.df <- as.data.frame(predict(norm.values, valid.df))
housing.norm.df <- as.data.frame(predict(norm.values,housing.df))

#initialize a data frame with two columns: k, and accuracy
accuracy.df <- data.frame(k = seq(1, 5, 1), RMSE = rep(0, 5))
# compute knn for different k on validation.
for(i in 1:5){
  knn.pred<-class::knn(train = train.norm.df[,-13],
                       test = valid.norm.df[,-13],
                       cl = train.df[,13], k = i)
  accuracy.df[i,2]<-
    RMSE(as.numeric(as.character(knn.pred)),valid.df[,13])
}
accuracy.df

Part B
#Predict the MEDV for a tract with the following information,using the best k:
new.df<- data.frame(
  CRIM = 0.2, ZN = 0, INDUS = 7, CHAS = 0, NOX = 0.538,
  RM = 6, AGE = 62, DIS = 4.7, RAD = 4, TAX = 307,
  PTRATIO = 21, B = 360, LSTAT = 10
)
new.norm.values <- preProcess(new.df, method=c("center","scale"))
new.norm.df <- predict(new.norm.values, newdata = new.df)
#predict the MEDV
new.knn.pred <-class::knn(train = train.norm.df[,-13],
                           test = new.norm.df,
                           cl =train.df$MEDV, k = 2)
new.knn.pred
new.accuracy.df<-RMSE(as.numeric(as.character(new.knn.pred)),valid.df[,13])
new.accuracy.df



    