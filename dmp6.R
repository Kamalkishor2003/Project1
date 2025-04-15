library(dplyr)
library(caret)
install.packages("tidyverse")
library(tidyverse)
Toyotacorolla <- read.csv("ToyotaCorolla.csv")
View(Toyotacorolla)
Toyotacorolla1 <- Toyotacorolla[,c(
  "price", "age_08_04", "km", "fuel_type", "hp", "automatic",
  "doors", "quarterly_tax","mfr_guarantee",
  "guarantee_period",
  "airco", "automatic_airco", "cd_player", "powered_windows",
  "sport_model", "tow_bar")]
#Converting Categorical Predictor to dummy variable using
#library fast dummies
install.packages("fastDummies")
library(fastDummies)
Toyotacorolla2 <- Toyotacorolla1 %>%
  dummy_cols(select_columns=c('fuel_type'))
#Removing original Fuel_Type and one of the dummy variables
#from the previous data frame
Toyotacorolla3 <- Toyotacorolla2 %>%
  dummy_cols(select_columns=c('fuel_type','fuel_type_CNG'))
#Removing NA values
Toyotacorolla3[is.na(Toyotacorolla3)]<-0

library(caret)
#Let's preprocess the data by scaling the numerical variables to a 0-1 scale using method="range"
data_normalize <- preProcess(Toyotacorolla3,
                             method=c("range"), na.remove=TRUE)
#The processed data is sent to predict() function to get the final normalized data using the min-max scaling method
data_normalize2 <-
  predict(data_normalize,as.data.frame(Toyotacorolla3))
#a. Fit a neural network model to the data. Use a single hidden layer with 2 nodes.
#Let's split the data into training (80%) and validation (20%)
ind <- sample(2, nrow(data_normalize2), replace=TRUE,
              prob=c(0.8, 0.2))
tdata <- data_normalize2[ind==1, ] #ind==1 means the firstsample
vdata<- data_normalize2[ind==2, ] #ind==2 means the second sample
#A. Fitting a neural network model to the data using a single hidden layer with 2 nodes.
#Plotting the neural network for training data
install.packages("fastDummies")
library(fastDummies)

# Convert categorical variables to dummies
tdata <- dummy_cols(tdata, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Ensure all columns are numeric
str(tdata)
install.packages("neuralnet")
library(neuralnet)
nn <- neuralnet(data = tdata, price ~., hidden=2)
plot(nn, rep="best")
#Calculating RMSE on training data
pred <- compute(nn, tdata)$net.result
install.packages("Metrics")
library(Metrics)
Error <- RMSE(tdata[, "price"], pred)
#We use rmse() function from the Metrics package
Error
#Plotting the neural network for validation data
vdata <- dummy_cols(vdata, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
nn <- neuralnet(data = vdata, price ~., hidden=2)
plot(nn, rep="best")
#Calculating RMSE on the validation data
pred <- compute(nn, vdata)$net.result
error <-RMSE(vdata[, "price"], pred) #We use rmse() function from the Metrics package
error

#B. Fitting a neural network model to the data using single layer with 5 nodes
#Plotting the neural network for training data
nn <- neuralnet(data = tdata, price ~., hidden=c(5)) #5 nodes, 1 layers
plot(nn, rep="best")
#Calculating RMSE on training data
pred <- compute(nn, tdata)$net.result
error <- RMSE(tdata[, "price"], pred) #We use rmse() function from the Metrics package
error
#Plotting the neural network for validation data
nn <- neuralnet(data = vdata, price ~., hidden=c(5)) # 5 nodes,1 layers
plot(nn,rep="best")
#Calculating RMSE on validation data
pred <- compute(nn, vdata)$net.result
error <- RMSE(vdata[, "price"], pred) #We use rmse() function from the Metrics package
error

#C. Fitting a neural network model to the data using two layer with 5 nodes in eack layer
#Plotting the neural network for training data
nn <- neuralnet(data = tdata, price ~., hidden=c(5,5)) #5 nodes, 2 layers
plot(nn, rep="best")
#Calculating RMSE on training data
pred <- compute(nn, tdata)$net.result
error <- RMSE(tdata[, "price"], pred) #We use rmse() function from the Metrics package
error
#Plotting the neural network for validation data
nn <- neuralnet(data = vdata, price ~., hidden=c(5,5)) # 5 nodes,2 layers
plot(nn,rep="best")
#Calculating RMSE on validation data
pred <- compute(nn, vdata)$net.result
error <- RMSE(vdata[, "price"], pred) #We use rmse() function from the Metrics package
error
#i. What happens to the RMS error for the training data as the number of layers and nodes increases?
#We can see from the above outputs that the root mean square
#error for the training data decreases as we increase the number of layers and nodes.
#ii. What happens to the RMS error for the validation data?
#We can see from the above outputs that the root mean square error for the validation data increases.
#iii. Comment on the appropriate number of layers and nodes for this application.
#From the above results, we can conclude that 2 layers and 5 nodes in each layer are appropriate for this application.


