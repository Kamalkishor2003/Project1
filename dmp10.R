
#Create a well-formatted time plot of the data
# Load necessary libraries
library(ggplot2)
library(readr)
library(lubridate)

# Read data
data <- read.csv("SouvenirSales.csv")
View(data)
library(zoo)
colnames(data)[1] <- "Month"
head(data$Month)
data$Month <- as.yearmon(data$Month, format = "%b-%y")
data$Month <- as.Date(data$Month)

# Time plot of original data
ggplot(data, aes(x = Month, y = Sales)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Souvenir Sales (1995–2001)",
       x = "Month", y = "Sales") +
  theme_minimal()

#ii. Apply log scale to achieve linearity
# Time plot with log(y-axis)
ggplot(data, aes(x = Month, y = log(Sales))) +
  geom_line(color = "darkgreen") +
  labs(title = "Log-Transformed Monthly Souvenir Sales",
       x = "Month", y = "Log(Sales)") +
  theme_minimal()

#iii. Type of trend in the data?
#The original time plot shows a non-linear, exponential upward trend.
#The log-transformed plot reveals a linear trend.

#iv. Why were the data partitioned?
#Purpose of partitioning:
#To train the model on historical data and test its forecasting performance on unseen data (validation set).
#Prevents overfitting and ensures that forecasts generalize well.

#Partitioning the data (1995–2000: training, 2001: validation)
# Split data into training and validation sets
training <- subset(data, year(Month) < 2001)
validation <- subset(data, year(Month) == 2001)

# Check sizes
nrow(training)  # Should be 72 months
nrow(validation)  # Should be 12 months
