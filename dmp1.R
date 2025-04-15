# Load required libraries
install.packages("readr")
install.packages("GGally")
library(readr)
library(GGally)
library(dplyr)

# Load the dataset
toyota <- read.csv("ToyotaCorolla.csv")
View(toyota)
# Select relevant numeric columns for correlation matrix
numeric_vars <- toyota[, c("price", "age_08_04", "km", "hp", "weight", "cc")]

# Plot matrix
ggpairs(numeric_vars)

# Convert categorical variables into dummy variables
toyota_dummies <- toyota %>%
  mutate(
    Fuel_Type = as.factor(fuel_type),
    Metallic = as.factor(metallic_rim)
  ) %>%
# Create dummy variables
model.matrix(~ fuel_type + metallic_rim - 1, data = .) %>%
  as.data.frame() %>%
# Combine with original dataset (excluding original categorical vars)
bind_cols(toyota %>% select(-fuel_type, -metallic_rim), .)

# View the updated dataset
head(toyota_dummies)
