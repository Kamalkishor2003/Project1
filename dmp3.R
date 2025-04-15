# Load the dataset
ToyotaCorolla <- read.csv("ToyotaCorolla.csv")
View(ToyotaCorolla)
# i. Identify the categorical variables
# Assuming categorical variables are those with character or factor data type

ToyotaCorolla$Fuel_Type <- factor(ToyotaCorolla$fuel_type)
unique(ToyotaCorolla$fuel_type)
categorical_vars <- sapply(ToyotaCorolla, is.factor)
categorical_vars

# ii. Explain the relationship between a categorical variable and
the series of binary dummy variables derived from it
##A categorical variable represents different categories or
groups, such as colors, types of cars, or levels of education
##Dummy variables are binary variables created to represent
the categories of a categorical variable.
##Each dummy variable corresponds to one category of the
categorical variable.
# Dummy variables are binary variables created to represent
the categories of a categorical variable.
# Each dummy variable corresponds to one category of the
categorical variable, with a value of 1 indicating presence and 0
indicating absence.
# iii. How many dummy binary variables are required to capture
the information in a categorical variable with N categories?
  # For a categorical variable with N categories, N-1 dummy
  variables are needed. This is to avoid perfect multicollinearity.
# iv. Convert categorical variables into dummy binaries

ToyotaCorolla$Fuel_Type <- factor(ToyotaCorolla$fuel_type)

# Create dummy variables for Fuel_Type
dummy_vars <- model.matrix(~ fuel_type - 1, data =ToyotaCorolla)
# Attach the dummy variables to the dataset
ToyotaCorolla <- cbind(ToyotaCorolla, dummy_vars)

# Optionally, you can remove the original Fuel_Type column if it's no longer needed
ToyotaCorolla <- ToyotaCorolla[, !(names(ToyotaCorolla) %in% "fuel_type")]

View(ToyotaCorolla)










