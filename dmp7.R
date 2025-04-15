# Load required libraries
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# Load the dataset
data <- read.transactions("Coursetopics.csv", format = "basket", sep = ",")

summary(data)
itemFrequencyPlot(data, topN = 10, type = "absolute", main = "Top 10 Course Topics")

rules <- apriori(data, 
                 parameter = list(supp = 0.05, conf = 0.6, minlen = 2))

# Basic visualization
plot(rules, method = "graph", engine = "htmlwidget")

plot(rules, method = "grouped")
plot(rules, method = "matrix", measure = c("support", "confidence"))

