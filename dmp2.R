library(readr)
library(dplyr)
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("zoo")
library(zoo)

# Load the dataset
data <- read.csv("ApplianceShipments.csv")
View(data)

# Convert Quarter to Date format
data$Date <- as.yearqtr(data$Quarter, format = "Q%q-%Y")
data$Year <- year(data$Date)
data$Qtr <- quarter(data$Date, with_year = FALSE)

# i. Time Plot
ggplot(data, aes(x = Date, y = Shipments)) +
  geom_line(color = "steelblue") +
  labs(title = "Quarterly Appliance Shipments", y = "Shipments ($M)", x = "Time")

# ii. Zoomed plot
ggplot(data, aes(x = Date, y = Shipments)) +
  geom_line(color = "tomato") +
  coord_cartesian(ylim = c(3500, 5000)) +
  geom_label(aes(label=Quarter,color =Quarter),show.legend = FALSE)+ 
  labs(title = "Zoomed Quarterly Appliance Shipments", y = "Shipments ($M)", x = "Time")

# iii. Separate lines by Quarter
ggplot(data, aes(x = Year, y = Shipments, color = as.factor(Qtr))) +
  geom_line() +
  facet_wrap(~Qtr, ncol = 1) +
  coord_cartesian(ylim = c(3500, 5000)) +
  labs(title = "Shipments by Quarter", y = "Shipments ($M)", color = "Quarter")

# iv. Yearly Aggregated Line Graph
yearly_data <- data %>%
  group_by(Year) %>%
  summarise(YearlyTotal = sum(Shipments))

ggplot(yearly_data, aes(x = Year, y = YearlyTotal)) +
  geom_line(color = "darkgreen") +
  labs(title = "Yearly Total Appliance Shipments", y = "Total Shipments ($M)")
