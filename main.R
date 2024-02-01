library(tidyverse)
library(readxl)
library(ggplot2)
library(caret)
library(randomForest)

readData <- function(){
  read_excel("./1657875746_day.xlsx")
}

data <- readData()

#Date formating
data$dteday <- as.Date(data$dteday)
data$month <- format(data$dteday, "%m")
data$year <- format(data$dteday, "%Y")

#aggragating by month
monthly_counts <- aggregate(cnt ~ year + month, data, sum)

#ploting
ggplot(monthly_counts, aes(x = as.factor(month), y = cnt, group = year, color = factor(year))) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Distribution of Total Bikes Rented",
       x = "Month",
       y = "Total Bikes Rented") +
  theme_minimal()


yearly_counts <- aggregate(cnt ~ year, data, sum)

# Plotting
ggplot(yearly_counts, aes(x = as.factor(year), y = cnt)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Yearly Distribution of Total Bikes Rented",
       x = "Year",
       y = "Total Bikes Rented") +
  theme_minimal()


ggplot(data, aes(x = as.factor(year), y = cnt)) +
  geom_boxplot(fill = "skyblue", color = "gray") +
  labs(title = "Boxplot for Outliers' Analysis",
       x = "Year",
       y = "Total Bikes Rented") +
  theme_minimal()


##Machine Learning Portion

#Spliting Data
set.seed(508)
index <- createDataPartition(data$cnt, p = 0.75, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]


# Create the random forest model
rf_model <- randomForest(cnt ~ ., data = train_data, ntree = 500, importance = TRUE)

predictions <- predict(rf_model, newdata = test_data)

# Compare predictions with actual values
comparison <- data.frame(Actual = test_data$cnt, Predicted = predictions)

# View the comparison
print(comparison)