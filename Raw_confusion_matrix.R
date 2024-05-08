# yhat <- readRDS("yhat.rds")
# data <- readRDS("Completed_military_training_data.rds")

# Convert Yhat to numeric
Yhat_numeric <- as.numeric(as.character(yhat))

# Convert the predicted values to a factor
predicted_values <- as.factor(ifelse(Yhat_numeric >= 0.5, 1, 0))

# Define true_values as the actual values to compare predictions against
true_values <- as.factor(data$FULLFORT_RS)

# Create the confusion matrix
confusionMatrix(predicted_values, true_values)