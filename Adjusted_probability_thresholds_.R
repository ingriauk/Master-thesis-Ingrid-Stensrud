library(party)
library(caret)

# data <- readRDS("~/Completed_military_training_data.rds")
# MODEL <- load("/MODEL.RData")


# Get predicted probabilities
# start=Sys.time()
# prob <- predict(MODEL, newdata=data, OOB=TRUE, type="prob")
# stop=Sys.time()
# stop-start
#saveRDS(prob, "prob.rds")

prob <-readRDS("/prob.rds")

# Extract probabilities for class 1
prob_class1 <- sapply(prob, function(x) x[2])

# Apply the threshold to the probabilities
thresholds <- c(0.3, 0.4, 0.5, 0.6)

for (threshold in thresholds) {
  predicted_class <- ifelse(prob_class1 > threshold, 1, 0)
  
  confusion_matrix <- table(Predicted = predicted_class, Actual = data$FULLFORT_RS)
  
  cat("Threshold:", threshold, "\n")
  print(confusion_matrix)
  cat("\n")
}