library(party)
# data <- readRDS("Completed_military_training_data.rds")

set.seed(123)  # Set the seed for reproducibility

n <- nrow(data) # Get the number of rows in the dataset
indices <- sample(1:nrow(data))
K = 10
folds <- cut(indices, breaks = K, labels = FALSE)	# Split indices into K equally sized groups
print(K)
num_vars <- ncol(data) - 1 # Get the number of variables in the model (excluding the response variable)

# Initialize
VARIMP <- matrix(NA, nrow = num_vars, ncol = K) # Get correct dimensions
Yhat = data$FULLFORT_RS

start=Sys.time()
total_time <- 0
for(k in 1:K){
  print("this is running")
  start_time <- Sys.time()
  MODEL <- party::cforest(FULLFORT_RS ~ ., data =data[indices[folds != k],], controls = party::cforest_unbiased(ntree = 500, mtry = 10))
  VARIMP[,k] = party::varimp(MODEL)
  Yhat[indices[folds == k]] <- predict(MODEL, newdata = data[indices[folds == k],])
  end_time <- Sys.time()
  fold_time <- end_time - start_time
  total_time <- total_time + fold_time
  avg_time_per_fold <- total_time / k
  remaining_folds <- K - k
  estimated_remaining_time <- avg_time_per_fold * remaining_folds
  print(paste("Fold", k, "of", K, "completed in", fold_time, "seconds"))
  print(paste("Estimated remaining time:", estimated_remaining_time, "seconds"))
}
stop=Sys.time()
stop-start

#save(MODEL, file = "MODEL.RData")
#saveRDS(Yhat, "yhat.rds")
#saveRDS(VARIMP, "varimp.rds")