# Load the R package
library(tidyverse)
library(readxl)
library(randomForest) 
library(caret)
library(vivid)
options(warn = -1)
# Data Reading
df <- read_xlsx('climate and wild bird count.xlsx')
df <- df[,-1]
set.seed(123)
# Define cross-validation control parameters
ctrl <- trainControl(
  method = "cv",          
  number = 10,           
  verboseIter = TRUE,    
  allowParallel = TRUE  
)
# Define the range of hyperparameter values to be tuned
mtry_values <- c(2, 3, 4, 5)          
ntree_values <- c(500, 1000, 1500)    
maxnodes_values <- c(16, 32, 64)      

# Variables used to store the best model results
best_metric_value <- -Inf
best_params <- list()
best_model <- NULL

# Perform grid search
for (n_tree in ntree_values) {
  for (max_node in maxnodes_values) {
    cat(paste0("Tuning in progress: ntree = ", n_tree, ", maxnodes = ", max_node, "\n"))
    param_grid_inner <- expand.grid(mtry = mtry_values)
    set.seed(123) 
    rf_cv_inner <- train(
      count ~ .,
      data = df,
      method = "rf",      
      trControl = ctrl,     
      tuneGrid = param_grid_inner,  
      ntree = n_tree,      
      maxnodes = max_node,  
      importance = TRUE,
      metric = "Rsquared"
    )
    current_results <- rf_cv_inner$results
    best_mtry_row <- current_results[which.max(current_results$Rsquared), ]
    current_r_squared <- best_mtry_row$Rsquared
    
    cat(paste0("  Current combination (ntree = ", n_tree, ", maxnodes = ", max_node, ") best mtry = ", best_mtry_row$mtry, ", R-squared = ", round(current_r_squared, 4), "\n"))
    if (current_r_squared > best_metric_value) {
      best_metric_value <- current_r_squared
      best_params <- list(ntree = n_tree, maxnodes = max_node, mtry = best_mtry_row$mtry)
      best_model <- rf_cv_inner
    }
  }
}

print("Best hyperparameter combination found:")
print(best_params)
print("Best model:")
print(best_model)

# Plot the pdp
set.seed(123)
pdpVars(data = df,
        fit = best_model,
        response = "count",
        pal = c("#2A363BFF", "#019875FF", "#99B898FF", "#FECEA8FF", "#FF847CFF", "#E84A5FFF", "#C0392BFF", "#96281BFF"),
        nIce = 500)
# Calculate R-squared
cor(df$count,predict(best_model))^2

# 使用caret的变量重要性函数
var_imp_caret <- varImp(best_model,scale = F)
print(var_imp_caret)


























































