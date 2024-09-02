# HW_07.R based on problem 4.2

# Load necessary libraries
library(faraway)
library(olsrr)

# Load your data
ridge_data <- read.csv("Documents/Rutgers/RegressionAnalysis/Data/ridge_data.csv")

lmod = lm(y ~ x1 + x2 + x3 + x4 + x5 + x8 + x9 + x10, data = ridge_data)

# a. Forward Stepwise Selection
forward_model <- ols_step_forward_p(lmod)
forward_model

# Print the chosen model
print("Forward Stepwise Selection Chosen Model:")
print(forward_model$model)

# b. Backward Stepwise Elimination
backward_model <- ols_step_backward_p(lmod)
backward_model

# Print the chosen model
print("Backward Stepwise Elimination Chosen Model:")
print(backward_model$model)

# c. Bi-directional Stepwise Selection
both_model <- ols_step_both_p(lmod, print_plot=TRUE)
both_model

# Print the chosen model
print("Bi-directional Stepwise Selection Chosen Model:")
print(both_model$model)

# d. All Possible Regressions
all_models <- ols_step_all_possible(model = lm(y ~ x1 + x2 + x3 + x4 + x5 + x8, data = ridge_data))
ols_step_all_possible

# Choosing the best model using Adjusted R-Squared
best_model <- all_models$models[which.max(all_models$adjr), ]

# Print the chosen best model
print("Best Model among All Possible Regressions (based on Adjusted R-Squared):")
print(best_model)

dev.off()
