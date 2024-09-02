# HW_08.R based on problem 13.7
# Data we will be using is p13_7modified.csv

# Load libraries
library(faraway)
library(olsrr)
library(epiDisplay)

# Defining dataset
p13_7 <- read.csv(file="./Documents/Rutgers/RegressionAnalysis/Data/p13_7modified.csv", header = TRUE)
p13_7
summary(p13_7)

#save graph in pdf
pdf(file="./Documents/Rutgers/RegressionAnalysis/Output/HW_08_fig.pdf")
pairs(~ y+x1+x2+x3+x4, data=p13_7)

# a. Fit a logistic regression model to the data.
glmod <- glm(y ~ x1 + x2 + x3 + x4, data = p13_7, family = binomial)
summary(glmod)

# b.Test H0: all Beta-parameters are 0 versus at least one Beta-parameter is not 0.
# Fit the null model
null_mod <- glm(y ~ 1, data = p13_7, family = binomial)
summary(null_mod)
# Perform global test on parameters using the likelihood ratio test
lrtest_res <- lrtest(null_mod, glmod)
print(lrtest_res)

# c. Plot the observed binary responses versus the fitted values.
fitted_values <- fitted(glmod)
plot(p13_7$y, fitted_values, xlab = "Observed Binary Responses", ylab = "Fitted Values", main = "Observed vs Fitted Values")
abline(0, 1, col = "red")

# d. Use the logistic.display function found in the epiDisplay library to show 
# statistics for each individual parameter from the logistic regression performed in part a
logistic.display(glmod)

dev.off()
