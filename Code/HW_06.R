# HW_06.R based on problem 4.2
# Data we will be using is B1.csv based on NFL data in p3_1

# Load libraries
library(faraway)
library(olsrr)
library(car)

# Defining dataset
p3_1 <- read.csv(file="./Documents/Rutgers/RegressionAnalysis/Data/B1.csv",header = TRUE)
p3_1
summary(p3_1)

#save graph in pdf
pdf(file="./Documents/Rutgers/RegressionAnalysis/Output/HW_06_fig.pdf")
pairs(~ y+x2+x7+x8, data=p3_8)

# Model Regression
lmod <- lm(y ~ x2 + x7 + x8, data = p3_1)

# Summary of the linear model
summary(lmod)
# ANOVA of the linear model
anova(lmod)

# a. Generate a Q-Q plot of the residuals
qqnorm(residuals(lmod), ylab = "Residuals", main = "Q-Q plot")
qqline(residuals(lmod))

# Does there seem to be any problem with the normality assumption?
# As observed from the Q-Q plot. although the central values have residuals
# approximated along the line, indicating a normal distribution; as the values 
# get higher, the residuals deviate due to possible outliers within the data
# However, considering this is not such a huge diveation, with only outliers having
# the exception, we can assume the residuals are normally distributed.

# b. Construct and interpret a plot of the residuals versus the predicted response.
# Calculate predicted values and residuals
predicted_values <- fitted(lmod)
residuals <- residuals(lmod)

# Plot the residuals versus the predicted values
plot(predicted_values, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs Predicted Values")
abline(h = 0, col = "red")

# c. Construct plots of the residuals versus each of the regressor variables. Do
# these plots imply that the regressor is correctly specified?

# Plot for x2
plot(p3_1$x2, residuals(lmod), xlab = "x2", ylab = "Residuals")
abline(h = 0)

# Plot for x7
plot(p3_1$x7, residuals(lmod), xlab = "x7", ylab = "Residuals")
abline(h = 0)

# PLot for x8
plot(p3_1$x8, residuals(lmod), xlab = "x8", ylab = "Residuals")
abline(h = 0)


# d. Construct the partial regression plots for this model.
# Compare the plots with the plots of residuals versus regressors from part c above.
# Discuss the type of information provided by these plots.

avPlots(lmod, print_plot = TRUE)


# e. Compute the studentized residuals and the R-student residuals for this model.
# What information is conveyed by these scaled residuals?

# Compute studentized residuals
studentized_residuals <- ols_plot_resid_stand(lmod)$data
print(studentized_residuals)

# Compute R-student residuals
r_student_residuals <- ols_plot_resid_stud(lmod)$data
print(r_student_residuals)

# Plot studentized residuals
plot(studentized_residuals, ylab = "Studentized Residuals", main = "Studentized Residuals")
abline(h = c(-2, 2), col = "red", lty = 2)

# Plot R-student residuals
plot(r_student_residuals, ylab = "R-student Residuals", main = "R-student Residuals")
abline(h = c(-2, 2), col = "red", lty = 2)

dev.off()
