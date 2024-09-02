# HW_10.R Multiple Testing
# Load package and meatspec dataset
library(faraway)
data(meatspec)
head(meatspec)
str(meatspec)

bh_fdr <- 0.2

# Do linear regression on meatspec
lmod <- lm(fat ~ ., data=meatspec)
summary(lmod)
anova(lmod)

# Extract the p-values for coefficients from summary
p_values <- summary(lmod)$coefficients[, 4]

# Benjamini-Hochberg

# Load the stats package for p.adjust function
adjusted_p <- p.adjust(p_values, n = length(p_values), method = "BH")

# List the variables with coefficients that are significantly different from 0
significant_vars <- names(adjusted_p[adjusted_p < bh_fdr])
significant_vars
