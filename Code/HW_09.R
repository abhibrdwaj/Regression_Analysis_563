# HW_09.R
# Load package and meatspec dataset
library(faraway)
data(meatspec)
head(meatspec)
str(meatspec)

# Constants
alpha <- 0.05
n <- ncol(meatspec) - 1

# Do linear regression on meatspec
lmod <- lm(fat ~ ., data=meatspec)
summary(lmod)
anova(lmod)

# Extract the p-values from summary
p_values <- summary(lmod)$coefficients[, 4]

# using NO FWER adjustment (use alpha=.05 for each test of hypothesis)
names(p_values[p_values < alpha])

# using the Bonferroni procedure to control the FWER alpha at 0.05.
# alpha' = alpha/n
bonferroni_alpha <- alpha / n
names(p_values[p_values < bonferroni_alpha])

# using the Holm procedure to control the FWER alpha at 0.05.
holm_p <- p.adjust(p_values, method="holm")
names(p_values[holm_p < alpha])

