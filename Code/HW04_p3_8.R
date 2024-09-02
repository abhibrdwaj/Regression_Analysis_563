#HW04_p3_8.R Problem 3.8 B5.csv data

library(faraway)
library(olsrr)

p3_8 <- read.csv(file="./Documents/Rutgers/RegressionAnalysis/Data/B5.csv",header = TRUE)
head(p3_8)
str(p3_8)

#save graph in pdf
pdf(file="./Documents/Rutgers/RegressionAnalysis/Output/HW04_fig.pdf")
pairs(~ y+x6+x7, data=p3_8)

# a: Fit a multiple regression model relating CO2 product (y) to total solvent (x6) and hydrogen consumption (x7).

lmod <- lm(y ~ x6 + x7, data = p3_8)
lmod_summary <- summary(lmod)
(lmod_olsrr = ols_regress(y ~ x6 + x7,data=p3_8))

# Part b: Test for significance of regression
# Calculate R-squared and adjusted R-squared
anova(lmod)

lmod_summary$r.squared
lmod_summary$adj.r.squared

lmod_olsrr$rsq
lmod_olsrr$adjr


# Part c: Using t tests determine the contribution of x6 and x7 to the model.

lmod_summary

# Part d: Construct 95% CIs on β6 and β7.

ci_lmod <- confint(lmod)
ci_lmod

# Part e:  Refit the model using only x6 as the regressor. Test for significance of
# regression and calculate R2 and R2 . Discuss your findings. Based on these Adj statistics
# are you satisfied with this model?

lmod_refit <-  lm(y ~ x6, data = p3_8)
refit_summary <- summary(lmod_refit)

# Plot the graph for refitted model y vs x6
plot(y ~ x6, data=p3_8)
abline(lmod_refit)

refit_summary

refit_summary$r.squared
refit_summary$adj.r.squared

# Part f: Construct a 95% CI on β6 using the model with only x6
# Compare the length of this CI to the length of the CI in part d.
# Does this tell you anything important about the contribution of x7 to the model?

ci_refit <- confint(lmod_refit)

length_ci_lmod <- diff(ci_lmod["x6", ])
length_ci_refit <- diff(ci_refit["x6", ])

length_ci_lmod
length_ci_refit

# Part g: Compare MS_res for the two models
# How did the MSRes change when you removed x7 from the model?
# Does this tell you anything importaut about the contributiou of x7 to the model?

lmod$residuals

msres_lmod <- sum(lmod$residuals^2) / lmod$df.residual
msres_refit <- sum(lmod_refit$residuals^2) / lmod_refit$df.residual

msres_lmod
msres_refit

dev.off()
