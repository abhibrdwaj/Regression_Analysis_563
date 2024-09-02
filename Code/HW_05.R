#HW_05.R B17.csv data

library(faraway)
library(olsrr)

b_17 <- read.csv(file="./Documents/Rutgers/RegressionAnalysis/Data/B17.csv",header = TRUE)
head(b_17)
str(b_17)

#save graph in pdf
pdf(file="./Documents/Rutgers/RegressionAnalysis/Output/HW05_fig.pdf")

# a. Scatter plot matrix
pairs( ~ Satisfaction+Age+Severity+SurgicalMedical+Anxiety,data=b_17)

# b. Correlation matrix
cor(b_17)

# c. Least squares prediction equation
lmod <- lm(Satisfaction ~ Age + Severity + SurgicalMedical + Anxiety, data=b_17)
lmod
summary(lmod)
(lmod_olsrr = ols_regress(Satisfaction ~ Age + Severity + SurgicalMedical + Anxiety, data=b_17))

# d. Compute VIFs

vif(lmod)

# e. Interpret the Beta estimate of Anxiety

summary(lmod)$coefficients["Anxiety", "Estimate"]

# f. Partial F-test on Anxiety

full_mod <- lmod
red_mod <- lm(Satisfaction ~ Age + Severity + SurgicalMedical, data=b_17)
anova(red_mod, full_mod)

dev.off()

