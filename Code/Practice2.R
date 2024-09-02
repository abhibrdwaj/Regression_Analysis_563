library(tidyverse)
library(faraway)

Shelf = read.csv(file="Documents/Rutgers/RegressionAnalysis/Data/ShelfStockingdata.csv",header = TRUE)

Shelf

lmod = lm(time_y ~ cases_x, data=Shelf)

names(lmod)
pdf(file="Documents/Rutgers/RegressionAnalysis/Output/ShelfStocking_out.pdf")
plot(time_y ~ cases_x,Shelf)
abline(lmod)
# test HO:B1=0 vs H1:B1 ne 0
summary(lmod)
anova(lmod)

F = rnorm(50000,mean=0,sd=1)
d = density(F)
plot(d,main="standard normal distribution")
p1 = pnorm(-1.96)
