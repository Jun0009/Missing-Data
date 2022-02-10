# Quiz 3

# We use again, as in Quiz 2, the Anscombe dataset from the package carData
# Load the data with:
library(carData)
data(Anscombe)

# The research objective is still to predict education based on income, young and urban

# You are free to use any lecture notes, R code examples, packages and commands.

# When done, submit the R code with ANSWERS AS COMMENTS!


# Task 1: Fit the regression to the original dataset and report the estimated EQUATION
# Your answer should look something like education = b0 + b1*income + b2*young + b3*urban
# where b0, b1, b2 and b3 are the estimated coefficients after you perform the lm function

model = lm(education~income+young+urban,data=Anscombe)
summary(model)

#The estimated equation is education = -286.839+0.08065*inocme+0.8173*young-0.1058*urban

# Task 2. Now use ampute from mice package to create missingness with the following commands,
# which keeps education completely observed

library(mice)
set.seed(954)
ans.miss = ampute(Anscombe, prop = 0.6)$amp
ans.miss = cbind(Anscombe$education, ans.miss[, 2:4])
colnames(ans.miss)[1] = "education"

# Task 3: Use mice package to obtain the missing patterns matrix.
# How many total cells are missing? (That is, total number of NA)
# Your answer should be a single number.
md.pattern(ans.miss, rotate.names = T)


# 24 total cells are missing

# Task 4: Impute the data with mice package using seed 10.
# Use default settings and don't make any changes. 
# Pool and obtain the estimated regression equation.
# Report the average % change in all regression coefficients
# Your answer should be a single number obtained in the following way:

# mean(abs(reg$coef - micepoolsummary$estimate)/abs(reg$coef)), 

# where reg is the original regression and micepoolsummary is from mice output, as shown in class

imp = mice(ans.miss, seed = 10)
fit = with(imp, lm(education~income+young+urban))
s=summary(mice::pool(fit))
mean(abs(model$coef - s$estimate)/abs(model$coef))

#The average %change is 13.29%.

# Task 5: Keep seed at 10, but increase number of iterations to 50 and impute again.
# Pool and report the average percent change in the coefficients (again, compared to full dataset)
imp = mice(ans.miss, m=50,seed = 10)
fit2 = with(imp, lm(education~income+young+urban))
s2=summary(mice::pool(fit2))
mean(abs(model$coef - s2$estimate)/abs(model$coef))

#The average %change is 14.25%.

# Task 6: Now switch to mi package.
library(mi)

# IMPORTANT: To call a specicif pool command from the specific package use
# mi::pool or mice::pool

# Obtain the missing data patterns. How many different patterns do we have?
# Answer should a single number
mdf <- missing_data.frame(ans.miss)
tabulate(mdf@patterns)
levels(mdf@patterns) 

#4 patterns


# Task 7: Impute the data with default settings, use seed 10 and parallel = F
# pool and report average percent change in coefficients compared to full dataset
# To obtain the coefficients use coef(mipoolresult)
imp.a <- mi(mdf, seed = 10, parallel = F)
res.a = mi::pool(education~income+young+urban, data=imp.a)
mean(abs(model$coef - coef(res.a))/abs(model$coef))

#the average percent change is 34.14%.


# Task 8: Run diagnostic plots. Change income to positive type, repeat and 
# report the resulting mean % change in the regression coefficients.
plot(imp.a)
mdf = change(mdf, y = "income", what = "type", to = "po")
show(mdf)
imp.a <- mi(mdf, seed = 10, parallel = F)
res.a = mi::pool(education~income+young+urban, data=imp.a)
mean(abs(model$coef - coef(res.a))/abs(model$coef))

#the mean% change is 29.16.

# Task 9: Report the r-hat values for the mean of each imputed variable from Task 8
# Your answer should be three numbers.

Rhats(imp.a)

#income:1.002
#young: 1.015
#urban: 1.058

# Task 10: Which version of the imputations turned out to be the best?

#Imputation with mice package using seed 10 is the best
