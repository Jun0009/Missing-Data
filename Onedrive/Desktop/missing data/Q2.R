# Quiz 2
# Use the Anscombe dataset from the package carData

library(carData)
data(Anscombe)

# The research objective is to predict education based on income, young and urban

# You are free to use any lecture notes, R code examples, packages and commands.

# When done submit the R code with comments where your answers are

# Task 1: Fit the regression to the original dataset and report the estimated equation
# Your answer should look something like education = b0 + b1*income + b2*young + b3*urban
# where b0, b1, b2 and b3 are the estimated coefficients after you perform the lm function


#check data
str(Anscombe)


#fit regression 
model1 = lm(education ~ income+ young+ urban, data = Anscombe)
summary(model1)
#show intercept and coefficients of regression model
coef(model1)

# Task 2. Now use ampute from mice package to create missingness wih the following commands
# which keeps education completely observed
library(mice)
set.seed(895)
ans.miss = ampute(Anscombe, prop = 0.6)$amp
ans.miss = cbind(Anscombe$education, ans.miss[, 2:4])


# Task 3. Report the percent missing from each variable in ans.miss
# Your answer should be three percentages, the averages of non-deleted income, young and urban.

#check for missing values
m=sapply(ans.miss, function(x) sum(is.na(x)))
#percentage of missingness
total = nrow(ans.miss)
(m/total)*100

#missing percentage for income: 7.8%, for young: 21.57%, for urban:5.88%

#find average of non-deleted income
mean=sapply(ans.miss, function(x) mean(x,na.rm=TRUE))
mean

#the average of income is 3192.3, average of young is 359.1, and average of urban
#is 669.9



# 4. Is the resulting dataset ans.miss MCAR, MAR, or MNAR?

#the resulting dataset is MAR

# 5. Refit the regression model using lm command on the new dataset ans.miss
# Report the total change in all regression coefficients
# Your answer should be a single number obtain the following way:
# sum(abs(reg$coef - reg.miss$coef)), where reg is the original regression and reg.miss is from ans.miss

model2 = lm(Anscombe$education ~ income+ young+ urban, data = ans.miss)
#total as below:
sum(abs(model1$coef - model2$coef))

# 6. What is the name of the missing data technique you applied in part 5?

#the missing data technique is regression imputation

# 7. Use ans.miss dataset and apply mean imputation to restore all variables.
# Store the restored data as ans.mean
# Report the four imputed means.
# Your answer should be four numbers.

## Mean imputation
mean.imp <- function (a){
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  return (imputed)
}

ans.mean=mean.imp(ans.miss)
mean2=sapply(ans.mean, function(x) mean(x,na.rm=TRUE))
mean2

# 8. Fit again the regression model on mean imputed data and report the total change in the regression
# using same techinque as in part 5.
# Your answer should be a single number indicating the total absolute change in coefficients.

model3 = lm(Anscombe$education ~ income+ young+ urban, data = ans.mean)
#total as below:
sum(abs(model1$coef - model3$coef))


# 9. Repeat parts 7-8 but now use regression imputation based on the complete variable education
# Your answer should be a single number, the total change in coefficients
d =complete(mice(data.frame(ans.miss), method = "norm.predict", m = 1, maxit = 1))
model4 = lm(Anscombe$education ~ income+ young+ urban, data = d)
#total as below:
sum(abs(model1$coef - model4$coef))

# 10. Which method resulted in smallest change in the regression coefficients 
# as compared to the full dataset?

#the regression imputation gives the samllest change