# Quiz 2
# Use the Anscombe dataset from the package carData
# The research objectove is to predict education based on income, young and urban

# You are free to use any lecture notes, R code examples, packages and commands.

# When done submit the R code with comments where your answers are

library(carData)
data(Anscombe)

# Task 1: Fit the regression to the original dataset and report the estimated equation
# Your answer should look something like education = b0 + b1*income + b2*young + b3*urban
# where b0, b1, b2 and b3 are the estimated coefficients after you perform the lm function

reg = lm(education ~ ., data = Anscombe)
summary(reg)

# education = -286.8 + 0.008*income + 0.817*young - 1.06*urban

# Task 2. Now use ampute from mice package to create missingness wih the following command:

library(mice)
set.seed(894)
ans.miss = ampute(Anscombe, prop = 0.6)$amp
ans.miss = cbind(Anscombe$education, ans.miss[, 2:4])
colnames(ans.miss)[1] = "education"


library(mi)

# Create the missing data frame object
mdf = missing_data.frame(ans.miss)

# Five-number summary statistics + missing number
summary(mdf)

# Histograms of all variables with missing values
hist(mdf)

# Missing patterns
# Graph of the missing pattern matrix R
image(mdf, grayscale=TRUE)

# Standardized observed values together with missing
image(mdf)

# Or look at the patterns numerically:
tabulate(mdf@patterns)
levels(mdf@patterns) 

# Examine the default settings
show(mdf)


# Running the chains
imputations <- mi(mdf)
converged <- mi2BUGS(imputations)

# Note the result is an array of matrices with all imputed variables means and sd's
# Each matrix is of order iter x m

# Extract specific variables from the imputations
(mean_income = converged[, , 1])
mean_young = converged[,, 2]
mean_urban = converged[, , 3]

par(mfrow = c(1,1))

# Traceplot of mean imputed values of  ppvtr.36
ts.plot(mean_income[,1], col=1)
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income[,4], col= 4)

# Traceplot of mean imputed income
ts.plot(mean_young[,1], col=1)
lines(mean_young[,2], col= 2)
lines(mean_young[,3], col= 3)
lines(mean_young [,4], col= 4)


# R-hat should be around 1
Rhats(imputations)



# Checks how good the models fit
plot(imputations)

analysis <- mi::pool(education ~  income + young + urban, imputations, m=5)
display(analysis)

summary(glm(formula = education ~  income + young + urban, family = gaussian, data =Anscombe))

