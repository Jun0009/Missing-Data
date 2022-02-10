# Using the MI package
# install.packages("mi")
library(mi)

# Getting the built-in dataset nlsyV
data(nlsyV, package = "mi")

# Create the missing data frame object
mdf = missing_data.frame(nlsyV)

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
# For example, 172 cases had "nothing" missingness pattern

# Examine the default settings
show(mdf)

# Make changes to some variables as an example
mdf = change(mdf, y = c("momed", "momrace"), what = "type", to = "un")
show(mdf)

# Running the chains
imputations <- mi(mdf, parallel = F)
# IMPORTANT note: By default, on Macs mi uses parallel processor computing 
# however, it does not work well on macOS Big Sur, so I switched to FALSE.
# If you are using a different OS try parallel= T for faster computations.

(converged = mi2BUGS(imputations))
# Note the result is an array of matrices with all imputed variables means and sd's
# Each matrix is of order iter by m

# Extract specific variables from the imputations
(mean_ppvtr.36 = converged[, , 1])
mean_income = converged[, , 3]

# Switch back to 1x1 graph window
par(mfrow = c(1,1))

# Traceplot of mean imputed values of  ppvtr.36
ts.plot(mean_ppvtr.36[,1], col=1, ylim = c(-0.03, 0.03))
lines(mean_ppvtr.36[,2], col= 2)
lines(mean_ppvtr.36[,3], col= 3)
lines(mean_ppvtr.36[,4], col= 4)

# Traceplot of mean imputed income
ts.plot(mean_income[,1], col=1, ylim = c(-0.04, 0.04))
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income [,4], col= 4)

# We can also check if the mean of each completed variable is roughly the same for each of the 4 chains.
round(mipply(imputations, mean, to.matrix = TRUE), 3)

# R-hat should be around 1
Rhats(imputations)


# Checks how good the models fit
plot(imputations)
# Turn off the "Hit <Return>" message
par(ask=F)
par(mfrow = c(1,1))
# Conclusion: Income variable clearly has a problem!

# How do we fix the problem with income?
# Look at income again
hist(nlsyV$income)
# It is highly right-skewed and has pileup at 0

# Make changes to income variable
mdf <- change(mdf, y = "income", what = "type", to = "nonn")
show(mdf)
# Note the log transformation on income!

# Check histograms again
hist(mdf)
# Now income looks more promising and symmetric

# Run the chains again!
# And we can always benefit from more iterations:
imputations <- mi(mdf, n.iter = 60)
Rhats(imputations)
converged <- mi2BUGS(imputations)

mean_income = converged[, , 3]
# Traceplot of mean imputed income
ts.plot(mean_income[,1], col=1)
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income [,4], col= 4)

plot(imputations)

# Try pmm
mdf = missing_data.frame(nlsyV)


mdf <- change(mdf, y = c("momed", "momrace"), what = "type", to = "un")
show(mdf)
mdf <- change(mdf, y = "income", what ="imputation_method", to = "pmm")
show(mdf)
imputations <- mi(mdf, n.iter=60)


converged <- mi2BUGS(imputations)

mean_ppvtr.36 = converged[, , 1]
mean_income = converged[, , 3]
par(mfrow = c(1,1))
# Traceplot of mean income
ts.plot(mean_income[,1], col=1)
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income [,4], col= 4)

plot(imputations)

analysis <- pool(ppvtr.36 ~ first + b.marr + scale(income) + momage + momed + momrace, imputations, m=5)
display(analysis)

analysis <- pool(ppvtr.36 ~ first + b.marr + income + momage + momed + momrace, imputations, m=5)
display(analysis)

summary(glm(formula = ppvtr.36 ~ first + b.marr + scale(income) + momage
+ factor(momed) + factor(momrace), family = gaussian, data =
nlsyV))


# Exercise:
# Practice mi with the missing data that you created for Quiz 2. 
# Compare to the other results.