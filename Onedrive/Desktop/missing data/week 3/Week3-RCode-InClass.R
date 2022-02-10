# Generating missing data

# Assume regression model
# y = 1 + 5x + e, where e ~ N(0, 1)

# Simulate full dataset
# n is the sample size (number of cases)
n = 50
# the independent variable can be generated from any distribution
set.seed(48)
x = runif(n)

# Errors from the N(0, 1) distribution
e = rnorm(n)

# y according to our model
y = 1 + 5*x + e

# Plot full dataset
plot(x, y)

# Estimate b0 and b1 (should be around 1 and 5)
summary(lm(y~x))
# Adding the estimated line to the plot
abline(lm(y~x))

# Generate MCAR data by randomly deleting 10% of observations in each of the two variables
p = 0.1
nmiss = round(n*p, 0)

xmis = x
xmis[sample(1:n, nmiss)] = NA

ymis = y
ymis[sample(1:n, nmiss)] = NA


# Estimate b0 and b1 again using complete cases
summary(lm(ymis ~ xmis))

# Is there a bias?
# We need to replicate this experiment many times to answer the question.
# First with no missing data as an example of how simulation works


# Define number of replications 
R <- 1000

### Create a matrix to store output
# We need two columns
out <- matrix(0, R, 2)

### Run the simulation within a for loop
for (i in 1:R) 
{
  e = rnorm(n)
  y = 1 + 5*x + e
  reg = lm(y ~ x)
  out[i,] <- summary(reg)$coef[1:2]
}

### Plot the histograms
par(mfrow = c(1,2))

# b0
hist(out[,1])
abline(v = mean(out[,1]), col = "red")
mean(out[,1])
sd(out[,1])

# b1
hist(out[,2])
abline(v = mean(out[,2]), col = "red")
mean(out[,2])
sd(out[,2])
# Works like a charm!


# Now with missing data

# Analysis function to calculate the regression coefficients
simFun.mis <- function(e, p = 0.1) 
{	
  y = 1 + 5*x + e
  nmiss = round(n*p, 0)
  x[sample(1:n, nmiss)] = NA
  y[sample(1:n, nmiss)] = NA 
  reg = lm(y ~ x)
  summary(reg)$coef[1:2]
}

R <- 1000 # Define number of replications 

### Create a matrix to store output
# We need two columns
out.mis <- matrix(0, R, 2)


### Run the simulation within a for loop
for (i in 1:R) 
{
  out.mis[i,] <- simFun.mis(rnorm(n), 0.2)
}

### Plot the histograms
hist(out.mis[,1])
abline(v = mean(out.mis[,1]), col = "red")
mean(out.mis[,1])
sd(out.mis[,1])

hist(out.mis[,2])
abline(v = mean(out.mis[,2]), col = "red")
mean(out.mis[,2])
sd(out.mis[,2])



# Exercise: Repeat the above experiment about bias by
# a) using mean imputation
# b) using random imputation
# Try either the in-class functions and the mice package:

# Mean imputation function from last time
mean.imp <- function (a)
{
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  return (imputed)
}

# Alternative: Use mice(data.frame(d), method = "mean", m = 1, maxit = 1)


# Simple random imputation from last time
random.imp <- function (a)
{
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

# Alternative: Use mice(data.frame(d), method = "sample", m = 1, maxit = 1)










# We now will use a multiple regression model
# x1 and x2 are the predictor variables
# x1 will be completely observed
# y will be completely observed
# x2 will be MAR depending on x1


# Creating MAR data, approx. 10% of x2, those which x1 > 0.9
# Direct way
x1 = runif(n)
x2 = 2 + x1 + rnorm(n, sd=.5)
y = 1 + 5*x1 +6*x2 + rnorm(50)
x2[x1 > 0.9] = NA 
d = data.frame(y, x1, x2)

# Or use ampute function in mice package
x1 = runif(n)
x2 = 2 + x1 + rnorm(n, sd=.5)
y = 1 + 5*x1 +6*x2 + rnorm(50)
m = cbind(y,x1,x2)
m

library(mice)
ampute(m)
ampute(m, prop = 0.6)$amp

# regression imputation
fit = lm(x2 ~ y + x1, data = d)
# Note: for your project x2 is the variable you want to impute
# y, x1, ... will be all completely observed variables

# We use the ic function from mice package 
# It selects the incomplete cases
pred = predict(fit, newdata = ic(d))
# imputed values for x2
pred
# These should go here in place of NAs:
d$x2

# Alternatively, with mice package:
complete(mice(data.frame(d), method = "norm.predict", m = 1, maxit = 1))


# Function to calculate the regression coefficients by regression imputation
# Note it uses the function ic from mice package

simFun.reg <- function(e) 
{	
  y = 1 + 5*x1 +6*x2 + e # Generating data
  x2[x1 > 0.9] = NA # Create MAR for x2
  fit = lm(x2 ~ y + x1) # Regress missing data variable on complete variables
  x2[is.na(x2)] = predict(fit, ic(data.frame(y, x1, x2))) # Imputing the predictions
  reg = lm(y ~ x1 + x2) # Estimate coefficients from the now complete data
  summary(reg)$coef[1:3]
}

### Define number of replications 
R <- 1000

### Create a matrix to store output
# We need two columns
out.mis4 <- matrix(0, R, 3)


### Run the simulation within a for loop
for (i in 1:R) 
{
  out.mis4[i,] <- simFun.reg(rnorm(n))
}

### Plot the histogram
par(mfrow =c(1,3))

hist(out.mis4[,1])
abline(v = mean(out.mis4[,1]), col = "red")
mean(out.mis4[,1])
sd(out.mis4[,1])

hist(out.mis4[,2])
abline(v = mean(out.mis4[,2]), col = "red")
mean(out.mis4[,2])
sd(out.mis4[,2])

hist(out.mis4[,3])
abline(v = mean(out.mis4[,3]), col = "red")
mean(out.mis4[,3])
sd(out.mis4[,3])


### Analysis function to calculate the regression coefficients by hotdeck

simFun.hotdeck <- function(dat) 
{	
   y = 1 + 5*x1 +6*x2 + dat
   # Generates missing x2 if x1 > 0.9
   x2[x1 > 0.9] = NA

   # Calculate all distances using ONLY completely observed data
   d = as.matrix(dist(cbind(y, x1)))
   
   # This will store the new x2 with imputed nearest neighbor values
   imp.x2 = x2
   
   # Loops through the missing values, finds the nearest match and imputes it
   for (j in 1:n) if (is.na(x2[j]))
   {
     # Find minimum distance from missing to non-missing x2s
     min.d = min(d[j,!is.na(x2)])
     # Replaces missing x2 with first nearest non-missing x2
     imp.x2[j] = x2[d[j,]== min.d][1]
   }
  x2 = imp.x2
  reg = lm(y ~ x1 + x2)
  summary(reg)$coef[1:3]
}

R <- 1000

### Create a matrix to store output, we need two columns
out.mis5 <- matrix(0, R, 3)


### Run the simulation within a for loop
for (i in 1:R) 
{
  out.mis5[i,] <- simFun.hotdeck(rnorm(n))
}
par(mfrow=c(1,3))
### Plot the histogram
hist(out.mis5[,1])
abline(v = mean(out.mis5[,1]), col = "red")
mean(out.mis5[,1])
sd(out.mis5[,1])

hist(out.mis5[,2])
abline(v = mean(out.mis5[,2]), col = "red")
mean(out.mis5[,2])
sd(out.mis5[,2])

hist(out.mis5[,3])
abline(v = mean(out.mis5[,3]), col = "red")
mean(out.mis5[,3])
sd(out.mis5[,3])