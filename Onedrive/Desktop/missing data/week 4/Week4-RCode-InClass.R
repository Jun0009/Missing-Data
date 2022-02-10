# Generating missing data, Part 2
# This is from Problem 1.6 on p. 23 of Little and Rubin


# Consider three variables y1, y2, and u as follows
# y1 = 1 + z1
# y2 = 5 + 2*z1 + z2
# u = a*(y1 - 1) + b*(y2 - 5) + z3
# where z1, z2, and z3 are independent standard normal random variables
# Notice E(y1) = Var(y1) = 1, E(y2) = Var(y2) = 5, and corr(y1, y2) = 0.89
# y1 will be completely observed, but y2 will be missing if u < 0
# This will create roughly 50% missing observations

# Simulate full dataset
n <- 100
# You can set the seed for replication purposes
set.seed(35)
z1 = rnorm(n)
z2 = rnorm(n)
z3 = rnorm(n)
y1 = 1 + z1
y2 = 5 + 2*z1 + z2

# Check the theoretical mean, var, and corr
mean(y1)
mean(y2)
var(y1)
var(y2)
cor(y1, y2)
plot(y1, y2)

# a) Generate missingness with a = b = 0, which corresponds to MCAR

a = 0
b = 0
# u is a variable to decide if y is missing
u = a*(y1 - 1) + b*(y2 - 5) + z3

# Compare histograms of y1 and y2 for complete cases and missing data
hist(y1[u > 0])
hist(y1[u < 0], add = TRUE, border = "red")

hist(y2[u > 0])
hist(y2[u < 0], add = TRUE, border = "red")

plot(y1,y2)

# b) Carry out a t-test for the means of y1 for complete and missing data 
t.test(y1[u > 0], y1[u < 0])

# No evidence against H0 that the means are the same



# c) Repeat for a = 2, b = 0, which corresponds to MAR

a = 2
b = 0
u = a*(y1 - 1) + b*(y2 - 5) + z3

# Compare histograms of y1 and y2 for complete cases and missing data
hist(y1[u > 0])
hist(y1[u < 0], add = TRUE, border = "red")

hist(y2[u > 0])
hist(y2[u < 0], add = TRUE, border = "red")


# Carry out a t-test for the means of y1 for complete and missing data 
t.test(y1[u > 0], y1[u < 0])

# Now we reject the hypothesis of equal means!
# Note also the estimates of mean, var and corr:
mean(y2[u > 0])
var(y2[u > 0])
cor(y1[u > 0], y2[u > 0])



# Repeat for a = 0, b = 2, which corresponds to NMAR

a = 0
b = 2
u = a*(y1 - 1) + b*(y2 - 5) + z3

# Compare histograms of y1 and y2 for complete cases and missing data
hist(y1[u > 0])
hist(y1[u < 0], add = TRUE, border = "red")

hist(y2[u > 0])
hist(y2[u < 0], add = TRUE, border = "red")


# Carry out a t-test for the means of y1 for complete and missing data 
t.test(y2[u > 0], y2[u < 0])

# Now we reject the hypothesis of equal means!
# Note also the estimates of mean, var and corr:
mean(y2[u > 0])
var(y2[u > 0])
cor(y1[u > 0], y2[u > 0])







# Let's compare regression imputation under the three scenarios:

# MCAR:
a = 0
b = 0
u = a*(y1 - 1) + b*(y2 - 5) + z3

# Create a new variable y with missing values, so we keep original y2:
y = y2

# Decide on the proportion of missing:
p = 0.5

# Create NAs for y's which correspond to some u's:
y[u < qnorm(p)] = NA 

# Missing data indicator
Ry = as.numeric(!is.na(y))

# Complete cases
data.cc = cbind(y1,y)[Ry == 1, ]

# Removed cases due to missingness
data.dropped = cbind(y1, y)[Ry == 0, ]

# Regression of missing based on all completely observed vars:
reg = lm(y ~ y1, data = data.frame(data.cc))

# Predict the mising cases from the regression model:
y.imp = predict(reg, newdata = data.frame(data.dropped))

# Alternatively, you may use mice package command:
y.imp = mice.impute.norm.predict(y, !is.na(y), y1)

# Impute the predictions where they belong:
y[Ry == 0] = y.imp
plot(y1,y)

# Calculate SSE
sum((y-y2)^2)

# Compare regression after imputation to before missingness:
summary(lm(y1~y2))
summary(lm(y1~y))

# Repeat for MAR 
a = 2
b = 0
u = a*(y1 - 1) + b*(y2 - 5) + z3

# Create a new variable with missing values, so we keep original y2

y = y2
y[u < 0] = NA

# Missing data indicator
Ry = as.numeric(!is.na(y))

data.cc = cbind(y1,y)[Ry == 1, ]
data.dropped = cbind(y1, y)[Ry ==0, ]

reg = lm(y ~ y1, data = data.frame(data.cc))

y.imp = predict(reg, newdata = data.frame(data.dropped))
y[Ry == 0] = y.imp

# Calculate SSE
sum((y-y2)^2)
summary(lm(y1~y))




# Repeat for NMAR 
a = 0
b = 2
u = a*(y1 - 1) + b*(y2 - 5) + z3

# Create a new variable with missing values, so we keep original y2

y = y2
y[u < 0] = NA

# Missing data indicator
Ry = as.numeric(!is.na(y))

data.cc = cbind(y1,y)[Ry == 1, ]
data.dropped = cbind(y1, y)[Ry ==0, ]

reg = lm(y ~ y1, data = data.frame(data.cc))

y.imp = predict(reg, newdata = data.frame(data.dropped))
y[Ry == 0] = y.imp

# Calculate SSE
sum((y-y2)^2)
summary(lm(y1~y))
plot(y1, y)
cor(y,y1)

# Conclusion: Regression imputation works and produces smaller SEs in all three MDMs


# Regression imputation with noise:
# To add noise you need to still compute
# y.imp = predict(reg, newdata = data.frame(data.dropped))
# But have an extra line adding rnorm(length(y.imp),0,summary(reg)$sigma)


# Exercise 1: Calculate the mean, var, corr, and SSE when using regression imputation with noise and NMAR

a = 0
b = 2
u = a*(y1 - 1) + b*(y2 - 5) + z3



# Binary data regression imputation:

# First read some data from the internet:
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

# There are no missing values, but we will create about 10% within the admit variable

mydata.miss = mydata
u = runif(nrow(mydata))
mydata.miss$admit[(mydata$gre < 500) & (u < 0.5)] = NA
# Notice the decision was to remove randomly 50% of admit when gre < 500
# This shows how many are now missing among admit
summary(mydata.miss)

mydata.miss$rank <- factor(mydata$rank)

Ry = as.numeric(!is.na(mydata.miss$admit))
dat.cc = mydata.miss[Ry == 1, ]
dat.dropped = mydata.miss[Ry == 0, ]

# Now build the logistic model:

mylogit <- glm(admit ~ gre + gpa + rank, data = dat.cc, family = "binomial")
summary(mylogit)
# We now use the model to predict the missing
y.imp <- predict(mylogit, newdata = dat.dropped, type = "response")

# Note this returns the probability so we need to convert it to binary response before imputing it:
# without noise
mydata.miss$admit[Ry == 0] = round(y.imp,0)

# with noise
mydata.miss$admit[Ry == 0] = rbinom(sum(Ry==0), 1, y.imp)


# Exercise 2: Create 50% missingness in the rank variable when GPA > 3.6
# Then practice imputation with noise for categorical with more than one level 
# (pretend rank is unordered)