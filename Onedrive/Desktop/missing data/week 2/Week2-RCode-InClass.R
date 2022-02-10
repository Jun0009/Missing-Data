# Listwise deletion
(DF = data.frame(x = c(1, 2, 3), y = c(0, 10, NA)))
na.omit(DF)

# Alternative method

(mat = matrix(c(1, NA, 3, 4, 5, 6), 3, 2))
# This tells you which row is a complete case
complete.cases(mat)

# Then retain only the complete cases
mat[complete.cases(mat),]

# Complete variables
mat[, complete.cases(t(mat))]

# Mean imputation function
# The input is a=data vector

mean.imp <- function (a)
  {
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  # Output the imputed vector
  return (imputed)
}

# Sampling is used to select random elements from a vector:
sample(1:15, 5)

# Try it:
x = 1:20 # This is just a vector with first 20 natural numbers
hist(x)
# Make 4 of these missing
miss = sample(1:20, 4)
x.mcar = x
x.mcar[miss] <-NA
x.mcar
hist(x.mcar)

# Impute with mean

mean.imp(x.mcar)
x.car.mean.imp = mean.imp(x.mcar)
hist(x.car.mean.imp)

# Exercise: Try with MNAR mechanism. For example remove only the small values

x.mnar = 21:40 + rnorm(20)
x.mnar[1:4] = NA
# Finish by imputing the mean ...
hist(mean.imp(x.mnar))

# Alternative: install package mice
install.packages("mice")

# Load the package
library(mice)
d = cbind(x.mcar, x.mnar)
d
out = mice(data.frame(d), method = "mean", m = 1, maxit = 1)
complete(out)

# Real data example
## Read the the Social Indicators Survey data 
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sis
# Choose file siswave3v4impute3.csv

wave3 <- read.table (file.choose(), header=T, sep=",")

# The following makes the names of the variables available for direct reference
attach(wave3)

# Sample size or number of cases
n <- nrow (wave3)

# earnings variables:

# rearn:  respondent's earnings
# tearn:  spouse's earnings
# workmos:  primary earner's months worked last year


# set up some simplified variables to work with
# returns NA of argument is negative or equal to 999999
na.fix <- function (a) 
{
  ifelse (a<0 | a==999999, NA, a)
}

# Cleaning the data
earnings <- na.fix(rearn) + na.fix(tearn)

# sets earnings equal to 0 for those who worked 0 months
earnings[workmos==0] <- 0

# rescaling the earnings
earnings <- earnings/1000

# Showing some missing values on rows 91 - 95
cbind (sex, race, educ_r, r_age, earnings, police)[91:95,]

hist(earnings)


earnings.imp.mean <- mean.imp (earnings)

hist(earnings.imp.mean)

# Now the missing values are replaced with the mean
cbind (sex, race, educ_r, r_age, earnings.imp.mean, police)[91:95,]





## Simple random imputation
random.imp <- function (a)
{
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs = a[!missing] # Observed data
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

earnings.imp <- random.imp (earnings)

# Now the missing values are replaced with the data generated from sample
cbind (sex, race, educ_r, r_age, earnings.imp, police)[91:95,]



# Exercise:
# Apply mean and random imputations to the airquality data 
# to impute values for the missing Ozone levels
# Plot a histogram before and after the imputation




# LVCF for airquality data
a5 = airquality[,1]

for (i in 1:length(a5)) if (is.na(a5[i])) a5[i] = a5[i-1]


# or keep original Ozone
airquality.lvcf = cbind(a5,airquality)

# NVCB
attach(airquality)
a6 = Ozone

for (i in length(a6):1) 
{

if (is.na(a6[i])) a6[i] = a6[i+1]
}
# or keep original Ozone
airquality.nvcb = cbind(a6,airquality)


# Dummy variable approach for airquality
attach(airquality)

plot(Ozone, Temp)

# Regular regression
summary(lm(Temp ~ Ozone))

# Dummy variable imputation method
a3 = mean.imp(Ozone)
d = is.na(Ozone)
summary(lm(Temp ~ a3 + d))

# Dummy variable adjustment by inserting zeros
zero.imp <- function (a)
{
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- 0
  return (imputed)
}
a4 = zero.imp(Ozone)
d = is.na(Ozone)
summary(lm(Temp ~ a4 + d))


# Including one more predictor
summary(lm(Temp ~ a3 + d + Wind))

# Compare to listwise deletion
summary(lm(Temp ~ Ozone + Wind))


# Inverse logistic curve
ilogit = function(x) exp(x)/(1+exp(x))
curve(ilogit(.2*x),-20,20)

# Let's use the built-in cars dataset

# Choose some constants that will not give you too much missing:
p = ilogit((cars$speed- mean(cars$speed))/5 -2)
(R = rbinom(length(cars$speed), 1, p))
mean(R)

# Finally make dist missing for those R:
y = cars$dist
y[R==1] = NA
y

# Create a new dataset with the missing:
cars.miss = cbind(cars$speed, y)
colnames(cars.miss) = c("speed", "dist")
cars.miss

# Alternative method to be used for your project:
# use ampute function in mice package
n = 50
x1 = runif(n)
x2 = 2 + x1 + rnorm(n, sd=.5)
y = 1 + 5*x1 +6*x2 + rnorm(n)
(m = cbind(y,x1,x2))
ampute(m)
ampute(m, prop = 0.6)$amp