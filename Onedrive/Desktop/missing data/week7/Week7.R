# Gibbs sampler example

library(MASS)
# Means and standard deviations of the bivariate normal data
m1 <- 1; m2 <- 1
s1 <- 1
s2 <- 22

# Correlation between the two components
r <- 0.9

# Covariance matrix
s <- matrix(c(s1^2, r*s1*s2, r*s1*s2, s2^2),2,2)
m <- c(m1,m2)

# Generate draw from the bivariate normal
z <- mvrnorm(1000,m,s)
plot(z,col="grey",pch=20)

# Gibbs sampler to simulate this data
iter <- 1000
x <- 1:iter
y <- 1:iter
x[1] <- -1
for (i in 1:iter)
  {
    y[i] <- rnorm(1, m2+s2*r*(x[i]-m1)/s1, sqrt((1-r^2)*s2^2))
    if (i < iter) x[i+1] <- rnorm(1, m1+s1*r*(y[i]-m2)/s2, sqrt((1-r^2)*s1^2))
  }

# Compare with package generator:
plot(z,col="grey",pch=20, main = "r = 0.9")
points(x,y,col="red", pch=20)

ts.plot(x)

# Now change r to 0.999 and repeat!

library(mice)
# VIM package examples
library(VIM)

# This plot gives the frequencies for different combination of missing variables: 
nhanes_aggr = aggr(nhanes, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# Missing patterns
matrixplot(nhanes)

# The margin scatterplot of the pairs
marginplot(nhanes[, c("chl", "bmi")],col = mdc(1:2), cex.numbers = 1.2,pch = 19)


# Hotdeck function within VIM package:
hotdeck(nhanes2)


# Convergence plot of mice package:
imp20 = mice(nhanes, m=20, printFlag=FALSE, maxit = 30, seed=2525)
fit.mi20 = with(data=imp20, exp = lm(chl ~ age + bmi))
combFit = pool(fit.mi20)
summary(combFit)
plot(imp20) 

# Comparing models:
fit1 = with(data = imp20, expr = lm(chl ~ age))
fit2 = with(data = imp20, expr = lm(chl ~ age + bmi))
# Wald test
stat = pool.compare(fit2, fit1, method = "wald")
# P-value of the test
stat$p



# HotDeckImputation package examples:

# Mean imputation:

#Set the random seed to an arbitrary number
set.seed(421)

#Generate matrix of random integers
Y <- matrix(sample(0:9,replace=TRUE,size=6*3),nrow=6)

#generate 6 missing values, MCAR, in all but the first row

Y[-1,][sample(1:12,size=6)]<-NA
Y
#Impute the colMeans of Y

# install.packages("HotDeckImputation")
# Install from file
library(HotDeckImputation)

impute.mean(DATA=Y)
# Check it worked, for example the first column:
mean(Y[,1], na.rm = T)


# Nearest neighbor hotdecking
set.seed(421)

#Generate random integer matrix size 10x4
Y <- matrix(sample(x=1:100,size=10*4),nrow=10)

#remove 5 values, ensuring one complete covariate and 5 donors
Y[-c(1:5),-1][sample(1:15,size=5)]<-NA
Y

#Impute using various different settings
Y = as.data.frame(Y)
# Using Manhattan distance and variance as weight to rescale variables:
impute.NN_HD(DATA=Y, distance="man", weights="var")

# Check with dist function:
# For example, when imputing the 7th row missing value in last column:
dist(Y[,-4], method = "manhattan")
# Closest match is in row 1, which is exactly what was imputed
# Note in this manual comparison we ignored the weight being the variance 
# as it is not as easy to apply it manually

# Using donor_limit restricts how many times an observation can be used as a donor for missing value
impute.NN_HD(DATA=Y, distance=2, weights=rep(.5,4), donor_limit=2, optimal_donor="mmin")

# You can impute something, like the mean, before distance computation
impute.NN_HD(DATA=Y,distance="eukl",weights=.25,comp="mean",donor_limit=1, optimal_donor="odd")

# Recover some diagnostics
impute.NN_HD(DATA=Y,distance="eukl",weights=.25,comp="mean",donor_limit=1,
optimal_donor="odd",diagnose = "diagnostics")
# look at the diagnostics
diagnostics



# Multilevel example from van Buuren, 2nd edition:
d <- brandsma[, c("sch", "lpo")]
md.pattern(d, plot = FALSE)

install.packages("miceadds")
library(miceadds)
methods <- c("sample", "pmm", "2l.pan", "2l.norm", "2l.pmm")
result <- vector("list", length(methods))
names(result) <- methods
for (meth in methods) 
{
  d <- brandsma[, c("sch", "lpo")]
  pred <- make.predictorMatrix(d)
  pred["lpo", "sch"] <- -2
  result[[meth]] <- mice(d, pred = pred, meth = meth,
                         m = 10, maxit = 1,
                         print = FALSE, seed = 82828)
}

# Check which methods imputes better compared to observed data
densityplot(result[[3]])
densityplot(result[[5]])
# pmm tracks better the observed distribution