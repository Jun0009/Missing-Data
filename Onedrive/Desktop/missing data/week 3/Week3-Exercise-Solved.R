# Exercise: Repeat the above experiment about bias by
# a) using mean imputation
# b) using random imputation



## Mean imputation
mean.imp <- function (a){
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  return (imputed)
}


### Analysis function to calculate the regression coefficients
simFun.mis <- function(dat) 
{	
  y = 1 + 5*x + dat
  x[sample(1:n, 5)] = NA
  y[sample(1:n, 5)] = NA 
  x = mean.imp(x)
  y = mean.imp(y)
  reg = lm(y ~ x)
  summary(reg)$coef[1:2]
}

### Define number of replications 
R <- 10000

### Create a matrix to store output
# We need two columns
out.mis2 <- matrix(0, R, 2)


### Run the simulation within a for loop
for (i in 1:R) 
{
  out.mis2[i,] <- simFun.mis(rnorm(n))
}

### Plot the histogram

hist(out.mis2[,1])
mean(out.mis2[,1])
sd(out.mis2[,1])

hist(out.mis2[,2])
mean(out.mis2[,2])
sd(out.mis2[,2])



library(mice)



### Analysis function to calculate the regression coefficients
simFun.mis <- function(dat) 
{	
  y = 1 + 5*x + dat
  x[sample(1:n, 5)] = NA
  y[sample(1:n, 5)] = NA 
  d = data.frame(x, y)
  d.complete = complete(mice(data.frame(d), method = "sample", m = 1, maxit = 1))
  reg = lm(y ~ x, data = d.complete)
  summary(reg)$coef[1:2]
}

### Define number of replications 
R <- 1000

### Create a matrix to store output
# We need two columns
out.mis3 <- matrix(0, R, 2)


### Run the simulation within a for loop
for (i in 1:R) 
{
  out.mis3[i,] <- simFun.mis(rnorm(n))
}

### Plot the histograms

hist(out.mis3[,1])
mean(out.mis3[,1])
sd(out.mis3[,1])

hist(out.mis3[,2])
mean(out.mis3[,2])
sd(out.mis3[,2])

