# Generate MAR data from airquality
# install.packages("mice")
library(mice)

# First get just the fully observed data
air.miss = airquality[, 3:4]
a = ampute(air.miss)

# Put it back together with the other two variables
air.miss = cbind(airquality[, 1:2], a$amp)

# Check % missing
n = nrow(airquality)
sum(is.na(air.miss$Ozone))/n
sum(is.na(air.miss$Solar.R))/n
sum(is.na(air.miss$Temp))/n
sum(is.na(air.miss$Wind))/n

summary(air.miss)


# Using package mice

# Check the missing data pattern
md.pattern(air.miss, rotate.names = T)


# Impute missing data

imp = mice(air.miss, seed = 1)
summary(imp)

# To change number of iterations use maxit = 
# To change the number of chains use m = 

# Check prediction methods:
imp$pred
imp$method

fit = with(imp, lm(Ozone ~ Wind + Temp + Solar.R))
fit

# Pool together the results
pool(fit)
summary(pool(fit))

# riv is the relative increase in variance due to nonresponse r
# RIV = (V.B+V.B/)m/V.W
# This value can be interpreted as the proportional increase in the sampling variance of the parameter 
# of interest that is due to the missing data.

# You can change the number of imputed datasets with 
imp2 = mice(air.miss, m = 30, seed = 1)
# Check FMI again

fit = with(imp2, lm(Ozone ~ Wind + Temp + Solar.R))
pool(fit)
summary(pool(fit))

# To obtain just the fmi:
pool(fit)$pooled$fmi

# To check the rule of thumb:
pool(fit)$pooled$fmi/30
# Could be a bit better, but ok.

# To check imputed data:
imp$imp


# To obtain the complete data:

completedData <- complete(imp,1)
completedData

# To check imputed values
imp$imp$Ozone


# We can get some seriously cool plots as well
plot(imp)

library(lattice)
# Produces a conditional scatterplots. 
# The function automatically separates the observed (blue) and imputed (red) data.
xyplot(imp, Ozone~Wind+Temp+Solar.R, pch=18)

# Density estimates of observed and imputed:
densityplot(imp)

# Dotplots of observed and imputed:
stripplot(imp, pch=20)
# In ideal circumstances imputed (magenta) should have similar shape to observed (blue)

# Package VIM has very advanced graphs
install.packages("VIM")
library(VIM)
matrixplot(air.miss)

# Now using mi package
# install.packages("mi")
library(mi)
mdf <- missing_data.frame(air.miss)

summary(mdf)
image(mdf)
show(mdf)

tabulate(mdf@patterns)
levels(mdf@patterns) 

imp.air <- mi(mdf, seed = 124, parallel = F)
converged <- mi2BUGS(imp.air)

mean_Ozone = converged[, , 1]
mean_Solar.R = converged[, , 2]
mean_Wind = converged[, , 3]
mean_Temp = converged[, , 4]

ts.plot(mean_Ozone[,1], col=1)
lines(mean_Ozone[,2], col= 2)
lines(mean_Ozone[,3], col= 3)
lines(mean_Ozone[,4], col= 4)

plot(imp.air)


mdf <- change(mdf, y = "Ozone", what = "type", to = "pos")
show(mdf)
imp.air2 <- mi(mdf, seed = 124, parallel = F)
converged2 <- mi2BUGS(imp.air2)
plot(imp.air2)

mdf <- change(mdf, y = c("Solar.R"), what = "imputation_method", to = "pmm")
show(mdf)
imp.air3 <- mi(mdf, seed = 124, parallel = F)
plot(imp.air3)


air.comp1 = complete(imp.air3, m=1)
air.comp.1to5 = complete(imp.air3, m=5)
air.comp2 = air.comp.1to5[[2]]

lapply(air.comp.1to5, summary) 


par(mfrow=c(1,1))
par(ask = F)
summary(lm(Ozone ~ Wind + Temp + Solar.R, data=air.comp2))

# Scatterplot of completed dataset variables Solar.R vs. Ozone (same as xyplot)
plot(air.comp1$Solar.R, air.comp1$Ozone)

# Add red color to points which had Ozone missing
points(air.comp1$Solar.R[air.comp1$missing_Ozone==1],
	air.comp1$Ozone[air.comp1$missing_Ozone==1],col="red",pch=18)


res.air = mi::pool(Ozone ~ Wind + Temp + Solar.R, data=imp.air3)
display(res.air)


# Count data (van Buuren, Section 3.7.1)
set.seed( 1234 )
b0 <- 1
b1 <- .75
b2 <- -.25
b3 <- .5
N <- 5000
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
lam <- exp( b0 + b1 * x1 + b2 * x2 + b3 * x3 )
y <- rpois( N, lam )
POIS <- data.frame( y, x1, x2, x3 )
head(POIS)

## introduce MAR missingness to simulated data
generate.md <- function( data, pos = 1, Z = 2, pmis = .5, strength = c( .5, .5 ) ) 
{
  total <- round( pmis * nrow(data) )
  sm <- which( data[,Z] < mean( data[,Z] ) )
  gr <- which( data[,Z] > mean( data[,Z] ) )
  sel.sm <- sample( sm, round( strength[1] * total ) )
  sel.gr <- sample( gr, round( strength[2] * total ) )
  sel <- c( sel.sm, sel.gr )
  data[sel,pos] <- NA
  return(data)
}
MPOIS <- generate.md( POIS, pmis = .2, strength = c( .2, .8) )
head(MPOIS)

# You need to install countimp package from archive
library(countimp)
imp <- countimp( MPOIS, method = c( "poisson" ,"" ,"" ,"" ), print=FALSE)
repeated.analysis <- with(imp, glm(y~x1+x2+x3,family = poisson))
summary(mice::pool(repeated.analysis))


# Practice for Quiz 3:

# Use the nhanes2 dataset from package mice
# The goal is to predict bmi based on age, hyp, chl

# a) What percent of cases is incomplete
# b) Impute the data using mice and seed = 1. 
# Pool the results and report the estimated model.
# c) Repeat with seed = 2. Are the results the same?
# d) Repeat with seed = 1 but now using m = 50. Report the estimated model.
# e) Now use seed = 1 but with mi package. Impute and report the estimates.
# f) Run diagnostic plots. 
# Make one change (up to your discretion), repeat and report the results.
# g) Compare to listwise deletion estimated regression.

# To call specific package use mice::pool(imp)