# Practice for Quiz 3:

# Use the nhanes2 dataset from package mice
# The goal is to predict bmi based on age, hyp, chl

# a) What percent of cases is incomplete
1-sum(complete.cases(nhanes2))/nrow(nhanes2)
# 48% incomplete cases

# b) Impute the data using mice and seed = 1. 
# Pool the results and report the estimated model.
imp = mice(nhanes2, seed = 1)
fit = with(imp, lm(bmi ~ age + hyp + chl))
summary(mice::pool(fit))
# Estimated model is:
# bmi = 18.67 - 4.67*age2 - 6.55*age3 + 2.27*hyp2 + 0.05*chl

# c) Repeat with seed = 2. Are the results the same?
imp = mice(nhanes2, seed = 2)
fit = with(imp, lm(bmi ~ age + hyp + chl))
summary(mice::pool(fit))
# Estimated model is:
# bmi = 17.89 - 5.3*age2 - 7.21*age3 + 2.81*hyp2 + 0.06*ch

# d) Repeat with seed = 1 but now using m = 50. Report the estimated model.
imp = mice(nhanes2, seed = 1, m = 50)
fit = with(imp, lm(bmi ~ age + hyp + chl))
summary(mice::pool(fit))
# Estimated model is:
# bmi = 18.89 - 5.27*age2 -7*age3 + 1.92*hyp2 + 0.05*ch

# e) Now use seed = 1 but with mi package. Impute and report the estimates.
mdf <- missing_data.frame(nhanes2)
imp.nhanes <- mi(mdf, seed = 1, parallel = F)
converged <- mi2BUGS(imp.nhanes)
mean_bmi = converged[, , 1]
mean_chl = converged[, , 3]

par(mfrow = c(1,1))
ts.plot(mean_bmi[,1], col=1, ylim = c(-1, 1))
lines(mean_bmi[,2], col= 2)
lines(mean_bmi[,3], col= 3)
lines(mean_bmi[,4], col= 4)

ts.plot(mean_chl[,1], col=1)
lines(mean_chl[,2], col= 2)
lines(mean_chl[,3], col= 3)
lines(mean_chl[,4], col= 4)

res.nhanes = mi::pool(bmi ~ age + hyp + chl, data=imp.nhanes)
display(res.nhanes)
# Estimated model is:
# bmi = 25.76 - 4.14*age2 - 6.64*age3 + 1.28*hyp2 + 0.02*ch


# f) Run diagnostic plots. 
# Make one change (up to your discretion), repeat and report the results.
plot(imp.nhanes)

mdf <- change(mdf, y = c("bmi", "chl"), what = "imputation_method", to = "pmm")

imp.nhanes2 <- mi(mdf, seed = 1, n.iter = 2000, parallel = F)
converged <- mi2BUGS(imp.nhanes2)
mean_bmi = converged[, , 1]
mean_chl = converged[, , 3]
par(mfrow=c(1,1))

ts.plot(mean_bmi[,1], col=1)
lines(mean_bmi[,2], col= 2)
lines(mean_bmi[,3], col= 3)
lines(mean_bmi[,4], col= 4)

ts.plot(mean_chl[,1], col=1)
lines(mean_chl[,2], col= 2)
lines(mean_chl[,3], col= 3)
lines(mean_chl[,4], col= 4)
plot(imp.nhanes2)

res.nhanes2 = mi::pool(bmi ~ age + hyp + chl, data=imp.nhanes2)
display(res.nhanes2)

# Estimated model is:
# bmi = 20.38 - 5.23*age2 - 7.93*age3 + 2.25*hyp2 + 0.05*ch


# g) Compare to listwise deletion estimated regression.
summary(lm(bmi ~ age + hyp + chl, data = nhanes2))

# To call specific package use mice::pool(imp)