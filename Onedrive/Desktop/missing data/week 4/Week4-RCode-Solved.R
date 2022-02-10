# Insert this code into the In-Class code:


# Exercise: Calculate the mean, var, corr, and SSE when using regression imputation with noise



# For NMAR (other cases done similarly)

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


noise = rnorm(length(y.imp), 0, summary(reg)$sigma)
# Just add the random normal draw to your predicted values
y.imps = y.imp + noise
y[Ry == 0] = y.imps

sum((y-y2)^2)
summary(lm(y1~y))
cor(y,y1)

# Exercise: Create 50% missingness in the rank variable when GPA > 3.6
# Then practice imputation for categorical with more than one level (pretend rank is unordered)

u = runif(nrow(mydata))
mydata.miss$rank[(mydata$gpa > 3.6) & (u < 0.5)] = NA
summary(mydata.miss)

library(nnet)
R = complete.cases(mydata.miss)
mydata.cc = mydata.miss[R,]
mydata.dropped = mydata.miss[!R,]

mmod = multinom(rank ~ gre + gpa, data = mydata.cc)
# Make sure rank is a factor
ps = predict(mmod, type = "prob", newdata = mydata.dropped)
ps

k = 4    # k is the number of categories
(nimp = sum(is.na(mydata.miss$rank)))

cat.imps = numeric(nimp)    
# nimp is the number to be imputed (sum(Ry==0))
for(i in 1:nimp)  cat.imps[i] = sum(rmultinom(1, 1, ps[i,])*c(1:k))
cat.imps
