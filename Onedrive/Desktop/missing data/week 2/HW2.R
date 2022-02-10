#Missing Data Homework 2

#Read and inspect data set
data <- read.csv("C:/Users/joann/OneDrive/Desktop/missing data/week 1/missingdata_hw1.csv", na.strings = "")
str(data)

#Find variables with missing values
sapply(data, function(x) sum(is.na(x)))

#All variables with missing values are categorical
#I need to generate missing values for a continuous variable 

#Generate missing values for training_hours depending on one variable
library(dplyr)
data_new = select(data,'city_development_index','training_hours')
library(mice)
cont_cat = ampute(data_new,prop = 0.2,patterns=c(1,0),mech = "MAR")$amp
data['training_hours'] = cont_cat['training_hours']

#To keep it simple, I only use one categorical variable with missing variable for analysis
#The variable gender2 have missing value > 20%
data2 = select(data,'enrollee_id','city','city_development_index','training_hours','gender2','relevent_experience')
#Check for missingness
sapply(data2, function(x) sum(is.na(x)))

#Mean imputation for numeric missing value
mean.imp <- function (a)
{
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  # Output the imputed vector
  return (imputed)
}

data2['training_hours_imp']=mean.imp(data2['training_hours'])



#Mode imputation for the categorical variable
mode <- function (a)
{
  ta =table(a)
  tam = max(ta)
  if(all(ta==tam))
    mod =NA
  else
    mod = names(ta)[ta==tam]
  return (mod)
}

mode.imp <- function (a)
{
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mode(a.obs)
  # Output the imputed vector
  return (imputed)
}

data2['gender_imp']=mode.imp(data2['gender2'])

#Analysis with complete case 
data_complete = na.omit(data2)

#analyse the relationship between training hours and other factors

anova_one_way1 <- aov(training_hours~city_development_index+gender2,data=data_complete)
summary(anova_one_way1)
anova_one_way2 <- aov(training_hours_imp~city_development_index+gender_imp,data=data2)
summary(anova_one_way2)

#For the imputed data, the sum of square for city development index is much larger and the sum of square for the gender is smaller
#However, both variables does not pass the f test for both dataset
#The conclusion for both datasets is the same: we cannot reject the null hypothesis and different gender and city index does not impact the training hours
