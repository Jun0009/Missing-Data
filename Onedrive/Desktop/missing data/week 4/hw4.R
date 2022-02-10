#Missing Data Homework 4

#Read and inspect data set
data <- read.csv("C:/Users/joann/OneDrive/Desktop/missing data/week 2/aug_train.csv", na.strings = "")
str(data)
#check for missing values
sapply(data, function(x) sum(is.na(x)))
#the variables contain missing values are all categorical


#Encode character variables

unique(data$relevent_experience )
library(plyr)
data$relevent_experience <- revalue(data$relevent_experience, c("Has relevent experience"=1))
data$relevent_experience <- revalue(data$relevent_experience, c("No relevent experience"=0))
data$relevent_experience <-as.numeric(data$relevent_experience)

unique(data$last_new_job)
data$last_new_job <- revalue(data$last_new_job, c("never"=0))
data$last_new_job <- revalue(data$last_new_job, c(">4"=5))
data$last_new_job <-as.numeric(data$last_new_job)

unique(data$enrolled_university )
data$enrolled_university <- revalue(data$enrolled_university, c("no_enrollment"=0))
data$enrolled_university <- revalue(data$enrolled_university, c("Part time course"=1))
data$enrolled_university <- revalue(data$enrolled_university,c("Full time course" = 2))
data$enrolled_university <-as.numeric(data$enrolled_university)

unique(data$education_level)
data$education_level <- as.numeric(factor(data$education_level, 
                                          levels = c("Primary School",
                                                     "High School","Graduate",
                                                     "Masters","Phd")))
unique(data$gender) 
data$gender <- as.factor(data$gender)

#I will keep the variables that can be used for my analysis
library(dplyr)
data2 = select(data,'city_development_index','training_hours','gender','relevent_experience',
               'last_new_job','enrolled_university','education_level','target')

#Generate missing values for training_hours depending on one variable
library(dplyr)
data_new = select(data2,'city_development_index','training_hours')
library(mice)
cont_cat = ampute(data_new,prop = 0.2,patterns=c(1,0),mech = "MAR")$amp
data2['training_hours'] = cont_cat['training_hours']


#since the homework requires imputation for dichotomous variable, I also need to 
#generate missing values for relevent_experience (the only dichotomous variable)
#note that gender is not dichotomous in this data set
data_new2 = select(data2,'city_development_index','relevent_experience')
cont_cat2 = ampute(data_new2,prop = 0.2,patterns=c(1,0),mech = "MAR")$amp
data2['relevent_experience'] = cont_cat2['relevent_experience']

#check again for the generated missing values
sapply(data2, function(x) sum(is.na(x)))


#Q1
#regression imputation with noise for the numeric variable
#variables without missing values are: target,city_development_index
data3 = select(data2,'city_development_index','training_hours','target')

# Missing data indicator
Ry = as.numeric(!is.na(data3$training_hours))

data.cc = data3[Ry ==1, ]
data.dropped = data3[Ry ==0, ]

reg = lm(training_hours ~ city_development_index+target,data = data.cc)

y.imp = predict(reg, newdata = data.dropped)


noise = rnorm(length(y.imp), 0, summary(reg)$sigma)
#add noise to model
y.imps = y.imp + noise
data3$training_hours[Ry == 0] = y.imps
data2['training_hours']= data3['training_hours']

#Q2

#the dichotomous variable:logistic regression imputation with noise
#select data set with full variables 
data4 = select(data2,'city_development_index','relevent_experience','target')

#Missing data indicator
Ry2 = as.numeric(!is.na(data4$relevent_experience))
dat.cc = data4[Ry2 == 1, ]
dat.dropped = data4[Ry2 == 0, ]

# Now build the logistic model:

mylogit <- glm(relevent_experience ~ city_development_index+ target, 
               data = dat.cc, family = "binomial")
summary(mylogit)
# We now use the model to predict the missing
y.imp2 <- predict(mylogit, newdata = dat.dropped, type = "response")

# with noise
data4$gender[Ry2 == 0] = rbinom(sum(Ry2==0), 1, y.imp2)

#replace the imputed variable to dataset 2
data2['relevent_experience']= data4['relevent_experience']



#Q3

#listwise deletion for all other missing categorical values
data2 = na.omit(data2)

#original complete data set
data_complete = na.omit(data)

#Linear regression analysis for the target variable
#0-not looking for a job change 1-looking for a job change
#model with the resulting data set
model1 = lm(target ~ city_development_index+training_hours+gender+relevent_experience+
              last_new_job+enrolled_university+education_level, data = data2)
summary(model1)


#model with the original complete data cases
model2 = lm(target ~ city_development_index+training_hours+gender+relevent_experience+
              last_new_job+enrolled_university+education_level, data = data_complete)
summary(model2)


#Comparing the two results:
#the variables gender  and education level are statistically significant in the 
#resulting dataset but not significant in the complete data set
#training hours is not significant in either data set
#However, the R-square value is larger for the complete data set 
#this implies that the linear regression model fits better for the complete data set
