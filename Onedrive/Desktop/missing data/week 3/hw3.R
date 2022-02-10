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

#check again for the generated missing values
sapply(data2, function(x) sum(is.na(x)))

# regression imputation for the numeric variable
#variables without missing values are: target and city_development_index
data3 = select(data2,'city_development_index','training_hours','target')
data3 =complete(mice(data.frame(data3), method = "norm.predict", m = 1, maxit = 1))
data2['training_hours']= data3['training_hours']


#listwise deletion for the missing categorical values
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
#the variables training hours and education level are significant in the resulting 
#dataset but not significant in the complete data set
#However, the R-square value is larger for the complete data set 
#this implies that the linear regression model fits better for the complete data set
