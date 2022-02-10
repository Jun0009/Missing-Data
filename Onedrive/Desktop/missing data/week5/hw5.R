#Missing Data Homework 5
#Read and inspect data set
data <-read.csv("C:/Users/joann/OneDrive/Desktop/missing data/week 2/aug_train.csv", 
                 na.strings = "")

#Encode character variables

unique(data$relevent_experience )
library(plyr)
data$relevent_experience <- revalue(data$relevent_experience, 
                                    c("Has relevent experience"=1))
data$relevent_experience <- revalue(data$relevent_experience, 
                                    c("No relevent experience"=0))
data$relevent_experience <-as.numeric(data$relevent_experience)

unique(data$last_new_job)
data$last_new_job <- revalue(data$last_new_job, c("never"=0))
data$last_new_job <- revalue(data$last_new_job, c(">4"=5))
data$last_new_job <-as.numeric(data$last_new_job)

unique(data$enrolled_university )
data$enrolled_university <- revalue(data$enrolled_university, 
                                    c("no_enrollment"=0))
data$enrolled_university <- revalue(data$enrolled_university, 
                                    c("Part time course"=1))
data$enrolled_university <- revalue(data$enrolled_university,
                                    c("Full time course" = 2))
data$enrolled_university <-as.numeric(data$enrolled_university)

unique(data$education_level)
data$education_level <- as.numeric(factor(data$education_level, 
                                          levels = c("Primary School",
                                                     "High School","Graduate",
                                                     "Masters","Phd")))
unique(data$company_size)
data$company_size <- as.numeric(factor(data$company_size, 
                                          levels = c("<10","10/49" ,"50-99" ,
                                                     "100-500","500-999","1000-4999",
                                                     "5000-9999", "10000+" )))

unique(data$gender) 
data$gender <- as.factor(data$gender)

#I will keep the variables that can be used for my analysis
library(dplyr)
data2 = select(data,'training_hours','gender','relevent_experience',
               'last_new_job','enrolled_university','education_level','target')

#Generate missing values for training_hours depending on one variable
library(dplyr)
data_new = select(data,'city_development_index','training_hours')
library(mice)
cont_cat = ampute(data_new,prop = 0.2,patterns=c(1,0),mech = "MAR")$amp
data2['training_hours'] = cont_cat['training_hours']


#check again for the generated missing values
sapply(data2, function(x) sum(is.na(x)))


#Q1

library(mi)
#Run mi with 5 chains and 50 iterations on the dataset

# Create the missing data frame object
mdf = missing_data.frame(data2)

# Examine the default settings
show(mdf)

# Running the chains
imputations <- mi(mdf, n.chains = 5, n.iter=50,max.minutes = 20)

#Q2

#Check convergence/diagnostics and make changes if necessary
Rhats(imputations)
round(mipply(imputations, mean, to.matrix = TRUE), 3)

#make changes for last_new_job and training hours
#since they have unequal means for each chain 
#the inspected problem is on the type of these two variables
#training hours are always >0 and last new job is a ordered categorical variable
mdf <- change(mdf, y = "last_new_job", what = "type", 
              to = "ordered-categorical")
mdf <- change(mdf, y = "training_hours", what = "type", to = "pos")
show(mdf)
# Rerunning the chains
imputations <- mi(mdf, n.chains = 5, n.iter=50)
round(mipply(imputations, mean, to.matrix = TRUE), 3)
converged <- mi2BUGS(imputations)
Rhats(imputations)
mean_th = converged[, , 1]
# Traceplot of mean imputed training hours
ts.plot(mean_th[,1], col=1)
lines(mean_th[,2], col= 2)
lines(mean_th[,3], col= 3)
lines(mean_th [,4], col= 4)
lines(mean_th [,5], col= 5)

mean_l = converged[, , 4]
# Traceplot of mean imputed last new job
ts.plot(mean_l[,1], col=1)
lines(mean_l[,2], col= 2)
lines(mean_l[,3], col= 3)
lines(mean_l [,4], col= 4)
lines(mean_l [,5], col= 5)

#check for the plots 
plot(imputations)

#the results converged 

#Q3

#Pool the results and report the estimated equation
analysis <- pool(target ~ training_hours+gender+relevent_experience+
                   last_new_job+enrolled_university+education_level,imputations)
display(analysis)

#the estimated equation of the pooled result is as shown above 

#Q4
#compare pooled result with complete dataset result
#original complete data set
data_complete = na.omit(data)

#Linear regression analysis for the target variable
#0-not looking for a job change 1-looking for a job change


#model with the original complete data cases
model2 = lm(target ~ training_hours+gender+relevent_experience+last_new_job+
              enrolled_university+education_level, data = data_complete)
summary(model2)


#Comparing the two results:
#the estimated coefficients for the original complete data set are all small 
#numbers and the 
