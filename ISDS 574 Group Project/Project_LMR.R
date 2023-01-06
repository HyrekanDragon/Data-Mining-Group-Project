rm(list = ls()); gc()

setwd("C:/Users/Hyrek/OneDrive/Desktop/ISDS 574 Group Project")

student.por =read.table("cleaned_student_por.csv",sep=",",header=TRUE)
# Not sure why this was added, but this is just an index, doesnt need to be here
data = subset(student.por, select = -X)

# Set Indices
set.seed(9) # set a seed so that people get the same 80% next time they run the same code
n.train = floor( nrow(data)*.8 )
id.train = sample(1:nrow(data), n.train) # ncol() gives number of columns
id.test = setdiff(1:nrow(data), id.train) # setdiff gives the set difference

## Additional topic: variable selection (backward, forward, best subset)
# a good resource: http://www.stat.columbia.edu/~martin/W2024/R10.pdf
library(MASS)

### forward selection ###
## step 1: fit a null model and a full model first
obj.null = lm(G3 ~ 1, dat = data[id.train, ]) # only intercept, 1, is included in the model
data$school_MS = as.factor(data$school_MS) # tell R that these are categorical variables
data$sex_M	 = as.factor(data$sex_M)
data$address_R	 = as.factor(data$address_R)
data$famsize_LE3	 = as.factor(data$famsize_LE3)
data$Pstatus_A	 = as.factor(data$Pstatus_A)
data$Medu_College	 = as.factor(data$Medu_College)
data$Fedu_College	 = as.factor(data$Fedu_College)
data$Mjob_at_home	 = as.factor(data$Mjob_at_home)
data$Fjob_at_home	 = as.factor(data$Fjob_at_home)
data$reason_home	 = as.factor(data$reason_home)
data$reason_other	 = as.factor(data$reason_other)
data$reason_reputation	 = as.factor(data$reason_reputation)
data$guardian_father	 = as.factor(data$guardian_father)
data$guardian_other	 = as.factor(data$guardian_other)
data$schoolsup_yes	 = as.factor(data$schoolsup_yes)
data$famsup_no	 = as.factor(data$famsup_no)
data$paid_yes	 = as.factor(data$paid_yes)
data$activities_yes	 = as.factor(data$activities_yes)
data$nursery_no	 = as.factor(data$nursery_no)
data$higher_no	 = as.factor(data$higher_no)
data$internet_no	 = as.factor(data$internet_no)
data$romantic_yes	 = as.factor(data$romantic_yes)
data$traveltime_TravelGT30Mins	 = as.factor(data$traveltime_TravelGT30Mins)
data$studytime_StudyGT5Hrs	 = as.factor(data$studytime_StudyGT5Hrs)

obj.full = lm(G3 ~ ., dat = data[id.train, ])

## scope is to start from a null model and end with a full model; direction is forward
obj1 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward') # forward selection by Akaike information criterion (AIC)
summary(obj1) 
# Mallows's Cp is equivalent to AIC in the case of (Gaussian) linear regression

### backward elimination ###
obj2 = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward') # start with full and end with null; reversed comparing to forward
summary(obj2) 

### stepwise selection ###
obj3 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both') # start with full and end with null
summary(obj3)

####################################################
# compare prediction results of forward, backward, stepwise
require(hydroGOF)

# forward
yhat1 = predict(obj1, newdata = data[id.test, ])
rmse(data[id.test, 'G3'], yhat1) ## RMSE for test data

# backward
yhat2 = predict(obj2, newdata = data[id.test, ])
rmse(data[id.test, 'G3'], yhat2)

# stepwise
yhat3 = predict(obj3, newdata = data[id.test, ])
rmse(data[id.test, 'G3'], yhat3)

#All 3 models are the same, RMSE = 2.844952

#Scatterplot to check accuracy
ytest = data[id.test,c('G3')]
plot(ytest, yhat3)
abline(0, 1, col='red')
