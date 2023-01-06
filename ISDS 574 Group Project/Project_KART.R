rm(list = ls()); gc()
library(rpart); library(rpart.plot)

setwd("C:/Users/Hyrek/OneDrive/Desktop/ISDS 574 Group Project")

student.por =read.table("cleaned_student_por.csv",sep=",",header=TRUE)

data = subset(student.por, select = -X)

################################################
# Regression Tree Example
K = 10 # number of cross-validations

fit = rpart(formula = G3 ~ failures + higher_no + school_MS + Dalc + health + 
              studytime_StudyGT5Hrs + schoolsup_yes + reason_other + Medu_College + 
              sex_M + address_R + reason_reputation + guardian_father, 
            method="anova", data=data, xval=K)

data

# Minimum Error Tree
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')

# Best Pruned Tree
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(K) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')

## How to predict? I am taking best pruned tree as an example.
yhat = predict(pfit.bp, data) # replace "dat" by validation data if you have it

rmse = function(x, y) sqrt( mean((x-y)^2) ) # I wrote a function to calculate RMSE
#RMSE for Best Pruned Tree
rmse(yhat, data$G3)
plot(data$G3, yhat) # check the scatter plot, not so good
abline(0, 1, col='red')

## How to predict? I am taking best pruned tree as an example.
yhat2 = predict(pfit.me, data) # replace "dat" by validation data if you have it
#RMSE for Minimum Error Tree
rmse(yhat2, data$G3)
plot(data$G3, yhat2) # check the scatter plot, not so good
abline(0, 1, col='red')