
rm(list=ls()); gc()
setwd("C:/Users/Hyrek/OneDrive/Desktop/ISDS 574 Group Project")

student.por =read.table("cleaned_student_por.csv",sep=",",header=TRUE)

dat = subset(student.por, select = -X)
## If predictors are very different in scales,
## standardize them to have comparable scales.
## The code to do so is as follows:
dat[,!colnames(dat) %in% c("G3")] = scale(dat[,!colnames(dat) %in% c("G3")])

## simulate a data set
set.seed(9)
error = rnorm( nrow(dat) )
# This is for student.por
dat$G3 = 14.06420 - dat$failures*1.56743 - dat$higher_no*2.25467 - dat$school_MS*1.07593 - dat$Dalc*0.31749 - dat$health*0.22021 + dat$studytime_StudyGT5Hrs*.81581 - dat$schoolsup_yes*1.08314 - dat$reason_other*.97016 + dat$Medu_College*.73840 - dat$sex_M*.47239 - dat$address_R*.54745 + dat$reason_reputation*.57637 + dat$guardian_father*.43476 + error

# detach("package:class", unload=TRUE)
##### functions needed for knn prediction #######
library(dplyr)
library(FNN)
one.pred = function(xnew, xtrain, ytrain, k, algorithm) {
  ind = knnx.index(xtrain, matrix(xnew, 1), k=k, algorithm=algorithm)
  mean(ytrain[ind])
}

knn.predict = function(Xtrain, ytrain, Xtest, k=5, algorithm = 'kd_tree') {
  ypred = apply(Xtest, 1, one.pred, xtrain = Xtrain, ytrain = ytrain, k=k, algorithm=algorithm)
  return(ypred)
}

knn.predict.bestK = function(Xtrain, ytrain, Xtest, ytest, k.grid = 1:20, algorithm='kd_tree') {
  fun.tmp = function(x) {
    yhat = knn.predict(Xtrain, ytrain, Xtest, k = x, algorithm=algorithm) # run knn for each k in k.grid
    rmse = (yhat - ytest)^2 %>% mean() %>% sqrt()
    return(rmse)
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error),
             error.all = error)
  return(out)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct ) + 1
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}
#################################################

set.seed(9)
n.train = floor( nrow(dat)*0.8 )
ind.train = sample(1:nrow(dat), n.train)
ind.test = setdiff(1:nrow(dat), ind.train)

Xtrain = dat[ind.train,!colnames(dat) %in% c("G3")]
Xtest = dat[ind.test,!colnames(dat) %in% c("G3")]
ytrain = dat[ind.train,c('G3')]
ytest = dat[ind.test,c('G3')]

ypred = knn.predict(Xtrain, ytrain, Xtest, k = 3)
plot(ytest, ypred)
abline(0, 1, col='red')

obj1 = knn.predict.bestK(Xtrain, ytrain, Xtest, ytest, k.grid = 1:53) ## best k = 5
obj1

## rerun with the best k
ypred1 = knn.predict(Xtrain, ytrain, Xtest, k = obj1$k.optimal)
plot(ytest, ypred1)
abline(0, 1, col='red')

sqrt(mean((ytest - ypred1)^2))#RMSE = 2.364477





