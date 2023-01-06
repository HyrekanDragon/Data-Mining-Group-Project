#Link to dataset
#http://archive.ics.uci.edu/ml/datasets/Student+Performance

rm( list=ls() ) # remove all existing objects in the environment
gc() # garbage collection

#Set Directory
setwd("C:/Users/Hyrek/OneDrive/Desktop/ISDS 574 Group Project")

#Read the table
student=read.table("student-por.csv",sep=";",header=TRUE)
View(student)

## check missing ##
matrix.na = is.na(student)
pmiss = colMeans(matrix.na) # proportion of missing for each column
nmiss = rowMeans(matrix.na) # proportion of missing for each row
plot(pmiss) # a few columns with high proportion of missing. we want to exclude them.

library(Amelia)
dev.new()
missmap(student)  #There is no missing data values, so we can move on the next step

## check the distribution of continuous variables ##

#Range 15 to 22, most students under the age of 18, potential outliers at age 22
# Might want to keep because range is still relatively small
dev.new()
par(mfrow=c(1, 2))
hist(student$age)
boxplot(student$age)

# 1: < 15 mins
# 2: 15 - 30 mins
# 3: 30 mins to an hour
# 4: > hour
# A majority of students make it to school in under 30  minutes
# Student who take over a hour to make it school are considered outliers according
# To the data but the range is far too small to make 
dev.new()                       #Open a new window
par(mfrow=c(1, 2))              #The next two plots will be displayed over a 1 row and 2 columns
hist(student$traveltime)    #Draw a historgram and boxplot of the given data
boxplot(student$traveltime)

id1 = which(student$traveltime %in% c(1, 2))
id2 = which(student$traveltime %in% c(3, 4))
student$traveltime[id1] = 'TravelLT30Mins'
student$traveltime[id2] = 'TravelGT30Mins'
table(student$traveltime)

# 1: < 2 hours
# 2: 2-5 hours
# 3: 5-10 hours
# 4: > 10 hours
# Most students study for 5 hours or less
# Potential outliers for studnets who spend over 10 hours studying
dev.new()
par(mfrow=c(1, 2))
hist(student$studytime)
boxplot(student$studytime)

#Consolidation
id1 = which(student$studytime %in% c(1, 2))
id2 = which(student$studytime %in% c(3, 4))
student$studytime[id1] = 'StudyLT5Hrs'
student$studytime[id2] = 'StudyGT5Hrs'
table(student$studytime)

#Range from from 1-4, 4 can mean 4+ failures
# As expected pretty much most students pass thier classes, studets who failed 4+ times
# are potetial outliers, but there doesnt seem to be any in the dataset
dev.new()
par(mfrow=c(1, 2))
hist(student$failures)
boxplot(student$failures)

# Scale from 1, very bad, to 5 excellent
# Most studetns have a good relationship with parents
dev.new()
par(mfrow=c(1, 2))
hist(student$famrel)
boxplot(student$famrel)

dev.new()
par(mfrow=c(1, 2))
hist(student$freetime)
boxplot(student$freetime)

dev.new()
par(mfrow=c(1, 2))
hist(student$goout)
boxplot(student$goout)

# Most student's dont drinkk during the weekday
dev.new()
par(mfrow=c(1, 2))
hist(student$Dalc)
boxplot(student$Dalc)

# Student's are more likley to drink on the weekend, but most students dont drink in general
dev.new()
par(mfrow=c(1, 2))
hist(student$Walc)
boxplot(student$Walc)

dev.new()
par(mfrow=c(1, 2))
hist(student$health)
boxplot(student$health)

# Most students dont miss more than 5 days of school, IQR is about 0-15 days
dev.new()
par(mfrow=c(1, 2))
hist(student$absences)
boxplot(student$absences)

## check the frequency table of categorical variables
## A lot of these are just binary options, so there isn't much to analyze here
dev.new()
plot(table(student$school))

dev.new()
plot(table(student$sex))

dev.new()
plot(table(student$address))

dev.new()
plot(table(student$famsize))

dev.new()
plot(table(student$Pstatus))

# These aren't binary
dev.new()
plot(table(student$Medu))

dev.new()
plot(table(student$Fedu))

dev.new()
plot(table(student$Mjob))

dev.new()
plot(table(student$Fjob))
# Codense down vaiables down to at home (no job) and at work (job)

dev.new()
plot(table(student$reason))

# Thre rest of these are binary
dev.new()
plot(table(student$guardian))

dev.new()
plot(table(student$schoolsup))

dev.new()
plot(table(student$famsup))

dev.new()
plot(table(student$paid))

dev.new()
plot(table(student$activities))

dev.new()
plot(table(student$nursery))

dev.new()
plot(table(student$higher))

dev.new()
plot(table(student$internet))

dev.new()
plot(table(student$romantic))

#Combine Categories Together
id = which(student$Mjob %in% c('health','other','services','teacher'))
student$Mjob[id] = 'has_job'
table(student$Mjob)

id = which(student$Fjob %in% c('health','other','services','teacher'))
student$Fjob[id] = 'has_job'
table(student$Fjob)

id1 = which(student$Medu %in% c(0, 1, 2, 3))
id2 = which(student$Medu %in% c(4))
student$Medu[id1] = 'HS_or_lower'
student$Medu[id2] = 'College'
table(student$Medu)

id1 = which(student$Fedu %in% c(0, 1, 2, 3))
id2 = which(student$Fedu %in% c(4))
student$Fedu[id1] = 'HS_or_lower'
student$Fedu[id2] = 'College'
table(student$Fedu)


# Turn categorical variables into dummy variables
library(fastDummies)

student2 = dummy_columns(student,
                             select_columns = c('school', 'sex', 'address', 'famsize', 'Pstatus', 'Medu',
                                                'Fedu', 'Mjob', 'Fjob', 'reason', 'guardian', 'schoolsup', 'famsup', 'paid',
                                                'activities', 'nursery', 'higher', 'internet', 'romantic', 'traveltime', 'studytime'),
                             remove_most_frequent_dummy = T,
                             remove_selected_columns = T)

#Cleanup; remove unnecessary data
rm(student, matrix.na, nmiss, pmiss); gc();

dev.new()
View(round(cor(student2), 2))

#Check Correlation 
library(corrplot)
dev.new()
par(mfrow = c(1, 1))
corrplot(cor(na.omit(student2)), method="number", number.cex = .6)

#Checks the grades
dev.new()
par(mfrow=c(1, 2))
hist(student2$G1)

dev.new()
par(mfrow=c(1, 2))
hist(student2$G2)

dev.new()
par(mfrow=c(1, 2))
hist(student2$G3)

#Remove G1 and G2, complicates model
student3 <- subset(student2, select = -c(G1, G2))

write.csv(student3, "cleaned_student_por.csv")

rm( list=ls() ) # remove all existing objects in the environment
gc() # garbage collection

#Check to see that table was written correctly
student.por =read.table("cleaned_student_por.csv",sep=",",header=TRUE)

#Box plot checks for whether to treat as categorical or numerical
boxplot(G3 ~ Dalc, data = student.por)
boxplot(G3 ~ Walc, data = student.por)
boxplot(G3 ~ health, data = student.por)
boxplot(G3 ~ freetime, data = student.por)
boxplot(G3 ~ goout, data = student.por)
boxplot(G3 ~ famrel, data = student.por)



