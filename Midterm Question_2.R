# Nithin Das CWID:10422784 Date:11/07/2019 Midterm Fall 2019 Question 2
library(dplyr)
library(leaps)
library(olsrr)
# loading data in to R
data<- read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Midterm/MPG.csv')
attach(data)

# converting string values to integer and set variable as factor
data$Domestic<-factor(Domestic,labels = c(0,1))
data_fit<-lm(MPG~cylinders+horsepower+weight+acceleration+age+Domestic,data=data)

# apply forward selection
best_forward<-regsubsets(MPG~cylinders+horsepower+weight+acceleration+age+Domestic,data=data,nbest=5,nvmax=5,method='forward')

best_forward_summary<-summary(best_forward)
best_forward_summary

# apply stepwise
best_seqrep<-regsubsets(MPG~cylinders+horsepower+weight+acceleration+age+Domestic,data=data,nbest=5,nvmax=5,method='seqrep')

best_seqrep_summary<-summary(best_seqrep)
best_seqrep_summary

# get best subset using exhaustive method and choose the one with good R squared. Equivalent to MaxR in SAS
best_exhaustive=regsubsets(MPG~cylinders+horsepower+weight+acceleration+age+Domestic,data=data,nbest=5,nvmax=5,method='exhaustive')
best_exhaustive_summary=summary(best_exhaustive)
which.max(best_exhaustive_summary$rsq)



