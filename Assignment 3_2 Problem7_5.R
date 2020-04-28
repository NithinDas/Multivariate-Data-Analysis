#Name:Nithin Das, 10422784 Assignment: 3.2, Date: 10/29/2019
library(ggplot2)
depression_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 3/Depress.csv',header = TRUE,sep = ',')

attach(depression_data)
depression_fit<- lm(CESD~INCOME+SEX+AGE)
par(mfrow=c(1,1))
print(head(fortify(depression_fit)))
ggplot(aes(x=.fitted,y=.resid),data=depression_fit)+geom_point()+geom_hline(yintercept = 0)
summary(depression_fit)