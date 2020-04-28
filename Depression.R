#Nithin Das, CWID 10422784 Date: 10/15/2019 Problem: 6.9

depression_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 2/Depression.csv',header = TRUE,sep = ',')


reg_model<-lm(Income~Age,data=depression_data)
summary(reg_model)
# Adding each rows

# add 42,120
depression_data<-rbind(depression_data,c(42,120))
reg_model_42_120<-lm(Income~Age,data=depression_data)
summary(reg_model_42_120)
#remove the record
depression_data<-depression_data[-nrow(depression_data),]

# add 80,150
depression_data<-rbind(depression_data,c(80,150))
reg_model_80_150<-lm(Income~Age,data=depression_data)
summary(reg_model_80_150)
#remove the record
depression_data<-depression_data[-nrow(depression_data),]

# add 180,15
depression_data<-rbind(depression_data,c(180,15))
reg_model_180_15<-lm(Income~Age,data=depression_data)
summary(reg_model_180_15)
#remove the record
depression_data<-depression_data[-nrow(depression_data),]

# Question 3, adding all 3 data points together
depression_data<-rbind(depression_data,c(42,120))
depression_data<-rbind(depression_data,c(80,150))
depression_data<-rbind(depression_data,c(180,15))
plot(Age, Income, main="Scatterplot Example",xlab="Age ", ylab="Income ", pch=19)
reg_model_allpoints<-lm(Income~Age,data=depression_data)
abline(reg_model_allpoints,col='red')

par(mfrow=c(2,2))
plot(reg_model_allpoints)
