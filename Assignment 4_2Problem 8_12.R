library(leaps)#Name:Nithin Das, 10422784 Assignment: 4.2, Date: 10/29/2019
library(car)
lung_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 3/Lung.csv',header = TRUE,sep = ',')
attach(lung_data)
leaps<-regsubsets(OCHEIGHT~OCAGE+OCWEIGHT+MHEIGHT+MWEIGHT+FHEIGHT+FWEIGHT,data=lung_data,nvmax=3,method='backward')
summary(leaps)
detach(lung_data)

lung_data_boys<-lung_data[lung_data$OCSEX == 1,]
lung_data_girls<-lung_data[lung_data$OCSEX == 2,]

attach(lung_data_boys)
leaps<-regsubsets(OCHEIGHT~OCAGE+OCWEIGHT+MHEIGHT+MWEIGHT+FHEIGHT+FWEIGHT,data=lung_data_boys,nvmax=3,method='forward')
summary(leaps)
detach(lung_data_boys)

attach(lung_data_girls)
leaps<-regsubsets(OCHEIGHT~OCAGE+OCWEIGHT+MHEIGHT+MWEIGHT+FHEIGHT+FWEIGHT,data=lung_data_girls,nvmax=3,method='forward')
summary(leaps)
detach(lung_data_girls)
