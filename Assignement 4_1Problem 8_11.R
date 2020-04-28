#Name:Nithin Das, 10422784 Assignment: 4.1, Date: 10/29/2019

lung_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 3/Lung.csv',header = TRUE,sep = ',')
attach(lung_data)

#required_variables<- c('OCAGE', 'OCWEIGHT', 'MHEIGHT', 'MWEIGHT', 'FHEIGHT','FWEIGHT')

#no variable approach
fitforward_novar<-lm(OCHEIGHT~1)
fitforward<-lm(OCHEIGHT~OCAGE+OCWEIGHT+MHEIGHT+MWEIGHT+FHEIGHT+FWEIGHT)
#step(fitforward,direction = "forward",scope = formula(fitforward))
step(fitforward_novar,direction = "both",scope = list(lower=fitforward_novar,upper=fitforward))