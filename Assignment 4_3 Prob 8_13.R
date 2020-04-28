#Name:Nithin Das, 10422784 Assignment: 4.3, Date: 10/29/2019
parental_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 4/Parental Hiv.csv',header = TRUE,sep = ',')


attach(parental_data)
fitforward_novar<-lm(AGEALC~1)
fitforward<-lm(AGEALC~AGEMAR+AGESMOKE+SMOKEP3M+NGHB5+NGHB2+GENDER+SIBLINGS+NGHB1+NGHB3)
step(fitforward_novar,direction = "forward",scope = list(lower=fitforward_novar,upper=fitforward))
