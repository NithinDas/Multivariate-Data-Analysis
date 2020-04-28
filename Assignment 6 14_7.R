#Name:Nithin Das, 10422784 Assignment6 : 14.7, Date: 12/02/2019

lung2<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 6/Lung.csv')

prin_comp4 <- prcomp(lung2[,c('OCAGE','OCHEIGHT','OCWEIGHT')], scale. = T)
#standard deviation
std_dev4<-prin_comp4$sdev
#variance
prin_var4<-std_dev4^2
prop_varex4<-prin_var4/sum(prin_var4)
print(prop_varex4)
# plot graphs
plot(prop_varex4, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
plot(cumsum(prop_varex4), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")