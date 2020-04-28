#Name:Nithin Das, 10422784 Assignment6 : 14.1, Date: 12/02/2019

depression<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 6/Depress.csv')


cols_required<-c("DRINK","HEALTH","REGDOC","TREAT","BEDDAYS","ACUTEILL", "CHRONILL")

depress_data <- depression[,cols_required]


prin_comp <- prcomp(depress_data, scale. = T)
# STANDARD DEVIATION
std_dev<-prin_comp$sdev
# VARIANCE
prin_var<-std_dev^2
prop_varex<-prin_var/sum(prin_var)
print(prop_varex)
# PLOT GRAPHS
plot(prop_varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")