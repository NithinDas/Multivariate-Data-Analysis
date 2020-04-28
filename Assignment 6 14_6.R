#Name:Nithin Das, 10422784 Assignment6 : 14.6, Date: 12/02/2019

lung1<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 6/Lung.csv')
lung1['RATIO']=lung1['FFEV1']/lung1['FFVC']
# correlation between RATIO  and FFEV1
print(cor(lung1['RATIO'],lung1['FFEV1']))
# correlation between RATIO  and FFVC
print(cor(lung1['RATIO'],lung1['FFVC']))


# PCA on FEV1 and FFVC
prin_comp2 <- prcomp(lung1[,c('FFEV1','FFVC')], scale. = T)
#standard deviation
std_dev2<-prin_comp2$sdev
#variance
prin_var2<-std_dev2^2
prop_varex2<-prin_var2/sum(prin_var2)
print(prop_varex2)

plot(prop_varex2, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
plot(cumsum(prop_varex2), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# PCA on FEV1, FFVC and RATIO


prin_comp3 <- prcomp(lung1[,c('FFEV1','FFVC','RATIO')], scale. = T)
#standard deviation
std_dev3<-prin_comp3$sdev
#variance
prin_var3<-std_dev3^2
prop_varex3<-prin_var3/sum(prin_var3)
print(prop_varex3)
# plot graphs
plot(prop_varex3, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
plot(cumsum(prop_varex3), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


