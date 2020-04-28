#Nithin Das, CWID 10422784 Date: 10/15/2019 Problem: 6.10

lung_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 2/Lung.csv',header = TRUE,sep = ',')
required_variables<-c('OCSEX','OCAGE','OCHEIGHT','OCWEIGHT','OCFVC','OCFEV1')
oldest_child<-lung_data[required_variables]
# fitting regression
attach(oldest_child)

# Regression Analysis

fev1_ht_reg<-lm(OCFEV1~OCHEIGHT,data = oldest_child)
summary(fev1_ht_reg)
cat('Slope for FEV1 on Height analysis:',fev1_ht_reg$coefficients['OCHEIGHT'])
cat('Intercept for FEV1 on Height analysis:',fev1_ht_reg$coefficients['(Intercept)'])
cat('Correlation Coefficient FEV1 v/s Height: ', cor(OCFEV1,OCHEIGHT,method = 'pearson'))


fev1_wt_reg<-lm(OCFEV1~OCWEIGHT,data = oldest_child)
summary(fev1_wt_reg)
cat('Slope for FEV1 on Weight analysis:',fev1_wt_reg$coefficients['OCWEIGHT'])
cat('Intercept for FEV1 on Weight analysis:',fev1_wt_reg$coefficients['(Intercept)'])
cat('Correlation Coefficient FEV1 v/s Weight: ', cor(OCFEV1,OCWEIGHT,method = 'pearson'))

fvc_ht_reg<-lm(OCFVC~OCHEIGHT,data = oldest_child)
summary(fvc_ht_reg)
cat('Slope for FVC on Height analysis:',fvc_ht_reg$coefficients['OCHEIGHT'])
cat('Intercept for FVC on Height analysis:',fvc_ht_reg$coefficients['(Intercept)'])
cat('Correlation Coefficient FVC v/s Height: ', cor(OCFVC,OCHEIGHT,method = 'pearson'))

fvc_wt_reg<-lm(OCFVC~OCWEIGHT,data = oldest_child)
summary(fvc_wt_reg)
cat('Slope for FVC on Weight analysis:',fvc_wt_reg$coefficients['OCWEIGHT'])
cat('Intercept for FVC on Weight analysis:',fvc_wt_reg$coefficients['(Intercept)'])
cat('Correlation Coefficient FVC v/s Weight: ', cor(OCFVC,OCWEIGHT,method = 'pearson'))


par(mfrow=c(2,2))
plot(fev1_ht_reg)







