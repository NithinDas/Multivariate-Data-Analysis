# Nithin Das CWID:10422784 Date:11/07/2019 Midterm Fall 2019 Question 4

library(ggplot2)
library(dplyr)

# load data
lungs_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Midterm/Lung.csv')
# fit multiple regression model
lungs_data_fit<-lm(MFEV1~MHEIGHT+MWEIGHT,data = lungs_data)

# plot residual v/s fitted 
par(mfrow=c(1,1))
print(head(fortify(lungs_data_fit)))
ggplot(aes(x=.fitted,y=.resid),data=lungs_data_fit)+geom_point()+geom_hline(yintercept = 0)+ggtitle("Residuals vs fitted")

#plot histogram of residuals
hist(residuals(lungs_data_fit))

# plot the fitted model
plot(lungs_data_fit)

# calculate cooks distance
cooksd <- cooks.distance(lungs_data_fit)
# plot cooks distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
# plot cook's distance
abline(h = 6*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>6*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

# consider cooks distance greater than 6 times the mean to identify influential points

influential <- as.numeric(names(cooksd)[(cooksd > 6*mean(cooksd, na.rm=T))])
select(lungs_data[influential, ],'ID','MHEIGHT','MWEIGHT','MFEV1')


# remove ID=7 record and fit the model
lungs_rem_outlier1<-lungs_data[-c(7),]
lungs_rem_outlier1_fit<- lm(MFEV1~MWEIGHT+MHEIGHT, data = lungs_rem_outlier1)
lungs_rem_outlier1_fit

# remove ID=33 record and fit the model
lungs_rem_outlier2<-lungs_data[-c(33),]
lungs_rem_outlier2_fit<- lm(MFEV1~MWEIGHT+MHEIGHT, data = lungs_rem_outlier2)
lungs_rem_outlier2_fit

# remove ID=42 record and fit the model
lungs_rem_outlier3<-lungs_data[-c(42),]
lungs_rem_outlier3_fit<- lm(MFEV1~MWEIGHT+MHEIGHT, data = lungs_rem_outlier3)
lungs_rem_outlier3_fit

# remove ID=90 record and fit the model
lungs_rem_outlier4<-lungs_data[-c(90),]
lungs_rem_outlier4_fit<- lm(MFEV1~MWEIGHT+MHEIGHT, data = lungs_rem_outlier4)
lungs_rem_outlier4_fit

# remove ID=105 record and fit the model
lungs_rem_outlier5<-lungs_data[-c(105),]
lungs_rem_outlier5_fit<- lm(MFEV1~MWEIGHT+MHEIGHT, data = lungs_rem_outlier5)
lungs_rem_outlier5_fit

#Question d
lungs_standard<-lungs_data
#standardize the variables
lungs_standard$MWEIGHT_st<-scale(lungs_standard$MWEIGHT)
lungs_standard$MHEIGHT_st<-scale(lungs_standard$MHEIGHT)

lungs_standard_fit<- lm(MFEV1~MWEIGHT_st+MHEIGHT_st,data=lungs_standard)
lungs_standard_fit

# high leverage points

lev_points<-hatvalues(lungs_data_fit)
order(-lev_points)
# leverage points
select(lungs_data[c(144,94,45), ],'ID','MHEIGHT','MWEIGHT','MFEV1')

