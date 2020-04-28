# Nithin Das CWID:10422784 Date:11/07/2019 Midterm Fall 2019 Question 1

# loading data in to R
mpg_data<- read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Midterm/MPG.csv')

# exploring data

summary(mpg_data)

# find outliers

attach(mpg_data)

mpg_fit<- lm(MPG~weight+horsepower+age)

cooksd <- cooks.distance(mpg_fit)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
head(mpg_data[influential, ])

# fitting the model by removing one outlier at a time
# remove 27th data point and fit the model
mpg_rem_outlier1<-mpg_data[-c(27),]
mpg_rem_outlier1_fit<- lm(MPG~weight+horsepower+age, data = mpg_rem_outlier1)
summary(mpg_rem_outlier1_fit)
# remove 27th data point and fit the model

mpg_rem_outlier2<-mpg_data[-c(7),]
mpg_rem_outlier2_fit<- lm(MPG~weight+horsepower+age, data = mpg_rem_outlier2)
summary(mpg_rem_outlier2_fit)
# remove 20th data point and fit the model
mpg_rem_outlier3<-mpg_data[-c(20),]
mpg_rem_outlier3_fit<- lm(MPG~weight+horsepower+age, data = mpg_rem_outlier3)
summary(mpg_rem_outlier3_fit)

# remove 294th data point and fit the model
mpg_rem_outlier4<-mpg_data[-c(294),]
mpg_rem_outlier4_fit<- lm(MPG~weight+horsepower+age, data = mpg_rem_outlier4)
summary(mpg_rem_outlier4_fit)

# remove 103th data point and fit the model
mpg_rem_outlier5<-mpg_data[-c(103),]
mpg_rem_outlier5_fit<- lm(MPG~weight+horsepower+age, data = mpg_rem_outlier5)
summary(mpg_rem_outlier5_fit)

# Question d
#Standardize the predictors to find most important variables

mpg_standard<-mpg_data
# standardize the variables
mpg_standard$horsepower_st<-scale(mpg_standard$horsepower)
mpg_standard$age_st<-scale(mpg_standard$age)
mpg_standard$weight_st<-scale(mpg_standard$weight)

mpg_standard_fit<- lm(MPG~weight_st+horsepower_st+age_st,data=mpg_standard)
summary(mpg_standard_fit)

#Question e
#get record with weight =2420,horsepower=100,age=3 and get actual MPG
record<-subset(mpg_data, weight == 2420 & horsepower==100 & age==3)
# predict MPG
record <- subset(record,select  = c("age","weight","horsepower"))
predict(mpg_fit, newdata = record)
# 95% CI on MPG prediction
predict(mpg_fit, newdata = record, interval = "prediction")
# 95% CI on average of MPg predictions
predict(mpg_fit, newdata = record, interval = "confidence")

# leverage points
mpg_lev_points<-hatvalues(mpg_fit)
order(-mpg_lev_points)
# leverage points sample 14,116,9,7,8
select(mpg_data[c(14,116,9,7,8), ],'MPG','weight','horsepower','age')



