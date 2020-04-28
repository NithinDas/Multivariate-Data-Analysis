#Nithin Das, 10422784, 11/21/19, Assignment 5: 12.23 & 12.24

library(MASS)
library(dplyr)
library(lmtest)
library(caret)
library(car)
library(ROCR)
library(pscl)
newdata<- read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignments 5/Parental Hiv.csv',na.strings = ".")

#impute missing values to 0
newdata[is.na(newdata)]<-0

# check summary of data 
str(data)
summary(data)

# convert target variable in to 0 and 1 notations
newdata$HOOKEY=ifelse(newdata$HOOKEY==1,0,1)

#remove "ID" column
myvars <- names(newdata) %in% c("ID")
clean_data <- newdata[!myvars]

# convert required variables to categorical variables
req_columns_factor<-c("GENDER","LIVWITH","SIBLINGS" ,"JOBMO"  ,  "EDUMO" ,   "HOWREL" ,  "ATTSERV" ,
               "NGHB1"   , "NGHB2"   , "NGHB3"  ,  "NGHB4"   , "NGHB5"  ,  "NGHB6"  ,  "NGHB7"  ,  "NGHB8"    ,"NGHB9" ,  
                 "NGHB10" ,  "NGHB11" ,  "MONFOOD" , "FINSIT",   "ETHN"   , "SMOKEP3M" ,  
               "FRNDS"  ,  "SCHOOL",   "LIKESCH",  "HOOKEY" ,   "HMONTH"  , "PB01"    , "PB02"   ,  "PB03"   , 
               "PB04"  ,   "PB05",     "PB06" ,    "PB07"    , "PB08"    , "PB09",     "PB10"    , "PB11"    , "PB12"    ,
               "PB13" ,    "PB14" ,    "PB15",     "PB16" ,    "PB17"    , "PB18" ,    "PB19"    , "PB20"     ,"PB21"    ,
               "PB22",     "PB23"  ,   "PB24"   ,  "PB25"  ,   "BSI01",    "BSI02" ,   "BSI03"  ,  "BSI04"   , "BSI05"  ,
               "BSI06" ,   "BSI07",    "BSI08" ,   "BSI09"  ,  "BSI10" ,   "BSI11" ,   "BSI12" ,   "BSI13"  ,  "BSI14"   ,
               "BSI15",    "BSI16" ,   "BSI17",    "BSI18"   , "BSI19",    "BSI20" ,   "BSI21"   , "BSI22"   , "BSI23"   ,
               "BSI24" ,   "BSI25"  ,  "BSI26"  ,  "BSI27"  ,  "BSI28" ,   "BSI29" ,   "BSI30"   , "BSI31",    "BSI32"   ,
               "BSI33",    "BSI34"   , "BSI35" ,   "BSI36"   , "BSI37"  ,  "BSI38" ,   "BSI39"  ,  "BSI40" ,   "BSI41"   ,
               "BSI42" ,   "BSI43"   , "BSI44",    "BSI45"  ,  "BSI46"  ,  "BSI47"  ,  "BSI48",    "BSI49" ,   "BSI50"   ,
               "BSI51",    "BSI52"   , "BSI53")

clean_data[,req_columns_factor] <- lapply(clean_data[,req_columns_factor] , factor)
# convert required columns to integer type

req_columns_numeric<-c("AGE","AGESMOKE","AGEALC","AGEMAR","NHOOKEY")

clean_data[,req_columns_numeric] <- lapply(clean_data[,req_columns_numeric] , as.integer)

# fit binary logistic regression with all variables
logistic_model<-glm(HOOKEY ~ . , data=clean_data,family = binomial(link = "logit"))

step.model <- stepAIC(logistic_model,direction = 'backward')
  
# model with best variables

new_lo<-glm(HOOKEY ~ AGE + HOWREL + BSI53+BSI37+BSI40+BSI14+BSI10 +AGEMAR , data=clean_data,family = binomial(link = "logit"))

# Goodness of Fit
# McFadden Pseudo R^2 value
pR2(new_lo)

new_var<-c('AGE','HOWREL','BSI53','BSI37','BSI40','BSI14','BSI10','AGEMAR')

new_clean_data <- clean_data[new_var]


#identify cut off point
p1<-predict(new_lo,new_clean_data,type = 'response')
pred_compare<-prediction(p1,clean_data$HOOKEY)

eval<-performance(pred_compare,"acc")
plot(eval)
max_accuracy<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max_accuracy]
cut<-slot(eval,"x.values")[[1]][max_accuracy]
print(acc)
print(cut)


abline(h=0.8730,v=0.64531,col=c("red"))

threshold=0.64531
predicted_values<-ifelse(p1>threshold,1,0)

# confusion matrix, specificity and sensitivity

conf_matrix<-table(predicted_values,clean_data$HOOKEY)
print(conf_matrix)
print(sensitivity(conf_matrix))
print(specificity(conf_matrix))
# ROC Curve
roc<-performance(pred_compare,"tpr","fpr")
plot(roc,title("ROC Curve"))

# Area under Curve
auc<-performance(pred_compare,"auc")
auc<-unlist(slot(auc,"y.values"))
print(auc)




