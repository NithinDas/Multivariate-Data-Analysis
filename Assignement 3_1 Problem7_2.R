#Name:Nithin Das, 10422784 Assignment: 3.1, Date: 10/29/2019

require("scatterplot3d")

lung_data<-read.csv('C:/Users/nithi/Documents/Stevens/2019 Fall/BIA 652/Assignemnts/Assignment 3/Lung.csv',header = TRUE,sep = ',')
attach(lung_data)
mr<- lm(FFVC~FAGE+FHEIGHT)
s3d <- scatterplot3d(FAGE,FHEIGHT,FFVC, pch=19, type = "p", color = "darkgrey",
                     main = "Regression Plane", grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 2, 1.5), angle = 55)

#multiple regression
s3d$plane3d(mr, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(.1, .2, .7, .5)))

