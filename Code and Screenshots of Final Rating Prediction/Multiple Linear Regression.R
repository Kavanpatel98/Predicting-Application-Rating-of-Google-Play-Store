rm(list=ls())
library("openxlsx")
data<-read.xlsx("C:\\Users\\Downloads\\data_set\\Final Clean and Merge Data of Information and Review\\Final_Data_Both_Review_and_Information.xlsx")

a<-data[,c(3:12,15)]
results <- fastDummies::dummy_cols(a)
x<-results[,c(1,4:7,9,11:13,15:46,48,50:53)]
#x<-results[,c(1:2,5:8,10,12:14,16:47,49,51:54)]

set.seed(222)
ind <- sample(2, nrow(x), replace = T, prob = c(0.8, 0.2))
train <- x[ind==1,]
test <- x[ind==2,]

rf <- lm(train$Rating ~ ., data = train)
library(caret)
p1 <- predict(rf, test)
mean((test$Rating - p1)^2) 
library(Metrics)
mse(test$Rating, p1)


postResample(pred = p1, obs = test$Rating)