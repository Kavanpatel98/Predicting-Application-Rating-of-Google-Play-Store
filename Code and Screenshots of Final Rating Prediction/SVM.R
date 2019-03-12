rm(list=ls())
library("openxlsx")
data<-read.xlsx("C:\\Users\\Downloads\\data_set\\Final Clean and Merge Data of Information and Review\\Final_Data_Both_Review_and_Information.xlsx")

a<-data[,c(3:12,15)]
results <- fastDummies::dummy_cols(a)
a<-results[,c(1,4:7,9,11:54)]

set.seed(222)
ind <- sample(2, nrow(results), replace = T, prob = c(0.7, 0.3))
train <- a[ind==1,]
test <- a[ind==2,]

library(e1071)
rf <- svm(Rating ~ ., data = train)
k <- as.matrix(train)
library(caret)
p1 <- predict(rf, test)
mean((test$Rating - p1)^2) 

plot(test$Rating,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))

postResample(pred = p1, obs = test$Rating)