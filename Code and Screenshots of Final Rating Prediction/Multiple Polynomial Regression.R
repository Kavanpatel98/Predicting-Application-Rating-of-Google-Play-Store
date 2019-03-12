rm(list=ls())
#f<-read.csv("C:\\Users\\Downloads\\data_set\\Data Cleaning of Application Information\\all category data with median_rating\\final\\Copy_final_median_.csv")

library(openxlsx)
data<-read.xlsx("C:\\Users\\Downloads\\data_set\\Final Clean and Merge Data of Information and Review\\Final_Data_Both_Review_and_Information.xlsx")


#a<-f[,c(2:9,12)]   
a<-data[,c(3:12,15)]
#str(f)
w <- a[,c(1:9)]
results <- fastDummies::dummy_cols(a)
w<-results[,c(1,4:7,9,11:12,14:46,48,50:53)]
set.seed(222)
ind <- sample(2, nrow(w), replace = T, prob = c(0.7, 0.3))
train <- w[ind==1,]
test <- w[ind==2,]

rf <- lm(train$Rating ~  poly( Pol+Reviews+Size + Installs_1 + Final_Price + No_of_days_till_last_update + Sentiment_Positive+ Sentiment_Negative + Category_TOOLS + Category_COMMUNICATION + Category_SPORTS + Category_PERSONALIZATION + Category_SOCIAL + Category_HEALTH_AND_FITNESS + Category_FAMILY + Category_SHOPPING + Category_LIFESTYLE + Category_BOOKS_AND_REFERENCE + Category_MEDICAL + Category_GAME + Category_PHOTOGRAPHY + Category_TRAVEL_AND_LOCAL + Category_DATING + Category_ART_AND_DESIGN + Category_FOOD_AND_DRINK  + Category_VIDEO_PLAYERS + Category_PRODUCTIVITY + Category_MAPS_AND_NAVIGATION + Category_FINANCE + Category_NEWS_AND_MAGAZINES + Category_EDUCATION + Category_BUSINESS + Category_WEATHER + Category_ENTERTAINMENT + Category_AUTO_AND_VEHICLES + Category_HOUSE_AND_HOME + Category_EVENTS + Category_LIBRARIES_AND_DEMO + Category_PARENTING + Category_COMICS + Type_Free + Content_Rating_Everyone + Content_Rating_Mature_17 + Content_Rating_Everyone_10 + Content_Rating_Teen,3),data = train)

library(caret)
p1 <- predict(rf, test)
x1<- mean((test$Rating-p1)^2) 

plot(test$Rating,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(p1,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))

postResample(pred = p1, obs = test$Rating)