rm(list=ls())
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library("openxlsx")
f<-read.xlsx("C:\\Users\\Downloads\\data_set\\Final Clean and Merge Data of Information and Review\\Final_Data_Both_Review_and_Information.xlsx")
str(f)
a<-f[,c(3:12,15)]   #a<-data[,c(2,4:8,10:11,15)]
str(a)
results <- fastDummies::dummy_cols(a)
colnames(results)
w <- results[,c(1,4:7,9,11:54)]
w[is.na(w)]<-0
n <- neuralnet(Rating ~ Pol+Reviews+Size+Installs_1+Final_Price+No_of_days_till_last_update+Sentiment_Positive+Sentiment_Neutral+Sentiment_Negative+Category_TOOLS+Category_COMMUNICATION+Category_SPORTS+Category_PERSONALIZATION+Category_SOCIAL+Category_HEALTH_AND_FITNESS+Category_FAMILY+Category_SHOPPING+Category_LIFESTYLE+Category_BOOKS_AND_REFERENCE+Category_MEDICAL+Category_GAME+Category_PHOTOGRAPHY+Category_TRAVEL_AND_LOCAL+Category_DATING+Category_ART_AND_DESIGN+Category_FOOD_AND_DRINK+Category_VIDEO_PLAYERS+Category_PRODUCTIVITY+Category_MAPS_AND_NAVIGATION+Category_FINANCE+Category_NEWS_AND_MAGAZINES+Category_EDUCATION+Category_BUSINESS+Category_WEATHER+Category_ENTERTAINMENT+Category_AUTO_AND_VEHICLES+Category_HOUSE_AND_HOME+Category_EVENTS+Category_LIBRARIES_AND_DEMO+Category_PARENTING+Category_COMICS+Category_BEAUTY+Type_Free+Type_Paid+Content_Rating_Everyone+Content_Rating_Mature_17+Content_Rating_Everyone_10+Content_Rating_Teen+Content_Rating_Adults_only_18
               , data = w, hidden = c(10,5), linear.output = FALSE, lifesign = 'full',rep = 1)


plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

w <- as.matrix(w)
dimnames(w) <- NULL
set.seed(222)
ind <- sample(2, nrow(w), replace = T, prob = c(0.7, 0.3))
training <- w[ind==1,c(1,3:50)]
test <- w[ind==2,c(1,3:50)]
trainingtarget <- w[ind==1,2]
testingtarget <- w[ind==2,2]

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)
------------------------------------------------------------------------------
library(keras)
library(tensorflow)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape =c(49)) %>%
  layer_dense(units = 1)

model %>% compile(loss= 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

model %>% evaluate(test, testingtarget)
pred <- model %>% predict(test)
mean((testingtarget-pred)^2)
plot(testingtarget,pred)
postResample(pred = pred, obs = testingtarget)

plot(testingtarget,col='red',main='Actual vs Predicted Rating',pch=18,cex=0.7)
points(pred,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))




