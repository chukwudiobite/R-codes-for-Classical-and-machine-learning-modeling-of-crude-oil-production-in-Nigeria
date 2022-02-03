# R-codes-for-Classical-and-machine-learning-modeling-of-crude-oil-production-in-Nigeria
library(readxl)
library(randomForest)
library(hydroGOF)
library(Metrics)
library(prodlim)
library(nnet)
library("neuralnet")
library(NeuralNetTools)
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)

#Import the data
data1 <- crude_oil <- read_excel("C:/Users/OBITE C. P/Desktop/Research/Crude oil/crude oil production main1.xlsx")
set.seed(40)

#Plot the crude oil production
plot.ts(data1$Production, ylab = "Crude Oil Productions (mmbd)", xlab = "Months (Jan. 2006 - Dec. 2020)")

#Split the data into train and test set
TrainSet <- data1[1:123,]
ValidSet <- data1[124:176,]

summary(TrainSet)
summary(ValidSet)
set.seed(40)

#Random forest model
model2 <- randomForest(Production ~ ., data = TrainSet, 
                       ntree = 500, mtry = 2, importance = TRUE)
model2
predTrain <- predict(model2, TrainSet, type = "class")

# Checking classification accuracy
MSE_RF1=sqrt((sum((TrainSet$Production-predTrain)**2))/nrow(as.matrix(TrainSet$Production))) #RMSE
MAPE_RF1=((sum(abs((TrainSet$Production-predTrain)/TrainSet$Production)))/nrow(as.matrix(TrainSet$Production))) #MAPE
NSE_RF1=NSE(predTrain, TrainSet$Production) #NSE

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")

# Checking classification accuracy
MSE_RF2=sqrt((sum((ValidSet$Production-predValid)**2))/nrow((ValidSet)))
MAPE_RF2=((sum(abs((ValidSet$Production-predValid)/ValidSet$Production)))/nrow(as.matrix(ValidSet$Production))) #MAPE
NSE_RF2=NSE(predValid,ValidSet$Production)

#Importance of the variables
round(importance(model2), 2)       
varImpPlot(model2, main = "")   



#neural network
library(MASS)
library("neuralnet")
library(readxl)
set.seed(40)

#Max-min regularization method
max_data<-apply(data1, 2, max)
min_data<-apply(data1, 2, min)
data_scaled<-scale(data1, center = min_data, scale = max_data - min_data)
train_data<-as.data.frame(data_scaled[1:123,])
test_data<-as.data.frame(data_scaled[124:176,])
data_scaled1<-as.data.frame(data_scaled)

#ANN model
net_data<-neuralnet(Production ~ ., data = train_data, hidden = 4, 
 
                                     linear.output = T, act.fct = "logistic")
#Ann plot
plot(net_data)

#Importance of the variales
olden(net_data) #variable importance

#Prediction
predict_net_test<-compute(net_data, test_data[,2:4])
predict_net_test_start<-(predict_net_test$net.result*(max(data1$Production)-min(data1$Production)))+
  min(data1$Production)
test_start<-as.data.frame(((test_data$Production)*(max(data1$Production)-min(data1$Production)))+min(data1$Production))
MSE.net<-sqrt(sum((test_start-predict_net_test_start)^2)/nrow(test_start))
MAPE_net=(sum(abs((test_start-predict_net_test_start)/test_start)))/nrow(test_start) #MAPE
NSE_net=NSE(predict_net_test_start, test_start) #NSE


predict_net_test2<-compute(net_data, train_data[,2:4])
predict_net_test_start2<-(predict_net_test2$net.result*(max(data1$Production)-min(data1$Production)))+
  min(data1$Production)
test_start2<-as.data.frame(((train_data$Production)*(max(data1$Production)-min(data1$Production)))+
                             min(data1$Production))
MSE.net2<-sqrt(sum((test_start2-predict_net_test_start2)^2)/nrow(test_start2))
MAPE_net2=(sum(abs((test_start2-predict_net_test_start2)/test_start2)))/nrow(test_start2) #MAPE
NSE_net2=NSE(predict_net_test_start2, test_start2) #NSE



#ARIMA
library(tseries)
library(forecast)

summary(data1$Production)
data<-TrainSet$Production
ggplot(g[1:123,], mapping = aes(x = DateTime3, y = Production) ) +
  geom_line()+ labs(x=("Months (Jan. 2006 - May. 2016)"), y="Crude Oil Production (mmbd)")
adf.test(data)

diff1=diff(data)
plot.ts(diff1, ylab="First differencing of the data", xlab="")
adf.test(diff1)
acf(diff1, main = "")
pacf(diff1, main = "")
auto.arima(data)
model<-arima(data, c(2,1,1))
summary(model)
NSE_ARIMA=NSE(forecast(model,h=1)$fitted, TrainSet$Production)#NSE
MAPE_ARIMA= mape(TrainSet$Production, forecast(model,h=1)$fitted)
checkresiduals(model)
ytrain=forecast(model,h=1)$fitted
xtrain=TrainSet$Production
rss=sum((ytrain-xtrain)**2)
tss=sum((xtrain-mean(xtrain))**2)
Rsquare=1-rss/tss
Rsquare
rss/nrow(TrainSet)
paste(c("R-squared:", round(Rsquare, 4)))
dataforecast<-forecast(model, 53)
MSE_ARIMA2=sqrt((sum((dataforecast$mean-ValidSet$Production)**2))/nrow(ValidSet))
MAPE_ARIMA2=mape(ValidSet$Production, dataforecast$mean)
NSE_ARIMA2=NSE(dataforecast$mean, ValidSet$Production) #NSE
forecast(model, 92)

Test_set1 <- read_excel("C:/Users/OBITE C. P/Desktop/Research/Crude oil/Test set.xlsx")
Test_set<-as.data.frame(Test_set1)

DateTime1 = seq(from=as.Date("2016-6-1"), to=as.Date("2020-10-1"), by="months")
DateTime = as.POSIXct(DateTime1)
a=data.frame(Test_set,DateTime)
dfplot <- a %>% gather(key, value, -DateTime)
k=ggplot(dfplot, mapping = aes(x = DateTime, y = value, color = key) ) +
  geom_line()+ labs(x=("Months (Jun. 2016 - Oct. 2020)"), y="Crude Oil Production (mmbd)")
k
DateTime2 = seq(from=as.Date("2006-3-1"), to=as.Date("2020-10-1"), by="months")
DateTime3 = as.POSIXct(DateTime2)
g=data.frame(data1,DateTime3)

ggplot(g, mapping = aes(x = DateTime3, y = Production) ) +
  geom_line()+ labs(x=("Months (Jan. 2006 - Oct. 2020)"), y="Crude Oil Production (mmbd)")



#error of the different models for both training and test sets
te_arima= TrainSet$Production - forecast(model,h=1)$fitted
ve_arima= dataforecast$mean-ValidSet$Production
ve_ann= test_start-predict_net_test_start
ve_ann=as.data.frame(ve_ann[1:53,])
ve_ann=ve_ann$`ve_ann[1:53, ]`
te_ann = test_start2-predict_net_test_start2
te_ann= as.data.frame(te_ann[1:123,])
te_ann = te_ann$`te_ann[1:123, ]`
te_rf = TrainSet$Production-predTrain
ve_rf = ValidSet$Production-predValid

# Modified Diebold-Mariano Test
dm.test(te_arima, te_ann, alternative = c("greater"),
        h=1)
dm.test(te_arima, te_rf, alternative = c("greater"),
        h=1)
dm.test(ve_arima, ve_ann, alternative = c("greater"),
        h=1)
dm.test(ve_arima, ve_rf, alternative = c("greater"),
        h=1)
dm.test( te_rf,te_ann, alternative = c("greater"),
         h=1)
dm.test( ve_rf, ve_ann, alternative = c("greater"),
        h=1)
