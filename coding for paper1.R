library(tseries)
library(forecast)
library(TSA)


setwd("C:/Users/hyeon/Desktop")
rawdata <- read.csv2("researchdata.csv",header=T ,sep = ",",dec=",")
rawdata <- rawdata[nrow(rawdata):1,]
rawdata <- as.matrix(rawdata)
rawdata <- rawdata[,-1]
colnames(rawdata) <- NULL
rawdata <- t(rawdata)
rawdata <- as.vector(rawdata)
traindata <- rawdata[1:132]
testdata <- rawdata[133:156]
trainset <- ts(traindata, frequency = 12, start = c(2004,1))
wholeset <- ts(rawdata, frequency = 12, start = c(2004,1))

par(mfrow=c(1,1))
plot(stl((trainset),s.window="periodic"),main="Figure1 : Analyzing trends & Seasonality",cex.main=1)
plot(trainset,main="dd",cex.main=3)

diffsea <- diff(trainset,12)
plot(diffsea)
acf(as.vector(diffsea))
pacf(as.vector(diffsea))


diffdata <- diff(diff(trainset),12)
plot(diffdata)
plot(diffdata, main="Figure2 : After seasonal and non-seasonal first differencing",ylab="",cex.main=1.5)



par(mfrow = c(2,1),
    oma = c(3,2,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.5)
acf(as.vector(diffdata),main="Figure3 :ACF and PACF plots",cex.main=1.5)
pacf(as.vector(diffdata))


auto.arima(trainset, D=1,d=1)



model1 <- Arima(trainset, order = c(1,1,2), seasonal = c(0,1,1))
model2 <- Arima(trainset, order = c(2,1,2), seasonal = c(0,1,1))
model3 <- Arima(trainset, order = c(3,1,2), seasonal = c(0,1,1))
model4 <- Arima(trainset, order = c(4,1,0), seasonal = c(0,1,1))
model5 <- Arima(trainset, order = c(4,1,2), seasonal = c(0,1,1))
model6 <- Arima(trainset, order = c(4,1,1), seasonal = c(0,1,1))

detectIO(model2)
pacf(as.vector(residuals(model1)),main="Figure4: PACF of residuals from ARIMA(1,1,2)(0,1,1)",cex.main=1.5)
  trainset[105]



par(mfrow = c(2,1),
      oma = c(3,3,0,0) + 0.1,
      mar = c(0,0,3,1) + 0.8)
  
  
qqnorm(residuals(model4),main="Figure5: Q-Q plot of residuals from ARIMA(4,1,0)(0,1,1)")
qqline(residuals(model4))
hist(residuals(model4),main="Figure6: Histogram of residuals from ARIMA(4,1,0)(0,1,1)")






BIC(model1)
BIC(model2)
BIC(model3)
BIC(model4)
BIC(model5)
BIC(model6)


Box.test(residuals(model1), type="Ljung-Box",lag =20,fitdf = 4)
Box.test(residuals(model2), type="Ljung-Box",lag =20,fitdf = 5)
Box.test(residuals(model3), type="Ljung-Box",lag =20,fitdf = 6)
Box.test(residuals(model4), type="Ljung-Box",lag =20,fitdf = 5)
Box.test(residuals(model5), type="Ljung-Box",lag =20,fitdf = 7)
Box.test(residuals(model6), type="Ljung-Box",lag =20,fitdf = 6)





predf <- predict(model5, n.ahead = 2*12)
predvect <- as.vector(predf$pred)

predts <- ts(predvect, frequency = 12, start = c(2015,1))
plot(wholeset)
lines(predts,col="blue")






testset <- ts(testdata,frequency = 12, start = c(2015,1))

library(forecast)
plot(forecast(model1,level = c(80,95)))
lines(testset,col="red")


x <- c("training set","predicted from ARIMA","test set")
plot(forecast(model4,level = c(80,95)),main="Figure7: Forecasts from ARIMA(4,1,0)(0,1,1)[12]",cex.main=1.5)
lines(testset,col="red")
legend('topleft',x, 
       lty=c(1,1,1), col=c('black', 'blue', 'red'), bty='n', cex=1)





#####################
# sum up the training and testset
# replace outlier as the average of august
rawdata[152] <- 4375702
rawdata[152]

wholedata <- ts(rawdata, frequency = 12, start = c(2004,1))
plot(wholedata)





diffwhole <- diff(diff(wholedata,12))
plot(diffwhole)


acf(as.vector(diffwhole))
pacf(as.vector(diffwhole))

modelw <- Arima(wholedata,order=c(4,1,0), seasonal=c(0,1,1))
modelw2 <- Arima(wholedata, order = c(4,1,0), seasonal = c(0,1,1))

detectAO(modelw)
detectIO(modelw)

pacf(residuals(modelw))

Box.test(residuals(modelw), type="Ljung-Box",lag =21,fitdf = 5)

qqnorm(residuals(modelw))
hist(residuals(modelw))



predf <- predict(modelw,n.ahead = 24)
future <- ts(predf$pred, frequency = 12, start = c(2017,1))
plot(future)


x <- c("Whole set (2004~2016)","predicted from ARIMA")
plot(forecast(modelw,level = c(80,95)),xlim = c(2004,2018), main="Figure8: Forecasts from ARIMA(4,1,0)(0,1,1)[12]",cex.main=1.4)
legend('topleft',x, 
       lty=c(1,1,1,2), col=c('black', 'blue', 'red',' black'), bty='n', cex=1)


