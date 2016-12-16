library("tseries")
library("forecast")
library("lawstat")
library('car')
library('hydroGOF')

#read in data
train <- read.csv('train.csv')
test <- read.csv('test.csv')

bankruptcy <- ts(train['Bankruptcy_Rate'], start = c(1987,1), frequency = 12)
house_price <- ts(train['House_Price_Index'], start = c(1987,1), frequency = 12)
house_price_test <- ts(test['House_Price_Index'], start = c(2011,1), frequency = 12)

BR.train <- bankruptcy
l.BR.train <- log(BR.train)

HPI.train <- house_price
l.HPI.train <- log(HPI.train)

HPI.test <- house_price_test
l.HPI.test <- log(HPI.test)

#predict-rolling
pred.result.df <- data.frame()
for(i in 1:length(HPI.test)){
  m.co.3 <- arima(l.BR.train, order = c(2,1,1), 
                  seasonal = list(order = c(1,0,2), period = 12), 
                  xreg = data.frame(l.HPI.train))
  pred.l.BR <- predict(m.co.3, n.ahead = 1, newxreg = l.HPI.test[i])$pred
  
  se <- predict(m.co.3, n.ahead = 1, newxreg = l.HPI.test[i])$se
  upper <- pred.l.BR + 1.96*se
  lower <- pred.l.BR - 1.96*se
  l.BR.train<- c(l.BR.train, pred.l.BR)
  l.HPI.train <- c(l.HPI.train, l.HPI.test[i])
  pred.result <- c(pred.l.BR, lower, upper)
  pred.result.df <- rbind(pred.result.df, pred.result)
  
}
pred.result.c <- exp(pred.result.df)

names(pred.result.c) <- c("mean", "lower", "upper")
pred.result.c
write.csv(pred.result.c,  "prediction2011_2012.csv")

#plot predictions
pred  <-  pred.result.c$mean
l  <-  pred.result.c$lower 
h  <-  pred.result.c$upper

fit.predict.l <- fitted(m.co.3)
fit.predict.l.ts <- ts(fit.predict.l, start = c(1987,1), frequency = 12)
fit.l.ts <- window(fit.predict.l.ts, end=c(2010,12))
pred.ts <- ts(pred, start=c(2011,1),frequency = 12)
l.ts <- ts(l, start=c(2011,1),frequency = 12)
h.ts <- ts(h, start=c(2011,1),frequency = 12)

par(mfrow  =  c(1,  1))
plot(bankruptcy,main = "Forcast of Bankruptcy Rate", ylab = "Bankruptcy Rate")
abline(v  =  2011,  lwd  =  0.5,  col  =  "black")
lines(exp(fit.l.ts),col='green')
lines(pred.ts,col='red')
lines(l.ts,col='blue')
lines(h.ts,col='blue')
legend(1987, 0.045,lty=c(1,1),cex=0.8,
       col=c("black","red",'green','blue'),
       legend=c('Observed','Predicted','Fitted','95%PI'))
