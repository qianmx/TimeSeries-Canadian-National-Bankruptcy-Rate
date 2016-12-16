library("tseries")
library("forecast")
library("lawstat")
library('car')
library('hydroGOF')

#read in data 
#split data: train:validation = 8:2
train <- read.csv('train.csv')
bankruptcy <- train['Bankruptcy_Rate']

bankruptcy.train <- bankruptcy[1:228,]
bankruptcy.train <- ts(bankruptcy.train,start = c(1987,1), frequency = 12)
bankruptcy.test <- bankruptcy[229:288,]
bankruptcy.test <- ts(bankruptcy.test,start = c(2006,1), frequency = 12)

#Diffs
plot(bankruptcy.train, main='Bankruptcy Rate')
bankruptcy.train.l <- log(bankruptcy.train)
plot(bankruptcy.train.l)

par(mfrow=c(2,1))
acf(bankruptcy.train.l,lag.max=60)
pacf(bankruptcy.train.l,lag.max = 60)

#check ordinary diff
ndiffs(bankruptcy.train.l)
bankruptcy.train.l1 <- diff(bankruptcy.train.l)
acf(bankruptcy.train.l1,lag.max=72)
pacf(bankruptcy.train.l1,lag.max = 72)
adf.test(bankruptcy.train.l1)

#check seasonal diff: no need
#nsdiffs(bankruptcy_train1)
#bankruptcy_train1.12 <- diff(bankruptcy_train1,12)

#Order of p,P,q,Q
#p<=4, q<=2, P<=1, Q<=6
m1 <- arima(bankruptcy.train.l, order = c(2,1,1), seasonal = list(order = c(1,0,2),period = 12))

#Diagnostics
e<-m1$residuals
r<-e/sqrt(m1$sigma2)
tsdiag(m1)
par(mfrow=c(1,1))
qqPlot(e, 
       xlab = 'Quantiles', 
       ylab = 'Residuals', 
       main = 'Q-Q Plot')

#Test Plot & RMSE
arima_pred <-forecast(m1,h=60,level=0.95)

par(mfrow=c(1,1))
plot(arima_pred,main = "Test data: Predicted vs. Actual", ylab = "Bankruptcy Rate", xlab = "Month")
lines(log(bankruptcy.test),col='red')
fit <- bankruptcy.train.l-m1$residuals
lines(fit,col='green')
abline(v = 2006.01, lwd = 1, col = "black")
legend(1990, -2.5, lty=c(1,1),col=c('green',"blue","red"), legend=c('fitted','predicted','actual'))

arima_pred <-forecast(m1,h=60,level=0.95)
RMSE_arima <- rmse(exp(arima_pred$mean),bankruptcy.test)
RMSE_arima

#compare with Auto arima 
auto.arima(bankruptcy.train.l)



