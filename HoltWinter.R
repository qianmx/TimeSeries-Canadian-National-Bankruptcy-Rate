library("tseries")
library("forecast")
library("lawstat")
library('car')
library('hydroGOF')

#read in data
train <- read.csv('train.csv')
bankruptcy <- train['Bankruptcy_Rate']
bankruptcy.train <- bankruptcy[1:228,]
bankruptcy.train <- ts(bankruptcy.train,start = c(1987,1), frequency = 12)
bankruptcy.test <- bankruptcy[229:288,]
bankruptcy.test <- ts(bankruptcy.test,start = c(2006,1), frequency = 12)
bankruptcy.train.l <- log(bankruptcy.train)

# Holt-Winters: Tripple Exponential Smoothing
hw1 <- HoltWinters(alpha = 0.038, beta = 0.69, gamma = 1, x = bankruptcy.train.l, seasonal = "add")

#Test Plot & RMSE
hw1_pred <- forecast(hw1, h = 60)
RMSE_arima <- rmse(exp(hw1_pred$mean),bankruptcy.test)
RMSE_arima

#Diagnostics
plot(hw1_pred$residuals, main='residual vs t',ylab="")
abline(h=0, col="red")
acf(hw1_pred$residuals,na.action = na.pass)

#plot
fit <- data.frame(hw1$fitted)$xhat
fit.ts <- ts(fit, start=c(1987,1),frequency = 12)
par(mfrow=c(1,1))
plot(hw1_pred,main = "Test data: Predicted vs. Actual", ylab = "Bankruptcy Rate", xlab = "Month")
lines(log(bankruptcy.test),col='red')
fit <- bankruptcy.train.l-hw1$residuals
lines(fit.ts,col='green')
abline(v = 2006.01, lwd = 1, col = "black")
legend(1990, -1.5, lty=c(1,1),col=c('green',"blue","red"), legend=c('fitted','predicted','actual'))


