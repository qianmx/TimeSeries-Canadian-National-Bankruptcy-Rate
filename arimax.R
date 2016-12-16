library("tseries")
library("forecast")
library("lawstat")
library('car')
library('hydroGOF')

#read in data
train <- read.csv('train.csv')

bankruptcy <- train['Bankruptcy_Rate']
unemployment_rate <- train['Unemployment_Rate']
population <- train['Population']
house_price_index <- train['House_Price_Index']

bankruptcy.train <- bankruptcy[1:228,]
bankruptcy.train <- ts(bankruptcy.train,start = c(1987,1), frequency = 12)
bankruptcy.test <- bankruptcy[229:288,]
bankruptcy.test <- ts(bankruptcy.test,start = c(2006,1), frequency = 12)

unemployment_rate.train <- unemployment_rate[1:228,]
unemployment_rate.train <- ts(unemployment_rate.train,start = c(1987,1), frequency = 12)
unemployment_rate.test <- unemployment_rate[229:288,]
unemployment_rate.test <- ts(unemployment_rate.test,start = c(2006,1), frequency = 12)

population.train <- population[1:228,]
population.train <- ts(population.train,start = c(1987,1), frequency = 12)
population.test <- population[229:288,]
population.test <- ts(population.test,start = c(2006,1), frequency = 12)

house_price.train <- house_price_index[1:228,]
house_price.train <- ts(house_price.train,start = c(1987,1), frequency = 12)
house_price.test <- house_price_index[229:288,]
house_price.test <- ts(house_price.test,start = c(2006,1), frequency = 12)

#Plot the data
par(mfrow=c(4,1))
plot(bankruptcy.train, main='Bankruptcy Rate')
plot(unemployment_rate.train, main='Unemployment Rate')
plot(population.train, main='Population')
plot(house_price.train, main='House Price')

#Order of p,P,q,Q
#p<=4, q<=2, P<=1, Q<=6
m1 <- arima(log(bankruptcy.train), 
            order = c(2,1,1), 
            seasonal = list(order = c(1,0,2),period=12),
            xreg = data.frame(log(house_price.train))
            )

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
arima_pred <-
  forecast(m1,h=60,level=0.95,xreg=data.frame(log(house_price.test)))

fit <- log(bankruptcy.train)-m1$residuals
par(mfrow=c(1,1))
plot(arima_pred,main = "Test data: Predicted vs. Actual", ylab = "Bankruptcy Rate", xlab = "Month")
lines(log(bankruptcy.test),col='red')
lines(fit,col='green')
abline(v = 2006.01, lwd = 1, col = "black")
legend(1990, -3.0, lty=c(1,1),col=c('green',"blue","red"), legend=c('fitted','predicted','actual'))

RMSE_arima <- rmse(exp(arima_pred$mean),bankruptcy.test)
RMSE_arima


#Compare Auto arima
auto.arima(log(bankruptcy.train),
           xreg = data.frame(log(house_price.train)))



