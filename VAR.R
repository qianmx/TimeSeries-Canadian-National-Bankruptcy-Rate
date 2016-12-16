library("tseries")
library("forecast")
library("lawstat")
library('car')
library('hydroGOF')
library('vars')

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

#fit the model
df2 <- data.frame(log(bankruptcy.train),
                  population.train,house_price.train)
VARselect(y = df2)
var1 <- VAR(y = df2, p = 10)

# plot the actual, fitted, predicted value
var_pred_raw <- predict(var1, n.ahead = 84, ci = 0.95)
var_pred <- ts(data.frame(var_pred_raw$fcst$log.bankruptcy.train)$fcst, start=c(2006,01),frequency = 12)
fit <- data.frame(fitted(var1))$log.bankruptcy.train.
fit.ts <- ts(fit,start = c(1987,01),frequency = 12)
  
par(mfrow=c(1,1))
plot(log(bankruptcy.train),main = "Test data: Predicted vs. Actual", ylab = "Bankruptcy Rate", xlab = "Month")
lines(log(bankruptcy.test),col='red')
lines(fit.ts,col='green')
abline(v = 2006.01, lwd = 1, col = "black")
legend(1987, -3.5, lty=c(1,1),col=c('green',"blue","red"), legend=c('fitted','predicted','actual'))

# check rmse on test_tr dataset
var_rmse <- rmse(exp(var_pred),bankruptcy.test)
var_rmse

#Diagnostic
plot(var1)



