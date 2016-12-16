# Smoothing Examples
library(forecast)

#read in data
train <- read.csv('train.csv')
test <- read.csv('test.csv')

mth <- train['Month']

bankruptcy <- train['Bankruptcy_Rate']
bankruptcy <- ts(bankruptcy,start = c(1987,1), frequency = 12)

######################################################################
# Double Exponential Smoothing
par(mfrow = c(1,1))
plot(bankruptcy, main = "Bankruptcy Rate", ylab = "Rate", xlab = "Month")
par(mfrow = c(2,1))
hw.CH <- HoltWinters(x = bankruptcy, gamma = F) 
plot(hw.CH)    #give more weight to recent value, so 
plot(forecast(hw.CH, h = 24))


