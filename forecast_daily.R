library(forecast)
library(lubridate)

# Read Data
daily_prices = read.csv('Data/Multi-Year Price Data (Aggregate).csv')
daily_prices$Date = as.Date(daily_prices$Date, format='%m/%d/%Y')  

# Remove outliers - STL decomposition
outliers = tsoutliers(daily_prices$Price, lambda = 'auto')
plot(daily_prices$Price)
points(outliers$index, daily_prices$Price[outliers$index], col='red')
points(outliers$index, outliers$replacements, col='blue')
price_wo_outliers = daily_prices$Price
price_wo_outliers[outliers$index] = outliers$replacements

# Visualize substituted data
plot(price_wo_outliers)
points(outliers$index, outliers$replacements, col='red')

# Defice MSTS object with weekly, monthly and yearly
msts = msts(price_wo_outliers, seasonal.periods = c(7, 30.4, 365), start=decimal_date(daily_prices$Date[1]))
plot(msts)

# Split into train and test datasets
training = subset(msts, end = length(msts)-365)
test = subset(msts, start = length(msts)-364)

# Method 1: TBATS
# Run TBATS model
tbats_training = tbats(training)
  plot(tbats_training, main='Multiple Season Decomposition')

# Forecast test model
tbats_training %>% 
  forecast(h=365, level=50) %>%
  autoplot() + autolayer(test)

fc_tbats <- forecast(tbats_training,h=1504)
plot(fc_tbats, main = "TBATS Forecast")

# Check residuals
checkresiduals(fc_tbats)

# Looks good. Retrain using all of the available data
# tbats_full_set = tbats(msts, use.arma.errors = TRUE)
# plot(tbats_full_set, main='Multiple Season Decomposition')
# 
# fc_full_set<- forecast(tbats_full_set, h=1505, level=0)
# plot(fc_full_set)


# Method 2: Fourier autoarima
# Credit to: https://www.kaggle.com/kailex/arima-with-fourier-terms

# Fit base model
fit0 <- auto.arima(training)
(bestfit <- list(aicc=fit0$aicc, i=0, j=0, fit=fit0))

# Find best combination of Fourier series for each of the frequencies
# This can take a while to run
for(i in 1:3) {
  for (j in 1:3){
    for (k in 1:3){
      z0 <- fourier(ts(training, frequency=7), K=i)
      z1 <- fourier(ts(training, frequency=30.4), K=j)
      z2 <- fourier(ts(training, frequency=365), K=k)
      fit <- auto.arima(training, xreg=cbind(z0, z1, z2))
      if(fit$aicc < bestfit$aicc) {
        bestfit <- list(aicc=fit$aicc, i=i, j=j, k=k, fit=fit)
      }
    }
  }
}
bestfit

#Fit Best model
fc <- forecast(bestfit$fit, 
               xreg=cbind(
                 fourier(ts(training, freq=7), K=bestfit$i, h=1504),
                 fourier(ts(training, frequency=30.4), K=bestfit$j, h=1504),
                 fourier(ts(training, frequency=365), K=bestfit$k, h=1504)))
plot(fc)

#Check Residuals
checkresiduals(fc)

accuracy(fc_tbats, test)
accuracy(fc, test)

# Fourier Method performs better
# Train on full data set with (i,j,k) = (3,1,3)
z0 <- fourier(ts(msts, frequency=7), K=3)
z1 <- fourier(ts(msts, frequency=30.4), K=1)
z2 <- fourier(ts(msts, frequency=365), K=3)
fit <- auto.arima(msts, xreg=cbind(z0, z1, z2))
fc <- forecast(fit, 
               xreg=cbind(
                 fourier(ts(training, freq=7), K=3, h=1504),
                 fourier(ts(training, frequency=30.4), K=1, h=1504),
                 fourier(ts(training, frequency=365), K=3, h=1504)))
plot(fc)
plot(tail(fc$mean, 29))
