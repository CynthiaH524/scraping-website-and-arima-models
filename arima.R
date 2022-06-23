#Mar 24, Thu
#install packages for scraping data - rvest library
#install.packages("rvest")
library(tidyverse)
library(forecast)
library(rvest)

## 1.	Import data by scraping the website.
# to scrape website implement the following:
# url -> read_html -> html_nodes -> html_table
url = "https://www.transtats.bts.gov/Data_Elements.aspx?Data=1"

# put the mouse at the table - right click - inspect - find id - select & copy xpath
passengers = url %>% read_html %>% 
  html_nodes(xpath = '//*[@id="GridView1"]') %>% 
  html_table() %>% 
  data.frame()
# [[1]] means that the data is stored in a list format

# How R treats each col
glimpse(passengers)
passengers$Month = as.numeric(passengers$Month)

# removing TOTAL and comma (converting to numeric)
passengers <- passengers %>% filter(!grepl('TOTAL', Month))

passengers$DOMESTIC <- as.numeric(gsub(pattern = ",",replacement = "", x = passengers$DOMESTIC))
passengers$INTERNATIONAL <- as.numeric(gsub(",","",passengers$INTERNATIONAL))
passengers$TOTAL <- as.numeric(gsub(",","",passengers$TOTAL))


# alternatively
#passengers[passengers$Month != "TOTAL",]

#
y = as.numeric(gsub(pattern = ",",replacement = "", x = passengers$DOMESTIC))
# create a monthly time series object
head(passengers)
tail(passengers)
y = ts(y, start = c(2002,10), frequency = 12) # -> data is monthly, let r know it needs to check annual seasonality
y
# daily data - seasonality if annual, then the frequency = [(365*3 + 366)/4] = 365.25

autoplot(y)+geom_point()+theme_bw()

## Scenario 1: ##
#Training set: start = 10/2002, end=12/2007
#Testing set:   start = 01/2008, end=12/2008

# Creating training and testing sets
y.train = window(y, end = c(2007, 12))
y.test = window(y, start = c(2008,1), end=c(2008,12))


## Scenario 2: ##
#Training set: start = 10/2002, end=12/2009
#Testing set:   start = 01/2010, end=12/2010

# Creating training and testing sets
y.train = window(y, end = c(2009, 12))
y.test = window(y, start = c(2010,1), end=c(2010,12))


## Scenario 3: ##
# Creating training and testing sets
y.train = window(y, end = c(2018, 1))
y.test = window(y, start = c(2018,2), end=c(2019,1))

## Scenario 4: ##
# Creating training and testing sets
y.train = window(y, end = c(2019, 12))
y.test = window(y, start = c(2020,1), end=c(2021,12))

################################################################################
# model
# Plot
autoplot(y.train) + autolayer(y.test, size = 1.5) + theme_bw()

# Build ARIMA model
# lambda = "auto" means transform that data to stabilize variance if needed
# lambda = 0 - log transformation y* = (Y^lambda -1)/lambda
# p is the number of autoregressive terms,
# d is the number of nonseasonal differences needed for stationarity, 
# q is the number of lagged forecast errors in the prediction equation.
M1 = auto.arima(y.train, lambda = "auto")
M1

# Predict on testing set
M1F = forecast(M1, h=length(y.test), level = 95)
M1F

# Accuracy metrics:
accuracy(M1F, y.test)
accuracy(M1F, y.test)[,"MAPE"]
accuracy(M1F, y.test)[1,5]
accuracy(M1F, y.test)['Training set',c('RMSE', "MAPE")]
class(accuracy(M1F, y.test))

# error in the testing set -> sign of overfitting in the training set 
#                             or for TS there's something happen

# plot data and over predictions and fitted values
autoplot(y.train) + autolayer(y.test) + theme_bw() + autolayer(M1F$fitted) +
  autolayer(M1F$mean) #M1F$mean future forecast (on training sets)

# regression, smoothing, and arima (auto arima)

#M2 - Exponential smoothing model
M2 = ets(y.train, lambda = 'auto')
M2

M2F = forecast(M2, h=length(y.test), level = 95)
M2F

accuracy(M2F, y.test)[,c('RMSE', 'MAPE')]

#metrics = rbind(accuracy(M1F, y.test)[,c('RMSE', 'MAPE')], accuracy(M2F, y.test)[,c('RMSE', 'MAPE')])
#rownames(metrics)
#row.names(metrics) <- (c('M1_Training set', 'M1_Test set', 'M2_Training set', 'M2_Test set'))

paste(rownames(metrics), c(rep("ARIMA",2), rep("ETS",2)))
#rownames(metrics) = paste(rownames(metrics), c(rep("ARIMA",2), rep("ETS",2)))
#metrics

# Naive model: F(t+1) = y(t)
M3 = snaive(y = y.train, h = length(y.test), level = 95)
#snaive = seasonal naive; naive = naive (use when there is no seasonality)
M3
M3F = forecast(M3, h=length(y.test), level = 95)
M3F
accuracy(M3F, y.test)[,c('RMSE', 'MAPE')]

autoplot(y.train) + autolayer(y.test) + theme_bw() + autolayer(M3F$fitted) +
  autolayer(M3F$mean) 

################################################################################
# Metrics
metrics = rbind(
  accuracy(M1F, y.test)[,c('RMSE', 'MAPE')], 
  accuracy(M2F, y.test)[,c('RMSE', 'MAPE')], 
  accuracy(M3F, y.test)[,c('RMSE', 'MAPE')]
)

rownames(metrics) = paste(rownames(metrics), c(rep('ARIMA', 2), rep('ETS', 2), rep('Naive',2)))
colnames(metrics) = paste("Scenario 1", colnames(metrics))
colnames(metrics) = paste("Scenario 2", colnames(metrics))
colnames(metrics) = paste("Scenario 3", colnames(metrics))
colnames(metrics) = paste("Scenario 4", colnames(metrics))

################################################################################
#allScenarios= metrics
allScenarios = cbind(allScenarios, metrics)
allScenarios


# visualize 3 models
# plot testing set and pverlay M1-M3 forecasts
autoplot(y.test) + autolayer(M1F$mean)+ autolayer(M2F$mean)+ autolayer(M3F$mean)



################################################################################
# Determine the champion model: find average of MAPE on test
# Set for each model
allScenarios[c(2,4,6), c(2,4,6,8)] %>%  apply(1,mean)
allScenarios[c(2,4,6), c(2,4,6,8)] %>%  apply(1,median)


allScenarios[c(2,4,6), c(2,4,6,8)] %>% rowMeans()
# median might be a better approach


# Use the champion model to predict future 12 months data
yc = window(y, end=c(2020,1))
M = auto.arima(yc, lambda = "auto")
M

MF = forecast(M, h=48, level = 95)

MF

autoplot(MF, size = 2) + theme_bw()+ autolayer(MF$mean, size = 1.5) +
  autolayer(MF$fitted, size = 1)
