library(RcppRoll)
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(data.table)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(forecast)
library(dplyr)
library(xts)

.libPaths()

## Introduction ##
#In this study, we are supposed to forecast UGS of the next four quarters of the given data. To do this,
#using regression methods are primary task.Our data contains many independent variables. Before forecasting, 
#we first need to analyse data statiscally and build our model accordingly to forecast the sales.



## Data Manipulation and Initialization##

hw2_data = read.csv("IE360_Spring22_HW2_data.csv", colClasses =c('character',rep('numeric',10)))

colnames(hw2_data) = c("Quarters", "UGS", "RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT") 


hw2_data$Quarters = as.Date(as.yearqtr(hw2_data$Quarters,format = "%Y_Q%q"))

str(hw2_data)# structure is fine with all numerics.

hw2_original=hw2_data[c(1:28),]# split dataset into two, original set and test set with missing output values

hw2_test=hw2_data[c(29:32),]


hw2_original= data.table(hw2_original)
hw2_test= data.table(hw2_test)
hw2_original
hw2_test

## Visualisation of the data ##

ggplot(hw2_original, aes(x=Quarters,y=UGS))+geom_line()+geom_point()+
  labs(title = "Unleaded Gasoline Sales Between 2000-2007")+theme_classic()+scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#It is obvious that there is a declining trend behaviour. As for seasonality, in every  3rd quarter of 
# the number of sales peeks. As this scenario occurs once in 4 quarters or yearly, it can be assumed that
#the data has yearly seasonality. Thus, as it is visually obvious, trend and seasonality are the principle components for the model.


mean_ts=roll_mean(hw2_data$UGS[1:28],4,align='left')
var_ts=roll_var(hw2_data$UGS[1:28],4,align='left')

plot(mean_ts, type='l',col='blue', xlab = "time (t)", ylab = "Rolling Mean of Series",main = "Mean of Time Series of UGS")
   
plot(var_ts,type='l',col='green',xlab = "time (t)",ylab = "Rolling Variance of Series",main = "Variance of Time Series of UGS ")

### There is a clear indication that the data is not stationary with respect to the mean. However,
#it seems that stationarity with respect to the variance holds when inspected with naked eyes.###    

## Autocorrelation function of UGS up to lag 14##

    
acf(hw2_original$UGS)

#comment here

#Lags 1, 4 and 8 has high correlations. However, lag 4 is attributed to the seasonality term, and lag 8 is also a multiple of 
#4, namely 2 season prior to the current data, they have similar effects. Moreover, Lag 1 might be implying the trend in the data.

seasonality=seq(1,4,by=1) # this will be utilised as seasonality
hw2_original=cbind(hw2_original,seasonality)

#hw2_data$quareters will be used to explain the trend

##BUILDING THE MODEL##

#Checking the correlations#

#For better forecasting purposes, variables with significant with UGS will be considered to build the model.

ggpairs(hw2_original)

# As it is seen on the plot, the ones with asterikses are significant in explaining the target. 
#Accordingly, GNPA, NDGV, NUGV, PG, PU, NLPG are the candidates to build the model.

# Quarters are also highly correlated,as trend component,  it will be used in the model.




### Start building with adding seasonality and trend ###

model0=lm(UGS~as.factor(seasonality)+Quarters, data=hw2_original)

summary(model0)

checkresiduals(model0)

# as expected, seasonality and trend made a very good work in prediction.
# corresponding t values for seasonality and trend are perfect. As for the model,
# it manifests promising in terms of r squared value and p-value. Additionally, shape of residuals
#quite resemble a normal distribution, however, we still somewhat worrying autocorrelations at lag 1 and 2.
#Lag1 and lag2 of some variables can be added to the model to overcome this.
#check cross-correlations of independent variables with residuals to see whether further improvements can be done on residuals
#hence, improvement in overall model and r squared.
ccf(model0$residuals,hw2_original$RNUV)
ccf(model0$residuals, hw2_original$NLPG)
ccf(model0$residuals, hw2_original$PU)
ccf(model0$residuals, hw2_original$PG)
ccf(model0$residuals, hw2_original$NUGV)
ccf(model0$residuals, hw2_original$NDGV)

# lag 1 of RNUV seems  correlated with the current residuals let's add it to the model.


hw2_original$RNUV_lag1=lag(hw2_original$RNUV,1)

model1=lm(UGS~as.factor(seasonality)+Quarters+RNUV_lag1, data=hw2_original)
summary(model1)

checkresiduals(model1)


hw2_original$RNUV_lag1
hw2_original$RNUV
hw2_test$RNUV
hw2_data$RNUV
#When we look at the r squared value, the model seems very promising. As for other measures, there is no
# significant autocorrelation in the residuals and they form a shape like that of a normal distribution.
#Moreover, lastly added lagged variable is significant in the model.This model seems useful.



##Prediction##


hw2_test$RNUV_lag1[c(1:4)]=hw2_data$RNUV[c(28:31)]
hw2_test
prediction=predict(model1,hw2_test)


#numerical results

prediction



