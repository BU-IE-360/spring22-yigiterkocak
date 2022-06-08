
require(data.table)
require(forecast)
library(urca)
library(MuMIn)
library(MLmetrics)
library(Metrics)
library(GGally)


todays_date=Sys.Date()-30
forecast_date=todays_date

weather<-fread("2022-06-02_weather.csv")
production<-fread("2022-06-02_production.csv")

weather$date<-as.Date(weather$date)
production$date<-as.Date(production$date)

production[, quarter:= as.factor(quarter(date))]
 


daily_prod=production[,list(mean_production=mean(production,na.rm=T)),by=list(date)]


plot(daily_prod,type="l")

windows()
plot(production$production[],type="l")

windows()


decomposed<-decompose(ts(daily_prod$mean_production,freq=24))
plot(decomposed)

# identify # of days to forecast
latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)

# get the latest n_days and modify for forecasting
# creates date,hour,production for the upcoming days
forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]

forecasted_production[,production:=NA]

# actual production data with forecasted dates
production_with_forecast=rbind(production,forecasted_production)

# create a template for forecast date
forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)

production_with_forecast=production_with_forecast[order(date,hour)]
production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)

wide_weather=dcast(weather,date+hour~variable+lat+lon,value.var='value')

production_with_weather=merge(production_with_forecast,wide_weather,by=c('date','hour'))

production_with_weather[,hour:=as.character(hour)]
train_data=production_with_weather[!is.na(production)]
test_data=production_with_weather[is.na(production)]


#Model1
lm_model=lm(production~.,train_data[,-c('date'),with=F])
summary(lm_model)
checkresiduals(lm_model)
train_data
lm_forecast=predict(lm_model,test_data)
test_data[,forecasted:=as.numeric(lm_forecast)]

daily_max_production=production_with_forecast[,list(max_prod=max(production)),by=list(date)]
daily_max_production[,rolling_max:=frollapply(max_prod,30,max,na.rm=T)]

production_with_weather_capacity=merge(production_with_weather,daily_max_production,by=c('date'))
production_with_weather_capacity[,normalized_production:=production/rolling_max]

production_with_weather_capacity[,hour:=as.character(hour)]
train_data=production_with_weather_capacity[!is.na(production)]
test_data=production_with_weather_capacity[is.na(production)]

# train with all variables

#Model2
lm_model2=lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model2)
checkresiduals(lm_model2)


#Model3
production_with_weather_capacity[,lag1:=shift(normalized_production,1)]

lm_model3<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model3)
checkresiduals((lm_model3))
pacf(lm_model3$residuals)


#Model4


production_with_weather_capacity[,lag7:=shift(normalized_production,7)]


lm_model4<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
# 
summary(lm_model4)
checkresiduals((lm_model4))



#Model5
production_with_weather_capacity[,lag24:=shift(normalized_production,24)]

lm_model5<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model5)
checkresiduals((lm_model5))
pacf(lm_model5$residuals)

production_with_weather_capacity[,lag26:=shift(normalized_production,26)]

#Model6
lm_model6<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model6)
checkresiduals((lm_model6))

pacf(lm_model5$residuals)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}


forecast_with_lr=function(fmla, data,forecast_data){
  fitted_lm=lm(as.formula(fmla),data)
  forecasted=predict(fitted_lm,forecast_data)
  return(list(forecast=as.numeric(forecasted),model=fitted_lm))
}


test_start=as.Date('2022-05-01')
test_end=as.Date('2022-05-31')

test_dates=seq(test_start,test_end,by='day')
test_dates

forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  
  past_data=production_with_weather_capacity[date<='2022-04-30']
  past_data=past_data[,-c('date','rolling_max','production','max_prod'),with=F]
  forecast_data=production_with_weather_capacity[date==test_dates[i]]
  
  # first lm models
  fmla='normalized_production~-1'
  forecasted=forecast_with_lr(fmla,past_data,forecast_data)
  forecast_data[,lm_prediction:=forecasted$forecast]
  
  
  results[[i]]=forecast_data
}

mean(overall_results$lm_prediction)

AICc(lm_model6)






