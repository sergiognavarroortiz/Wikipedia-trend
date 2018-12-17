######################################################
# Forecasting Wikipedia trend with Prophet algorithm #
######################################################

##Collect, Explore and Prepare the Data
require(wikipediatrend)
page_views<-wp_trend("main_page",from = "2010-01-01",
                     to = "2016-01-20",
                     lang = "en") 

#Viewing the observations
head(page_views)
tail(page_views)

y<-page_views[,"count"]/10000000
ds<-page_views[,"date"]
data_sample<-data.frame(ds,y)

#Dealing with missing values
data_sample$y[y==0]<-NA
sum(is.na(data_sample$y))

#Visualize the distribution of missing values
require(imputeTS)
plotNA.distribution(data_sample$y,
                    breaks=100)

plotNA.distribution(data_sample)

#Replace missing values with NA
data_sample$y<-na.interpolation(
  data_sample$y,"linear")

plot(data_sample)

#Test and training sample
require(lubridate)
library(dplyr)
day<-"2016-01-01"
data_train<-data_sample %>%filter(ymd(ds)<ymd(day))
data_test<-data_sample %>%filter(ymd(ds)>=ymd(day))


head(data_train)
tail(data_train)

head(data_test)
tail(data_test)

##Build a Forecast Model
require(prophet)
set.seed(2019)
fit<-prophet(data_train)

attributes(fit)

#Fitted and predicted values
n_test<-nrow(data_test)
future<-make_future_dataframe(fit,periods = n_test)
pred<-predict(fit,future)

plot(fit,pred)  #The fitted and predicted values are denoted by the solid (blue) line, and the actual observations are denoted by the black dots. 

##Evaluate Model Performance
prophet_plot_components(fit,pred)

#Test set performance
pred_test<-pred %>%filter(ymd(ds) >=ymd(day))
require(Metrics)
round(rmse(pred_test$yhat,
           data_test$y),4)

##Improving Model Performance
#Adding holidays
require(tis)
hols<-holidaysBetween(20100101,20160120,
                      goodFriday = TRUE,
                      board = TRUE,
                      businessOnly = TRUE)

head(hols)

holidays<-names(hols)
ds<-as.Date(as.character(hols),"%Y%m%d")
df<-data.frame(holidays,ds)

#Building the model
set.seed(2019)
fit1<-prophet(data_train,
              growth = "linear",
              holidays = df,
              changepoint.prior.scale = 5)

#Test set performance
future1<-make_future_dataframe(fit1,
                               periods = n_test)
pred1<-predict(fit1,future1)
prophet_plot_components(fit1,pred1)

#Root Mean Squared Error
pred_test1<-pred1 %>%filter(ymd(ds)>=ymd(day))
require(Metrics)
round(rmse(pred_test1$yhat,
           data_test$y),4)








