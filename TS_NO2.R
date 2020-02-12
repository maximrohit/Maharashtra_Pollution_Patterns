#############################################
library(vrtest)
library(DBI)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(forecast)
library(tseries)
require(graphics)
library(ggplot2)
library(reshape)
library(gridExtra)
library(gsubfn)
########################################################

Base_data<- read.csv('E:/MS/data/Base_polpop_iell_csrf_17oct.csv')
colnames(Base_data)
col_names<-c("Type_of_Location" ,'City_poll','year_number','month_number','NO2_Median','NO2_P10','NO2_P90','SO2_Median','SO2_P10','SO2_P90','RSPM_PM10_Median','RSPM_PM10_P10','RSPM_PM10_P90','SPM_Median','SPM_P10','SPM_P90')
  
Base_data_pollutant<-Base_data[,col_names]  
unique(Base_data_pollutant$City_poll)
unique(Base_data_pollutant$Type_of_Location)
sapply(Base_data_pollutant,function(x) sum(is.na(x)))
# year_number     month_number       NO2_Median          NO2_P10          NO2_P90 
# 0                0               38               38               38 
# SO2_Median          SO2_P10          SO2_P90 RSPM_PM10_Median    RSPM_PM10_P10 
# 38               38               38             1738             1738 
# RSPM_PM10_P90       SPM_Median          SPM_P10          SPM_P90 
# 1738             2725             2725             2725 
Base_data_mum_no2<-Base_data_pollutant[which(Base_data_pollutant$City_poll=='MUMBAI' & Base_data_pollutant$Type_of_Location=='Residential'),c('Type_of_Location','City_poll','year_number','month_number','NO2_Median')]
min(Base_data_mum_no2$NO2_Median,na.rm=T)#4.45 
max(Base_data_mum_no2$NO2_Median,na.rm=T)#71.45
View(Base_data_mum_no2)
ggplot(data=Base_data_mum_no2,aes(x=NO2_Median))+geom_histogram()
#fragmented right tail, avg sifeted towards lft
ggplot(data=Base_data_mum_no2,aes(x=year_number))+geom_histogram()
##yera data is inconsistant
###################################################################################
nrow(Base_data_mum_no2)# 264
unique(Base_data_mum_no2$year_number)
Base_data_mum_no2=Base_data_mum_no2[order(Base_data_mum_no2$year_number, Base_data_mum_no2$month_number),]
list_all<-Base_data_mum_no2 %>%
  group_by(year_number) %>%
  summarise(ncount = n())
View(list_all)

View(Base_data_mum_no2)


#View(Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2007),])
Base_data_mum_no2_2007<-Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2007),]
nrow(Base_data_mum_no2_2007)##108
length(unique(Base_data_mum_no2_2007$year_number))#9
9*12#108
colnames(Base_data_mum_no2_2007)

ggplot(Base_data_mum_no2_2007,aes(x=year_number,y=NO2_Median, colour=as.factor(month_number)))+geom_point()
Base_data_mum_no2_2007_bkp<-Base_data_mum_no2_2007
#Base_data_mum_no2_2007<-Base_data_mum_no2_2007_bkp
View(Base_data_mum_no2_2007)

Base_data_mum_no2_2007<-Base_data_mum_no2_2007[which(Base_data_mum_no2_2007$year_number>=2011),]

nrow(Base_data_mum_no2_2007) 

#?stl
############################
Train<-Base_data_mum_no2_2007[1:30,] # 2015-17
Validate<-Base_data_mum_no2_2007[31:36,]# 2018
View(Validate)

Train<-Base_data_mum_no2_2007[1:96,] # 2015-17
Validate<-Base_data_mum_no2_2007[97:108,]# 2018
View(Validate)


Train<-Base_data_mum_no2_2007[1:48,] # 2015-17
Validate<-Base_data_mum_no2_2007[49:60,]# 2018


#plotting time series for train
ts_train<-ts(Train$NO2_Median,frequency = 12)
plot(ts_train)

#plotting time series for validate
ts_validate<-ts(Validate$NO2_Median,frequency = 12)
plot(ts_validate)




##################################################
#test for stationarity
# KPSS Test for Level Stationarity
kpss.test(ts_train)
###overall
# KPSS Test for Level Stationarity
# 
# data:  ts_train
# KPSS Level = 1.1971, Truncation lag parameter = 3, p-value = 0.01
# 
# Warning message:
#   In kpss.test(ts_train) : p-value smaller than printed p-value
##null hypothersis os serie sbeing stationary can be rejected
#########################################
##2011 onwards
# KPSS Test for Level Stationarity
# 
# data:  ts_train
# KPSS Level = 0.60569, Truncation lag parameter = 3, p-value = 0.02212
# 
# adf.test(ts_train, alternative="stationary")
##stationarty null hypothesis can be rejected
## 2012 onwards 
# KPSS Test for Level Stationarity
# 
# data:  ts_train
# KPSS Level = 0.16872, Truncation lag parameter = 3, p-value = 0.1
## not stationary
##2013 onwards
# KPSS Test for Level Stationarity
# 
# data:  ts_train
# KPSS Level = 0.48036, Truncation lag parameter = 2, p-value = 0.04609
##stationary can be rejected
adf.test(ts_train, alternative="stationary")
##2013 onwards
# Augmented Dickey-Fuller Test
# 
# data:  ts_train
# Dickey-Fuller = -3.0235, Lag order = 2, p-value = 0.1825
# alternative hypothesis: stationary
##not stationary cna;t be rejected
##overall
# Augmented Dickey-Fuller Test
# 
# data:  ts_train
# Dickey-Fuller = -5.7304, Lag order = 4, p-value = 0.01
# alternative hypothesis: stationary
##conradictory results again
##2011 onwards
# Augmented Dickey-Fuller Test
# 
# data:  ts_train
# Dickey-Fuller = -2.4624, Lag order = 3, p-value = 0.3894
# alternative hypothesis: stationary
###no stationary null hypothesye  can;r be rejected
## 2012 onwards
# Augmented Dickey-Fuller Test
# 
# data:  ts_train
# Dickey-Fuller = -3.5138, Lag order = 3, p-value = 0.05671
# alternative hypothesis: stationary
## not statonary

?Auto.VR

y <- ts_train                       
nob <- length(y)
r <- log(y[2:nob])-log(y[1:(nob-1)])      
Auto.VR(r)
##overall
# $stat
# [1] -1.315802 ## showing mean revision tendency
# 
# $sum
# [1] 0.7284306
###2011 onwards
# $stat
# [1] -1.746134
# 
# $sum
# [1] 0.4884781
##2012 onwards
# $stat
# [1] -1.942526
# 
# $sum
# [1] 0.3470265
##2013 onwards
# $stat
# [1] -1.521672
# 
# $sum
# [1] 0.3942021


# It is important to note here that
# even though statistically all are random, five minute return
# show tendency towards persistence (VR> 1) at all lags. Hourly
# returns show persistence (VR> 1) till lag 9 and later tendency
# towards mean reversion (VR< 1). On the other hand, daily
# returns show inclination always towards mean reversion
# (VR < 1). This is expected as information procession is slow
# at high frequency than at low frequency prices.

###
# Mean reversion is a theory used in finance that suggests that asset prices and 
# historical returns eventually will revert to the long-run mean or average level of the
# entire dataset. This mean can pertain to another relevant average, 
# such as economic growth or the average return of an industry




####lets check the (p)acf plots for our traing data
# Compute and plot the ACF
acf(ts_train,lag.max =11)
##overall
##6 spikes
#ma 1,2--both emajorly ,3,9,10,11 have gretaer impact
#2011 onwards 
### 1,2  major
##2012  onwards 2  
## 2013 onwards none

# Compute and plot the ACF for the time series
acf(ts_train, type="partial",lag.max =11)
##overall
#AR(7)  is higher, 5 & 8 are on teh verge but not higher
##2011 onwards
## 7 onwards
###2012 onwards 1
## 2013 onwards nonw
#############################################################################################################
##Let's check what model auto arima model does predicts
autoarima<-auto.arima(ts_train,stepwise=FALSE, approximation=FALSE)
saveRDS(autoarima, "E:/MS/Timeseries/code/autoarima_2013_Mumbai_no2_onwards.rds")

autoarima
##2013 onwards
# Series: ts_train 
# ARIMA(1,1,1) 
# 
# Coefficients:
#   ar1     ma1
# -0.9149  0.5989
# s.e.   0.1218  0.2923
# 
# sigma^2 estimated as 25.85:  log likelihood=-69.33
# AIC=144.65   AICc=145.92   BIC=148.06


##overall
# Series: ts_train 
# ARIMA(1,1,1)(1,0,1)[12] 
# 
# Coefficients:
#   ar1      ma1    sar1     sma1
# 0.6810  -0.9598  0.7774  -0.4752
# s.e.  0.0901   0.0298  0.1514   0.2085
# 
# sigma^2 estimated as 110.7:  log likelihood=-358.12
# AIC=726.24   AICc=726.92   BIC=739.01
##2011 onwards
# Series: ts_train 
# ARIMA(0,1,1)(0,0,1)[12] 
# 
# Coefficients:
#   ma1    sma1
# -0.3058  0.5021
# s.e.   0.1505  0.2428
# 
# sigma^2 estimated as 74.21:  log likelihood=-168.67
# AIC=343.33   AICc=343.89   BIC=348.88
## 2012 onwards

# Series: ts_train 
# ARIMA(0,0,2) with non-zero mean 
# 
# Coefficients:
#   ma1     ma2     mean
# 0.2332  0.6699  15.3945
# s.e.  0.1374  0.1162   1.6816
# 
# sigma^2 estimated as 32.04:  log likelihood=-112.53
# AIC=233.06   AICc=234.35   BIC=239.39

plot(ts_train, col="black")
lines(fitted(autoarima), col="red")## no drift
##residual
resi_auto_arima <- ts_train - fitted(autoarima)
#tets for stationarity in rsidual
plot(as.numeric(resi_auto_arima))
kpss.test(resi_auto_arima)
## residual is stationary 
# KPSS Test for Level Stationarity
# 
# data:  resi_auto_arima
# KPSS Level = 0.10467, Truncation lag parameter = 3, p-value = 0.1
##null hupothesis of series being stationalt can;t be rejected
##similar ofr ovael and 2011 onwards, 2013 onwars too

fcast_auto_arima <- forecast(autoarima, h = 12)
names(fcast_auto_arima)
colnames(Validate)


MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
MAPE_auto_arima
## 2013 onwards
# ME     RMSE      MAE       MPE     MAPE
# Test set 2.148553 13.71672 10.86962 -35.11424 66.97659
##overall
# MAPE_auto_arima
# ME     RMSE      MAE      MPE         MAPE
# Test set 8.415819 15.76932 11.65769 6.029712 49.17436
##2011 onwards
# ME     RMSE     MAE       MPE     MAPE
# Test set 2.26414 13.32369 10.2659 -32.08378 62.98804
## 2012 onwards
# ME     RMSE      MAE      MPE     MAPE
# Test set 9.283258 16.77146 13.14367 5.787235 56.47684


res<-cbind(pred=fcast_auto_arima$mean,actual=Validate$NO2_Median)
View(res)
####################################
?stl
##seasonal model
Stl = stl(ts_train,s.window='periodic')
#Stl = stl(ts_train,s.window=1)
#Stl=stl(ts_train,s.window=10,t.window=100,l.window=106,t.jump=80,l.jump=80)
Stl
plot(Stl)
names(Stl)

?forecast
Stl$weights
Stl$call
Stl$win
Stl$deg
Stl$jump
Stl$inner
Stl$outer

###till 2015 oct
options[29817,]
# i j k l  m  n
# 29817 9 5 1 1 21 13
Stl = stl(ts_train,s.window=9,t.window=5,
          l.window=1,s.jump=1,t.jump=21,
          l.jump=13)

?stl
### from 2011 till 2015 oct
options[20666,]
#i j  k l  m  n
#20666 11 6 21 6 16 16
Stl = stl(ts_train,s.window=11,t.window=6,
          l.window=21,s.jump=6,t.jump=16,
          l.jump=16, robust =TRUE)

#       i j k l m  n o p q
#36489 7 5 1 1 1 25 1 1 1
Stl = stl(ts_train,s.window=7,t.window=5,
          l.window=1,s.jump=1,t.jump=1,
          l.jump=25, robust =TRUE,s.degree=1,t.degree=1,l.degree=1)

fcast_auto_arima <- forecast(Stl, h = 12,method ='ets')
fcast_auto_arima_ets<-fcast_auto_arima
fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
fcast_auto_arima_arima<-fcast_auto_arima
fcast_auto_arima <- forecast(Stl, h = 12,method ='naive',s.window=7,l.jump=25,t.window=5,allow.multiplicative.trend = TRUE)
# fcast_auto_arima <- forecast(Stl, h = 12,method ='naive',s.window=7,t.window=5,
#                              l.window=1,s.jump=1,t.jump=1,
#                              l.jump=25, robust =TRUE,s.degree=1,t.degree=1,l.degree=1,allow.multiplicative.trend = TRUE)

fcast_auto_arima_Naive<-fcast_auto_arima

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
MAPE_auto_arima
View(cbind(ets=fcast_auto_arima_ets$mean,arima=fcast_auto_arima_arima$mean,
           naive=fcast_auto_arima_Naive$mean,actual=Validate$NO2_Median))

#2011 onwards stl degree
#36489 7 5 1 1 1 25 1 1 1
#ets
#ME     RMSE      MAE     MPE     MAPE
#Test set 8.562561 12.92608 8.978396 26.7879 30.23755
#arima
#8.567126 12.92911 8.980679 26.8151 30.2433
#naive
#8.234103 12.71088 8.814167 24.83111 29.82409

##2011 onwards
#i j  k l  m  n
#20666 11 6 21 6 16 16
# ME    RMSE      MAE      MPE     MAPE
#Test set 7.895019 15.3012 9.936532 10.05047 38.04911

## 2015 oct
#ME     RMSE      MAE      MPE     MAPE
#Test set 7.353303 14.25671 9.557622 7.902872 37.62975

pred_data=as.data.frame(cbind(month_number=Validate$month_number,
                              predicted=fcast_auto_arima$mean,actual=Validate$NO2_Median))
ggplot(pred_data, aes(x=month_number)) + 
  geom_line(aes(y = predicted), color = "darkred") + 
  geom_line(aes(y = actual), color="steelblue", linetype="twodash") 

### 5 year
fcast_auto_arima <- forecast(Stl, h = 60,method ='naive',allow.multiplicative.trend = TRUE)
var<- as.data.frame(fcast_auto_arima$mean)
data_120<-as.data.frame(cbind(month=1:60,NO2=var[1:60,]))
data_120


g<-ggplot(data_120,aes(x=month,y=NO2, colour=NO2))+geom_line()+ 
  scale_x_continuous(breaks = seq(1, 60, by = 3))+
  labs(title="Mumbai NO2 Seasonality",
       x ="Months", y = "NO2 level", size=30)+
  theme(axis.text.x=element_text(size=rel(2)),axis.text.y=element_text(size=rel(2)),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
g
ggsave('E:/MS/Timeseries/Images/Mumbai_NO2.png', plot = g,width = 10, height = 7.5)



##1 years
fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
var<- as.data.frame(fcast_auto_arima$mean)
data_120<-as.data.frame(cbind(month=1:12,NO2=var[1:12,]))
data_120
#ggplot(data_120,aes(x=month,y=NO2, colour=NO2))+geom_line()+ 
#  scale_x_continuous(breaks = seq(6, 60, by = 6))
g<-ggplot(data_120,aes(x=month,y=NO2, colour=NO2))+geom_line()+ 
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  labs(title="Mumbai NO2 Seasonality",
       x ="Months", y = "NO2 level", size=30)+
  theme(axis.text.x=element_text(size=rel(2)),axis.text.y=element_text(size=rel(2)),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
g
ggsave('E:/MS/Timeseries/Images/Mumbai_NO2_12.png', plot = g,width = 10, height = 7.5)

## 2013 onwards
# ME    RMSE      MAE       MPE     MAPE
# Test set 3.927955 13.5466 9.748615 -17.32788 50.71495
## overall
# ME    RMSE      MAE      MPE     MAPE
# Test set 11.19151 18.3794 14.54321 31.41327 64.24801
##2011 onwards 
# ME    RMSE      MAE      MPE     MAPE
# Test set 7.153997 14.6177 9.686643 6.193006 39.05116
## 2012 onwards 
# ME    RMSE      MAE       MPE     MAPE
# Test set 3.927955 13.5466 9.748615 -17.32788 50.71495
################################################################# done till here


#stl(ts_train,s.window=7,t.window=100,l.window=106,t.jump=80,l.jump=80)
#23.56999
Stl=stl(ts_train,s.window=1,t.window=100,l.window=106,t.jump=80,l.jump=80)
# ME     RMSE      MAE      MPE     MAPE
# Test set 117873.2 280810.9 234506.4 1.970572 21.32905

?stl
str(MAPE_auto_arima)
MAPE_auto_arima[1,5]
results=as.data.frame(i=0,mape=0)

#options <- expand.grid(i=1:5, j=1:5,k=1:5,l=1:5,m=1:5,n=1:5)
options <- expand.grid(i=seq(7,13,2), j=seq(1,9,2),k=seq(1,12,4),l=seq(1,7,2),m=seq(1,30,8),
                       n=seq(1,40,8),o=seq(0,1,1),p=seq(0,1,1),q=seq(0,1,1))

options
  STL.window<-function(x) {
  Stl = stl(ts_train,s.window=options[x,]$i,t.window=options[x,]$j,
            l.window=options[x,]$k,s.jump=options[x,]$l,t.jump=options[x,]$m,
            l.jump=options[x,]$n,robust =TRUE,s.degree=options[x,]$o,t.degree=options[x,]$p,l.degree=options[x,]$q)
  fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
  MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
  rbind(x,mape=MAPE_auto_arima[1,5])
}
STL.window.results=sapply(1:nrow(options),STL.window)
View(t(STL.window.results))
#witrh degree
#36489	30.24330
#17384	31.95741
options[36489,]
#       i j k l m  n o p q
#36489 7 5 1 1 1 25 1 1 1
options[17384,]
#i j k l m  n o p q
#17384 13 1 9 3 1 25 1 1 0

## without degree
#29817	37.62975
#20666	38.04911
###########################
### from 2011 till 2015 oct
options[20666,]
#i j  k l  m  n
#20666 11 6 21 6 16 16

###till 2015 oct

options[29817,]
# i j k l  m  n
# 29817 9 5 1 1 21 13
#########################
#12751-3, 6-8	40.58636
options[12751,]
plot(fcast_auto_arima$mean, type = "l", col='black')
lines(ts_validate, type = "l", col='red')

stl_etl_overall_swindow1=cbind(ts_validate,Validate$TotalAmt,as.vector(fcast_auto_arima$mean))
write.csv(stl_etl_overall_swindow1, file='D:/Timeseries_locationwise/Data_Prediction/stl_etl_overall_swindow1.csv')
plot(ts_validate, type = "l", col='black')
lines(as.vector(fcast_auto_arima$mean), col='red')14

#stl did better at predition gere
##ets
fcast_auto_arima <- forecast(Stl, h = 53,method ='ets')
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$TotalAmt)
MAPE_auto_arima
#slightly better
# ME     RMSE      MAE     MPE    MAPE
# Test set 151723.6 282847.7 231025.4 1.05222 24.3992

##rwdrift
fcast_auto_arima <- forecast(Stl, h = 53,method ='rwdrift')
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$TotalAmt)
MAPE_auto_arima
## drift is removed it goes horibally wrng
# ME     RMSE    MAE      MPE     MAPE
# Test set 899429 931225.8 899429 66.61359 66.61359

#################################################################################################

##global decomposition
#moving avg method
plot(ts_train)
width<-c(3,4,6,9)
cols <- c("red", "blue", "green","orange", "black")
labels <- c(paste("width =", width), "Original")
for (i in 1:length(width)){
  smoothedseries <- stats::filter(ts_train, filter=rep(1/width[i], width[i]),
                                  method="convolution", sides=2)
  
  lines(smoothedseries, col=cols[i], lwd=2)
  print(length(smoothedseries))
}
legend("topright", labels, col=cols, lwd=2, cex=.5,bg=8)




# we will use the width 4 as teh curve pattern  has a single chnage in curve till then, 
smoothedseries <- stats::filter(ts_train, filter=rep(1/4, 4),
                                method="convolution", sides=2)
## we will atempt with 18 4 and half weke smmoting
#smoothedseries <- stats::filter(ts_train, filter=rep(1/18, 18),
#                                method="convolution", sides=2)

sum(is.na(smoothedseries)) ##3

smoothedseries
length(smoothedseries)#48

#first  and last two are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values
#intial 1 values
smoothedseries[1]<-smoothedseries[2]-(smoothedseries[3]-smoothedseries[2])
##for 18
#diff=smoothedseries[10]-smoothedseries[9]
#for (i in 8:1){smoothedseries[i]=smoothedseries[i+1]-diff}

#last  values
#onwards 2011
length(smoothedseries)#48
smoothedseries[47]<-smoothedseries[46]+(smoothedseries[46]-smoothedseries[45])
smoothedseries[48]<-smoothedseries[47]+(smoothedseries[47]-smoothedseries[46])
#onwards 2013
length(smoothedseries)#30
smoothedseries[29]<-smoothedseries[28]+(smoothedseries[28]-smoothedseries[27])
smoothedseries[30]<-smoothedseries[29]+(smoothedseries[29]-smoothedseries[28])


##18
#diff=smoothedseries[150]-smoothedseries[149]
#for (i in 151:159){smoothedseries[i]=smoothedseries[i-1]+diff}

smoothedseries
####created new series

timevals<-c(1:length(smoothedseries))
timevals
smoothedseries<-cbind.data.frame(date_order=1:length(smoothedseries),
                                 Total_sales=as.vector(smoothedseries))
## no smoothing
#smoothedseries<-cbind.data.frame(date_order=1:length(Train$TotalAmt),
#                                 Total_sales=as.vector(Train$TotalAmt))
smoothedseries
######################################################################
get_residual_additive<- function (x,timevals){
  
  lmfit <- lm(Total_sales ~ poly(date_order,options[x,]$i)+sin(options[x,]$j*date_order)
              +   cos((1-options[x,]$j)*date_order)
              , data=smoothedseries)
  globalpred <- predict(lmfit, Month=timevals)      
  localpred <- ts_train - globalpred
  sum(localpred^2)
  
}

######################################################################

get_residual_multiplicative<- function (x,timevals){
  
  lmfit <- lm(Total_sales ~ poly(date_order,options[x,]$i)*sin(options[x,]$j*date_order)
              +   poly(date_order,options[x,]$i)*cos((1-options[x,]$j)*date_order)
              , data=smoothedseries)
  globalpred <- predict(lmfit, Month=timevals)      
  localpred <- ts_train - globalpred
  sum(localpred^2)
  
}
####################################################################
options <- expand.grid(i=1:5, j=seq(.1,1,by=.05))
options
#running regression for additive model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_additive,timevals)
plot(sum_of_sq_err)
axis(side=1, at=1:30)

options[order(sum_of_sq_err),]
##2011 onwards
#4 month smoothing
# i   j
# 15 3 0.5
# 14 2 0.5
# 20 2 0.7
# 21 3 0.7
# 11 2 0.4
# 12 3 0.4
#option 14 is lowest degree and 2nd lowest error
## 2013 onwards
# i   j
# 15 3 0.5
# 14 2 0.5
# 27 3 0.9
##
#i    j
#10 5 0.15
##2011 onwards and 2013 onwards
options[14,]#  2 0.5
sum_of_sq_err[14]#2011 2818.138 #2013 1498.945



##lets look at teh actual prediction
lmfit <- lm(Total_sales ~ poly(date_order,5)+sin(.15*date_order) + cos(.85*date_order), data=smoothedseries)
summary(lmfit)
##2011
# Residual standard error: 6.364 on 44 degrees of freedom
# Multiple R-squared:  0.6697,	Adjusted R-squared:  0.6471 
# F-statistic: 29.73 on 3 and 44 DF,  p-value: 1.164e-10
## 2013
# Residual standard error: 2.602 on 26 degrees of freedom
# Multiple R-squared:  0.7534,	Adjusted R-squared:  0.725 
# F-statistic: 26.48 on 3 and 26 DF,  p-value: 4.575e-08
##
# Residual standard error: 1.817 on 22 degrees of freedom
# Multiple R-squared:  0.8983,	Adjusted R-squared:  0.8659 
# F-statistic: 27.75 on 7 and 22 DF,  p-value: 1.644e-09
globalpred <- predict(lmfit, Month=timevals)
timevals
localpred <- ts_train - globalpred
#######################################
plot(ts_train,col='green', type = "l")#actual time series
lines(globalpred, col='red', type = "l")#trend line
lines(localpred, col='blue', type = "l")##not in range
plot(localpred, col='blue', type = "l")## their seems the pattern is their asis
#######################################################
#Checking teh Locla autoregressive part of Time series
#######################################################
# Compute and plot the ACF for the time remaining series
acf(localpred,lag.max =11)
## 4 smothed
##2011
#1,5,6,7
#2013
## 1,2,3

acf(localpred,lag.max =11, type="partial")
#seems all good
##2011
#4 mpnths smoothede 6,7 
##2013
# 0 has weird negative behavior all else are in range

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
#2011
##smoothed 4
# Series: localpred 
# ARIMA(1,0,0)(1,0,0)[12] with zero mean 
# 
# Coefficients:
#   ar1    sar1
# 0.4893  0.3185
# s.e.  0.1232  0.1835
# 
# sigma^2 estimated as 61.57:  log likelihood=-166.75
# AIC=339.5   AICc=340.05   BIC=345.11
##2013
# Series: localpred 
# ARIMA(0,0,0)(0,1,0)[12] 
# 
# sigma^2 estimated as 52.09:  log likelihood=-61.12
# AIC=124.23   AICc=124.48   BIC=125.12

#calculation residual
resi <- localpred-fitted(armafit_local)
# Compute and plot the ACF for the time series
acf(resi,lag.max =11)
#seems fine now##4 month smothed
##7 
acf(resi, type="partial",lag.max =11)
##6 jumped

adf.test(resi, alternative="stationary")
# Augmented Dickey-Fuller Test
# 
# data:  resi
# Dickey-Fuller = -3.3821, Lag order = 3, p-value = 0.07
# alternative hypothesis: stationary
kpss.test(resi)
# 
# KPSS Test for Level Stationarity
# 
# data:  resi
# KPSS Level = 0.1284, Truncation lag parameter = 4, p-value = 0.1
## stationarty tread can;t be discarded

########################################################
#Prediction and Accuracy mesurements
#########################################################
#lets predict and see the mape value


global_pred_out <- predict(lmfit,data.frame(date_order = 31:36))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 6)
global_pred_out
auto_arima_pred_out
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
global_pred_out
MAPE_sales <- accuracy(global_pred_out,Validate$NO2_Median)
MAPE_sales
#2011
##overall smotheed
# ME     RMSE     MAE       MPE     MAPE
# Test set -6.819938 13.71509 12.1319 -76.65401 87.93176
#2013

# ME     RMSE      MAE      MPE     MAPE
# Test set 8.189975 13.65848 8.956798 19.28817 24.66301
################################################################################################
################################################################################################
##########$$$$$$$$$$$$$$$ running regression for multiplicative model $$$$$$$$$$$$$###########
################################################################################################
################################################################################################
#running regression for multiplicative model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_multiplicative,timevals)
plot(sum_of_sq_err)
axis(side=1, at=1:30)
options[order(sum_of_sq_err),]
# ####4 month smothing
# i   j
# 21 3 0.7
# 12 3 0.4
# 9  3 0.3
# 11 2 0.4
#all 4 are candidates
sum_of_sq_err[11] #1904.966

sum_of_sq_err[order(sum_of_sq_err)]## any fom top 4 are fine## choosing 4
sum_of_sq_err[11]
options[11,]

###lets tets out itsprediction capabilities
lmfit <- lm(Total_sales ~ poly(date_order,2)*sin(.4*date_order)
            +   poly(date_order,1)*cos((.6)*date_order)
            , data=smoothedseries)
summary(lmfit)

## smmothes four quater
# Residual standard error: 2.369 on 40 degrees of freedom
# Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9511 
# F-statistic: 131.6 on 7 and 40 DF,  p-value: < 2.2e-16
timevals
globalpred <- predict(lmfit, Month=timevals)      
localpred <- ts_train - globalpred
sum(localpred^2)

#######################################
plot(ts_train,col='green', type = "l")#actual time series
lines(globalpred, col='red', type = "l")#trend line
lines(localpred, col='blue', type = "l")##not in range
plot(localpred, col='blue', type = "l")## their still seems to be an auto regressive residual pattern

#######################################################
#######################################################
#Checking teh Locla autoregressive part of Time series
#######################################################
# Compute and plot the ACF for the time remaining series
acf(localpred,lag.max =11)
### 7


# Compute and plot the ACF for the time series
acf(localpred,lag.max =11, type="partial")
#6 seems high

armafit_local <- auto.arima(localpred)
armafit_local
##4 weke smothing 
# Series: localpred 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 estimated as 41.75:  log likelihood=-157.67
# AIC=317.34   AICc=317.43   BIC=319.21

######################################################
#Similar Residual models for all options even non smoothen one but trimester and additive is best in term of validation performce

#calculation residual
resi <- localpred-fitted(armafit_local)
# Compute and plot the ACF for the time series
acf(resi,lag.max =11)
#4 weekseems fine now
##6 poking
# Compute and plot the ACF for the time series
acf(resi, type="partial",lag.max =11)
# 6 week

adf.test(resi, alternative="stationary")
# p-value = 0.06194, 
kpss.test(resi)
#p-value = 0.1,
########################################################
#Prediction and Accuracy mesurements
#########################################################
#lets predict and see the mape value


global_pred_out <- predict(lmfit,data.frame(date_order = 49:60))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 12)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_sales <- accuracy(global_pred_out,Validate$NO2_Median)
MAPE_sales
## smotheed 4  months curve
# ME     RMSE      MAE       MPE     MAPE
# Test set -16.06215 21.67966 18.78675 -148.6749 153.7205

plot(ts_validate, col="black")
lines(global_pred_out, col="red")##ORIGINAL NOT IN VIEW
plot(global_pred_out)
