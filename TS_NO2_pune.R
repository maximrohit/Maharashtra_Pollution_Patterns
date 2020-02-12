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
Base_data_mum_no2<-Base_data_pollutant[which(Base_data_pollutant$City_poll=='PUNE' & Base_data_pollutant$Type_of_Location=='Residential'),c('Type_of_Location','City_poll','year_number','month_number','NO2_Median')]
min(Base_data_mum_no2$NO2_Median,na.rm=T)#9.2 
max(Base_data_mum_no2$NO2_Median,na.rm=T)#94
View(Base_data_mum_no2)
ggplot(data=Base_data_mum_no2,aes(x=NO2_Median))+geom_histogram()
#fragmented right tail, avg sifeted towards lft
ggplot(data=Base_data_mum_no2,aes(x=year_number))+geom_histogram()

ggplot(data=Base_data_mum_no2,aes(x=year_number,y=NO2_Median,col=month_number))+geom_point()
##yera data is inconsistant
Base_data_mum_no2 %>%
  group_by(year_number) %>%
  summarise(ncount = n())
###################################################################################
nrow(Base_data_mum_no2)# 264
unique(Base_data_mum_no2$year_number)
Base_data_mum_no2=Base_data_mum_no2[order(Base_data_mum_no2$year_number, Base_data_mum_no2$month_number),]

View(Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2004),])
Base_data_mum_no2_2007<-Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2004),]

Base_data_mum_no2_2007<-Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2004 & 
                                                  Base_data_mum_no2$year_number<=2015),]

nrow(Base_data_mum_no2_2007)
View(Base_data_mum_no2_2007)
#2004-15 till 8 oct onwards
Train<-Base_data_mum_no2_2007[1:128,] 
Validate<-Base_data_mum_no2_2007[129:140,]

nrow(Base_data_mum_no2_2007)##143,71, 59
length(unique(Base_data_mum_no2_2007$year_number))#12,6
12*12#144
colnames(Base_data_mum_no2_2007)

ggplot(Base_data_mum_no2_2007,aes(x=year_number,y=NO2_Median, colour=as.factor(month_number)))+geom_point()
#Base_data_mum_no2_2007_bkp<-Base_data_mum_no2_2007
#Base_data_mum_no2_2007<-Base_data_mum_no2_2007_bkp
View(Base_data_mum_no2_2007)
#Base_data_mum_no2_2007<-Base_data_mum_no2_2007[which(Base_data_mum_no2_2007$year_number>=2013),]

nrow(Base_data_mum_no2_2007) 
############################



plot(Base_data_mum_no2_2007$NO2_Median)
Train<-Base_data_mum_no2_2007[1:60,] # 2015-17
Validate<-Base_data_mum_no2_2007[61:71,]# 2018
View(Validate)
Train<-Base_data_mum_no2_2007[1:132,] # 2015-17
Validate<-Base_data_mum_no2_2007[133:143,]# 2018
#2011 onwards
Train<-Base_data_mum_no2_2007[1:48,] # 2015-17
Validate<-Base_data_mum_no2_2007[49:59,]# 2018
#2012 onwards
Train<-Base_data_mum_no2_2007[1:36,] 
Validate<-Base_data_mum_no2_2007[37:47,]
#2013 onwards
Train<-Base_data_mum_no2_2007[1:24,] 
Validate<-Base_data_mum_no2_2007[25:35,]


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
###2004
# KPSS Test for Level Stationarity
# 
# data:  ts_train
# KPSS Level = 0.088642, Truncation lag parameter = 4, p-value = 0.1
# 
# Warning message:
#2010 onwards
#KPSS Level = 0.15594, Truncation lag parameter = 3, p-value = 0.1
## 2011
#KPSS Level = 0.4653, Truncation lag parameter = 3, p-value = 0.04948

#   In kpss.test(ts_train) : p-value greater than printed p-value
##null hypothersis os serie sbeing stationary can't be rejected
#########################################
adf.test(ts_train, alternative="stationary")
#2011 onwards
#Dickey-Fuller = -3.416, Lag order = 3, p-value = 0.06478
##2004 onwards
# Augmented Dickey-Fuller Test
# 
# data:  ts_train
# Dickey-Fuller = -4.9626, Lag order = 5, p-value = 0.01
# alternative hypothesis: stationary
##non stationary can be rejected 
##2010 onwards
# Augmented Dickey-Fuller Test
# 
# data:  ts_train
# Dickey-Fuller = -3.4181, Lag order = 3, p-value = 0.06152
# alternative hypothesis: stationary
## no stationarity can;t b rejected

?Auto.VR

y <- ts_train                       
nob <- length(y)
r <- log(y[2:nob])-log(y[1:(nob-1)])      
Auto.VR(r)
#2011 onwards
# $stat
# [1] 0.6489159
# 
# $sum
# [1] 1.178315
##2004 overall
#$stat
# [1] 0.08615694
# 
# $sum
# [1] 1.010683

##2010 onwards
# $stat
# [1] 0.5690078
# 
# $sum
# [1] 1.135558

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
#2011 onwards
#1,2,6,7,10,11
##overall 2004 onwards
##2,3,4,8,7
##2010 onwards
##1,2,6,7

# Compute and plot the ACF for the time series
acf(ts_train, type="partial",lag.max =11)
#2011 onwards all good
##overall 2004 onwards
#AR(2,4)  is higher
## 2010 onwards
##4
#############################################################################################################
##Let's check what model auto arima model does predicts
autoarima<-auto.arima(ts_train,stepwise=FALSE, approximation=FALSE)
saveRDS(autoarima, "E:/MS/Timeseries/code/autoarima_2011_pune_no2_onwards.rds")

autoarima
#2013 onwrads
# ARIMA(4,0,0) with non-zero mean 
# 
# Coefficients:
#   ar1      ar2     ar3      ar4     mean
# 0.8881  -0.3245  0.3421  -0.5298  41.1933
# s.e.  0.1914   0.2695  0.2551   0.1784   2.4692
# 
# sigma^2 estimated as 57.3:  log likelihood=-81.13
# AIC=174.26   AICc=179.2   BIC=181.33
##2012 onwards
# Series: ts_train 
# ARIMA(0,0,0)(1,1,0)[12] with drift 
# 
# Coefficients:
#   sar1    drift
# -0.7148  -0.1754
# s.e.   0.1413   0.0704
# 
# sigma^2 estimated as 32.05:  log likelihood=-78.91
# AIC=163.82   AICc=165.02   BIC=167.35

#2011 onwards
# Series: ts_train 
# ARIMA(0,1,1)(0,1,0)[12] 
# 
# Coefficients:
#   ma1
# -0.6917
# s.e.   0.1891
# 
# sigma^2 estimated as 94.63:  log likelihood=-129.1
# AIC=262.21   AICc=262.58   BIC=265.32
##2004 onwards
# Series: ts_train 
# ARIMA(2,0,3) with non-zero mean 
# 
# Coefficients:
#   ar1      ar2      ma1     ma2     ma3     mean
# 1.6411  -0.8890  -0.8933  0.2189  0.2863  43.4604
# s.e.  0.0691   0.0681   0.1144  0.0941  0.0858   1.4239
# 
# sigma^2 estimated as 45.83:  log likelihood=-437.73
# AIC=889.45   AICc=890.36   BIC=909.63
##2010 owards
# Series: ts_train 
# ARIMA(4,1,0)(1,1,0)[12] 
# 
# Coefficients:
#   ar1      ar2      ar3     ar4     sar1
# -0.2607  -0.3060  -0.2185  0.2568  -0.6395
# s.e.   0.1428   0.1434   0.1430  0.1475   0.1135
# 
# sigma^2 estimated as 68.89:  log likelihood=-166.93
# AIC=345.86   AICc=347.96   BIC=356.96

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
# KPSS Level = 0.11799, Truncation lag parameter = 4, p-value = 0.1
##null hupothesis of series being stationalt can;t be rejected
##similar ofr ovael and 2011 onwards, 2013 onwars too

fcast_auto_arima <- forecast(autoarima, h = 11)
names(fcast_auto_arima)
colnames(Validate)


MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
MAPE_auto_arima
##2012 onwards
# ME    RMSE      MAE      MPE     MAPE
# Test set 12.57728 21.1878 16.64857 13.68493 28.49467
## 2012 onwards
# ME     RMSE      MAE      MPE     MAPE
# Test set 18.40853 22.31014 18.40853 31.48687 31.48687
##2011 onwafds
#ME     RMSE     MAE      MPE     MAPE
#Test set 12.21587 18.21146 14.1539 17.16997 24.33387
## 2004 onwards
# ME     RMSE      MAE      MPE     MAPE
# Test set 10.31457 19.13486 14.82286 9.670271 25.99953
##2010 onwards
# ME     RMSE      MAE     MPE    MAPE
# Test set 21.77266 25.03609 21.77266 39.0451 39.0451

res<-cbind(pred=fcast_auto_arima$mean,actual=Validate$NO2_Median)
View(res)
####################################
?stl
##seasonal model
Stl = stl(ts_train,s.window='periodic')

###2004 onwards stl balanced
Stl = stl(ts_train,s.window=1,t.window=1,
          l.window=1,s.jump=5,t.jump=1,
          l.jump=1)

##2010 onwards 1 1 1 3 5 1
Stl = stl(ts_train,s.window=1,t.window=1,
          l.window=1,s.jump=3,t.jump=5,
          l.jump=1)
##2011 onwards 1 1 4 3 5 5
Stl = stl(ts_train,s.window=1,t.window=1,
          l.window=4,s.jump=3,t.jump=5,
          l.jump=5)


## 2004- 2015 oct
#5413	14.66468
#options[5413,]
# i j k l  m n
# 5413 1 9 1 5 17 1
Stl = stl(ts_train,s.window=1,t.window=9,
          l.window=1,s.jump=5,t.jump=17,
          l.jump=1,robust =TRUE)


## 2004- 2015 oct above 7
#4600	16.43676
#options[4600,]
#i j  k l  m n
#4600 7 6 11 1 21 1
Stl = stl(ts_train,s.window=7,t.window=6,
          l.window=11,s.jump=1,t.jump=21,
          l.jump=1,robust =TRUE)
##ets
#ME     RMSE      MAE      MPE    MAPE
#Test set 6.598374 13.21419 9.267357 9.803113 16.7916
## ME     RMSE      MAE      MPE     MAPE arima
##Test set 4.547996 12.40183 8.880073 4.927592 16.43676

# names(Stl)
# stl_remain<-Stl$time.series[,'trend'] - Stl$time.series[,'seasonal']
# View(stl_remain)
# stl_remain_train<-ts(stl_remain,frequency = 12)
# 
# stl2<-stl(stl_remain_train,s.window='periodic',robust =TRUE)
# 
# 
# fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
# fcast_auto_arima_remain <- forecast(stl2, h = 12,method ='arima')
# 
# MAPE_auto_arima <- accuracy(fcast_auto_arima_remain$mean,Validate$NO2_Median)
# MAPE_auto_arima
# View(cbind(fcast_auto_arima_remain$mean,Validate$NO2_Median))

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

?forecast

#17001	13.76010
options[17001,]
#i j k l  m  n o p q
#17001 7 1 5 7 17 17 1 1 0
Stl = stl(ts_train,s.window=7,t.window=1,
          l.window=5,s.jump=7,t.jump=17,
          l.jump=17, robust =TRUE,s.degree=1,t.degree=1,l.degree=0)

fcast_auto_arima <- forecast(Stl, h = 12,method ='ets',allow.multiplicative.trend = TRUE)
fcast_auto_arima_ets<-fcast_auto_arima
fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
fcast_auto_arima_arima<-fcast_auto_arima
fcast_auto_arima <- forecast(Stl, h = 12,method ='naive',allow.multiplicative.trend = TRUE)
fcast_auto_arima_Naive<-fcast_auto_arima


names(fcast_auto_arima)

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
MAPE_auto_arima

View(cbind(fcast_auto_arima$mean,Validate$NO2_Median))
# with degree
#ME     RMSE      MAE        MPE    MAPE
#Test set 1.591392 10.23083 7.543743 0.04355494 13.7601

#ME     RMSE      MAE      MPE    MAPE
#Test set 6.598374 13.21419 9.267357 9.803113 16.7916

fcast_auto_arima$mean
  
Stl %>% seasadj() %>% ets() %>%
  autoplot() + ylab("NO2 Level") 
  


pred_data=as.data.frame(cbind(month_number=Validate$month_number,
                              predicted=fcast_auto_arima$mean,actual=Validate$NO2_Median))
View(pred_data)

pred_data[which(!(pred_data$month_number %in% c(3,8,10,11,12))),]
pred_data[which(!(pred_data$month_number %in% c(3,8,10,11,12))), 
          'predicted']=pred_data[which(!(pred_data$month_number %in% c(3,8,10,11,12))), 'predicted']*1.15


pred_data[which(pred_data$month_number %in% c(9,11)), 'predicted']*1.05

ggplot(pred_data, aes(x=month_number)) + 
  geom_line(aes(y = predicted), color = "darkred") + 
  geom_line(aes(y = actual), color="steelblue", linetype="twodash") 
axis(side=1, at=1:12)

fcast_auto_arima <- forecast(Stl, h = 16,method ='arima')
var<- as.data.frame(fcast_auto_arima$mean)
data_120<-as.data.frame(cbind(month=1:12,NO2=var[5:16,]))


g<-ggplot(data_120,aes(x=month,y=NO2, colour=NO2))+geom_line()+ 
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  labs(title="Pune NO2 Seasonality",
       x ="Months", y = "NO2 level", size=30)+
  theme(axis.text.x=element_text(size=rel(2)),axis.text.y=element_text(size=rel(2)),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
g
ggsave('E:/MS/Timeseries/Images/Pune_NO2_12.png', plot = g,width = 10, height = 7.5)


ggplot(data_120,aes(x=month,y=NO2, colour=NO2))+geom_line()+ 
  scale_x_continuous(breaks = seq(6, 60, by = 6))



## 2004- 2015 oct
#5413	14.66468
#options[5413,]
# i j k l  m n
# 5413 1 9 1 5 17 1
#ets
# ME     RMSE      MAE      MPE     MAPE
# Test set 4.195894 9.026377 6.802696 5.773441 13.71463



##2011 onwards ets 1 1 4 3 5 5
##              ME     RMSE      MAE      MPE     MAPE
#Test set 13.24328 19.23886 14.75854 18.57721 24.40511
##2011 onwards arima 1 1 4 3 5 5
# ME     RMSE      MAE      MPE     MAPE
# Test set 13.43336 19.37019 14.91405 18.99044 24.68543


##2011 onwars ets
# ME     RMSE      MAE      MPE     MAPE
##2011 onwards arima
#ME     RMSE      MAE      MPE     MAPE
#Test set 20.02919 24.43451 20.12631 33.09851 33.47203
# Test set 16.05468 21.14823 17.02429 24.28624 28.01379
# 2010 onwards modfied stl ets
# ME     RMSE      MAE      MPE     MAPE
# Test set 11.87335 18.03519 13.64039 16.64467 22.92047
# 2010 onwards modfied stl arima
# ME     RMSE      MAE      MPE     MAPE
# Test set 11.21099 17.65848 13.36504 14.93199 22.58971

#2010 onwards stl base ets
# ME     RMSE      MAE      MPE     MAPE
# Test set 16.64677 21.31903 17.35157 26.04992 28.76066
#2010 onwards base stl arima
#ME     RMSE      MAE      MPE     MAPE
#Test set 14.28715 19.46107 15.65023 21.09043 26.24444
## 2004 ets onwards
# ME     RMSE     MAE      MPE     MAPE
# Test set 12.99407 20.36901 16.0318 15.69097 26.97075
## 2004 arima onwards
#ME     RMSE      MAE      MPE     MAPE
#Test set 11.37783 19.51413 15.42464 12.01453 26.81161
##2004 onwards stl 111511 ets
# ME     RMSE      MAE      MPE     MAPE
# Test set 14.62075 19.66342 15.55229 22.45222 26.03507
##2004 onwards stl 111511 arima
#ME     RMSE      MAE      MPE     MAPE
#Test set 12.33374 18.14034 13.74545 17.15431 22.51107

################################################################# done till here


# #stl(ts_train,s.window=7,t.window=100,l.window=106,t.jump=80,l.jump=80)
# #23.56999
# Stl=stl(ts_train,s.window=1,t.window=100,l.window=106,t.jump=80,l.jump=80)
# # ME     RMSE      MAE      MPE     MAPE
# # Test set 117873.2 280810.9 234506.4 1.970572 21.32905
# 
# ?stl
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
            l.jump=options[x,]$n,robust =TRUE,
            s.degree=options[x,]$o,t.degree=options[x,]$p,l.degree=options[x,]$q)
  fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
  MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
  rbind(x,mape=MAPE_auto_arima[1,5])
}
STL.window.results=sapply(1:nrow(options),STL.window)
View(t(STL.window.results))
#17001	13.76010
options[17001,]
#i j k l  m  n o p q
#17001 7 1 5 7 17 17 1 1 0
# 
# 
# 
# 
# options <- expand.grid(i=seq(7,40,4), j=seq(1,21,5),k=seq(1,21,5),l=seq(1,21,5),m=seq(1,21,5),n=seq(1,21,5))
# options
# options
#   STL.window<-function(x) {
#   Stl = stl(ts_train,s.window=options[x,]$i,t.window=options[x,]$j,
#             l.window=options[x,]$k,s.jump=options[x,]$l,t.jump=options[x,]$m,
#             l.jump=options[x,]$n, robust =TRUE)
#   fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
#   MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$NO2_Median)
#   rbind(x,mape=MAPE_auto_arima[1,5])
# }
# STL.window.results=sapply(1:nrow(options),STL.window)
# View(t(STL.window.results))
## 2004- 2015 oct
#5413	14.66468
#options[5413,]
# i j k l  m n
# 5413 1 9 1 5 17 1

## 2004- 2015 oct
#4600	16.43676
#options[4600,]
#i j  k l  m n
#4600 7 6 11 1 21 1


#12751-3,   
#i j k l m n
#501-8 1 1 1 5 1 1
options[501,]
#STL.window.results=sapply(1:nrow(options),STL.window)
##2010 onwards
options[2751,]
#i j k l m n
#2751 1 1 1 3 5 1
## 2011 onwars
options[15326,]
#1 1 4 3 5 5

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
#onwards 2004 onwards
length(smoothedseries)#132
smoothedseries[131]<-smoothedseries[130]+(smoothedseries[130]-smoothedseries[129])
smoothedseries[132]<-smoothedseries[131]+(smoothedseries[131]-smoothedseries[130])
##2010 onwards
#onwards 2004 onwards
length(smoothedseries)#60
smoothedseries[59]<-smoothedseries[58]+(smoothedseries[58]-smoothedseries[57])
smoothedseries[60]<-smoothedseries[59]+(smoothedseries[59]-smoothedseries[58])



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
options <- expand.grid(i=1:3, j=seq(.1,1,by=.5))
options
#running regression for additive model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_additive,timevals)
plot(sum_of_sq_err)
axis(side=1, at=1:57)
#24,27
head(options[order(sum_of_sq_err),])
##2011 onwards
# i    j
# 27 3 0.50
# 24 3 0.45
##2011 onwards and 2013 onwards
options[27,]#  2 0.5
sum_of_sq_err[27]#2011 2818.138 #2013 1498.945
## 2010 onwards
# i    j
# 27 3 0.50
# 30 3 0.55
# 26 2 0.50
# 29 2 0.55
# 25 1 0.50
# 28 1 0.55
options[27,]#  3 0.5


##lets look at teh actual prediction
lmfit <- lm(Total_sales ~ poly(date_order,1)+sin(.5*date_order) + cos(.5*date_order), data=smoothedseries)
summary(lmfit)
##2004onwards
# Residual standard error: 8.243 on 126 degrees of freedom
# Multiple R-squared:  0.2691,	Adjusted R-squared:  0.2401 
# F-statistic: 9.278 on 5 and 126 DF,  p-value: 1.525e-07
##2010 onwards
# Residual standard error: 5.589 on 54 degrees of freedom
# Multiple R-squared:  0.7919,	Adjusted R-squared:  0.7726 
# F-statistic: 41.09 on 5 and 54 DF,  p-value: < 2.2e-16
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
##2004 onwards
#1,2,3,6,7
##2010 smoothed
#1,6,7,8
acf(localpred,lag.max =11, type="partial")
##2004 , 4

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
#2004
##smoothed 4
# Series: localpred 
# ARIMA(2,0,1) with zero mean 
# 
# Coefficients:
#   ar1      ar2      ma1
# 1.5824  -0.6974  -0.7591
# s.e.  0.4203   0.2530   0.5377
# 
# sigma^2 estimated as 43.71:  log likelihood=-435.61
# AIC=879.23   AICc=879.54   BIC=890.76
##2010 onwards
# Series: localpred 
# ARIMA(1,0,0) with zero mean 
# 
# Coefficients:
#   ar1
# 0.5029
# s.e.  0.1120
# 
# sigma^2 estimated as 52.33:  log likelihood=-203.51
# AIC=411.01   AICc=411.22   BIC=415.2

#calculation residual
resi <- localpred-fitted(armafit_local)
# Compute and plot the ACF for the time series
acf(resi,lag.max =11)
#seems fine now##4 month smothed
##7 
acf(resi, type="partial",lag.max =11)
##3 jumped

adf.test(resi, alternative="stationary")
# Augmented Dickey-Fuller Test
# 
# data:  resi
# Dickey-Fuller = -4.398, Lag order = 5, p-value = 0.01
# alternative hypothesis: stationary
kpss.test(resi)
# 
# KPSS Test for Level Stationarity
# 
# data:  resi
# KPSS Level = 0.1284, Truncation lag parameter = 4, p-value = 0.1
## stationarty tread can;t be discarded
##2010 onwards
# Augmented Dickey-Fuller Test
# 
# data:  resi
# Dickey-Fuller = -3.558, Lag order = 3, p-value = 0.04435
# alternative hypothesis: stationary


########################################################
#Prediction and Accuracy mesurements
#########################################################
#lets predict and see the mape value


global_pred_out <- predict(lmfit,data.frame(date_order = 61:71))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 11)
global_pred_out
auto_arima_pred_out
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
global_pred_out
MAPE_sales <- accuracy(global_pred_out,Validate$NO2_Median)
MAPE_sales
#2004
##overall smotheed
# ME    RMSE      MAE      MPE     MAPE
# Test set 21.00638 27.5214 21.17106 33.37213 33.96025
##.8 and degree 1
# MAPE_sales
# ME     RMSE      MAE      MPE     MAPE
# Test set 13.96221 21.38518 16.44321 17.71064 26.98862
## 2010 onwards deg 3 ,  .5
# MAPE_sales
# ME    RMSE      MAE       MPE     MAPE
# Test set -13.38543 22.7654 19.46869 -42.66571 49.13726
## 2010 onwards degree 1 , .5
View(cbind(global_pred_out,Validate$NO2_Median))
################################################################################################
################################################################################################
##########$$$$$$$$$$$$$$$ running regression for multiplicative model $$$$$$$$$$$$$###########
################################################################################################
################################################################################################
#running regression for multiplicative model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_multiplicative,timevals)
plot(sum_of_sq_err)
axis(side=1, at=1:57)
head(options[order(sum_of_sq_err),])
# ####4 month smothing
# i    j
# 27 3 0.50
# 24 3 0.45
sum_of_sq_err[27] #1904.966
##2010 onwards
# i    j
# 30 3 0.55
# 27 3 0.50
# 29 2 0.55
# 21 3 0.40
# 33 3 0.60
# 26 2 0.50
sum_of_sq_err[order(sum_of_sq_err)]## any fom top 4 are fine## choosing 4
sum_of_sq_err[27]
options[27,]

###lets tets out itsprediction capabilities
lmfit <- lm(Total_sales ~ poly(date_order,2)*sin(.5*date_order)
            +   poly(date_order,2)*cos((.5)*date_order)
            , data=smoothedseries)
summary(lmfit)

## smmothes four quater
##3 degree , .5
# Residual standard error: 6.431 on 122 degrees of freedom
# Multiple R-squared:  0.5692,	Adjusted R-squared:  0.5374 
# F-statistic: 17.91 on 9 and 122 DF,  p-value: < 2.2e-16
##2 degree , .5
# Residual standard error: 7.216 on 124 degrees of freedom
# Multiple R-squared:  0.4488,	Adjusted R-squared:  0.4177 
# F-statistic: 14.42 on 7 and 124 DF,  p-value: 1.265e-13
##degree 1
# Residual standard error: 7.392 on 126 degrees of freedom
# Multiple R-squared:  0.4122,	Adjusted R-squared:  0.3889 
# F-statistic: 17.67 on 5 and 126 DF,  p-value: 3.068e-13
### 2010 onwards 3, .55
# Residual standard error: 4.949 on 50 degrees of freedom
# Multiple R-squared:  0.8489,	Adjusted R-squared:  0.8217 
# F-statistic: 31.21 on 9 and 50 DF,  p-value: < 2.2e-16

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
### 1,2,3,4


# Compute and plot the ACF for the time series
acf(localpred,lag.max =11, type="partial")
#all good

armafit_local <- auto.arima(localpred)
armafit_local
##4 weke smothing 
#degree 3
# Series: localpred 
# ARIMA(1,0,0) with zero mean 
# 
# Coefficients:
#   ar1
# 0.6679
# s.e.  0.0653
# 
# sigma^2 estimated as 36.23:  log likelihood=-424.02
# AIC=852.04   AICc=852.13   BIC=857.81
##degree 2
# Series: localpred 
# ARIMA(1,0,0) with zero mean 
# 
# Coefficients:
#   ar1
# 0.7224
# s.e.  0.0619
# 
# sigma^2 estimated as 38.01:  log likelihood=-427.27
# AIC=858.53   AICc=858.63   BIC=864.3
##degree 1
# Series: localpred 
# ARIMA(1,0,0) with zero mean 
# 
# Coefficients:
#   ar1
# 0.7376
# s.e.  0.0612
# 
# sigma^2 estimated as 38.53:  log likelihood=-428.18
# AIC=860.36   AICc=860.45   BIC=866.13
## 2010 onwards
#ARIMA(0,0,1) with zero mean 
# 
# Coefficients:
#   ma1
# 0.3614
# s.e.  0.1020
# 
# sigma^2 estimated as 44.99:  log likelihood=-198.9
# AIC=401.8   AICc=402.01   BIC=405.99

######################################################
#Similar Residual models for all options even non smoothen one but trimester and additive is best in term of validation performce

#calculation residual
resi <- localpred-fitted(armafit_local)
# Compute and plot the ACF for the time series
acf(resi,lag.max =11)
#4 weekseems fine now
## all good
# Compute and plot the ACF for the time series
acf(resi, type="partial",lag.max =11)
# 3 week

adf.test(resi, alternative="stationary")
# p-value = 0.01, 
kpss.test(resi)
#p-value = 0.1,
########################################################
#Prediction and Accuracy mesurements
#########################################################
#lets predict and see the mape value


#global_pred_out <- predict(lmfit,data.frame(date_order = 61:71))

global_pred_out <- predict(lmfit,data.frame(date_order = 133:143))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 11)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_sales <- accuracy(global_pred_out,Validate$NO2_Median)
MAPE_sales
## smotheed 4  months curve
#degree 3
# ME     RMSE      MAE      MPE     MAPE
# Test set 22.79086 26.08815 22.79086 39.89376 39.89376
##degree 2
# ME     RMSE     MAE      MPE     MAPE
# Test set 10.4806 17.19437 12.7409 12.65307 21.07222
#degree 1
#ME     RMSE      MAE      MPE     MAPE
#Test set 12.04821 18.90782 13.61912 17.25511 21.73923
## deg 1 .55
# ME     RMSE      MAE     MPE     MAPE
# Test set 18.70396 29.01517 20.15429 26.8812 32.06093
plot(ts_validate, col="black")
lines(global_pred_out, col="red")##ORIGINAL NOT IN VIEW
plot(global_pred_out)
View(cbind(Validate$NO2_Median,global_pred_out))

res<-as.data.frame(cbind(actual=Validate$NO2_Median,base_pred=global_pred_out))

res$base_20<-res$base_pred*1.2
View(res)

