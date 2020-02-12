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
col_names<-c("Type_of_Location" ,'City_poll','year_number','month_number','NO2_Median','NO2_P10',
             'NO2_P90','SO2_Median','SO2_P10','SO2_P90','RSPM_PM10_Median','RSPM_PM10_P10','RSPM_PM10_P90','SPM_Median','SPM_P10','SPM_P90')

Base_data_pollutant<-Base_data[,col_names]  

Base_data_mum_no2<-Base_data_pollutant[which(Base_data_pollutant$City_poll=='PUNE' & 
                                               Base_data_pollutant$Type_of_Location=='Residential'
                                             ),
                                       c('Type_of_Location','City_poll','year_number','month_number',
                                         'SO2_Median')]
min(Base_data_mum_no2$SO2_Median,na.rm=T)#2.5 
max(Base_data_mum_no2$SO2_Median,na.rm=T)#49
View(Base_data_mum_no2)
ggplot(data=Base_data_mum_no2,aes(x=SO2_Median))+geom_histogram()
#fragmented right tail, avg sifeted towards lft
ggplot(data=Base_data_mum_no2,aes(x=year_number))+geom_histogram()

ggplot(data=Base_data_mum_no2,aes(x=year_number,y=SO2_Median,col=month_number))+geom_point()
##yera data is inconsistant
Base_data_mum_no2 %>%
  group_by(year_number) %>%
  summarise(ncount = n())
###################################################################################
nrow(Base_data_mum_no2)# 184
unique(Base_data_mum_no2$year_number)
Base_data_mum_no2=Base_data_mum_no2[order(Base_data_mum_no2$year_number, Base_data_mum_no2$month_number),]

View(Base_data_mum_no2)
#View(Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2004),])
Base_data_mum_no2_2007<-Base_data_mum_no2[which(Base_data_mum_no2$year_number>=2004 & Base_data_mum_no2$year_number<=2015),]
nrow(Base_data_mum_no2_2007)##143,71, 59, 2012 47
length(unique(Base_data_mum_no2_2007$year_number))#12,6
12*12#144
colnames(Base_data_mum_no2_2007)
year_number
ggplot(Base_data_mum_no2_2007,aes(x=month_number,y=SO2_Median, colour=as.factor(year_number)))+geom_point()
#Base_data_mum_no2_2007_bkp<-Base_data_mum_no2_2007
#Base_data_mum_no2_2007<-Base_data_mum_no2_2007_bkp
View(Base_data_mum_no2_2007)
Base_data_mum_no2_2007<-Base_data_mum_no2_2007[which(Base_data_mum_no2_2007$year_number>=2010),]

nrow(Base_data_mum_no2_2007) 
View(Base_data_mum_no2_2007[1:140,])
############################



plot(Base_data_mum_no2_2007$NO2_Median)
Train<-Base_data_mum_no2_2007[1:60,]
Validate<-Base_data_mum_no2_2007[61:71,]
View(Validate)
Train<-Base_data_mum_no2_2007[1:132,] 
Validate<-Base_data_mum_no2_2007[133:143,]
#2011 onwards
Train<-Base_data_mum_no2_2007[1:48,] 
Validate<-Base_data_mum_no2_2007[49:60,]
#2012 onwards
Train<-Base_data_mum_no2_2007[1:36,] 
Validate<-Base_data_mum_no2_2007[37:47,]
#2013 onwards
Train<-Base_data_mum_no2_2007[1:24,] 
Validate<-Base_data_mum_no2_2007[25:35,]

#2004 onwards
Train<-Base_data_mum_no2_2007[1:120,] 
Validate<-Base_data_mum_no2_2007[121:132,]


#2004-15 till 8 oct onwards
Train<-Base_data_mum_no2_2007[1:128,] 
Validate<-Base_data_mum_no2_2007[129:140,]

Train<-Base_data_mum_no2_2007[1:56,] 
Validate<-Base_data_mum_no2_2007[57:68,]

View(Train)
View(Validate)

nrow(Base_data_mum_no2_2007)
nrow(Validate)
nrow(Train)

View(Train)

#plotting time series for train
ts_train<-ts(Train$SO2_Median,frequency = 12)
plot(ts_train)

#plotting time series for validate
ts_validate<-ts(Validate$SO2_Median,frequency = 12)
plot(ts_validate)




##################################################
#test for stationarity
# KPSS Test for Level Stationarity
kpss.test(ts_train)
###2012
#KPSS Level = 0.0962, Truncation lag parameter = 3, p-value = 0.1

##null hypothersis of serie sbeing stationary can't be rejected
#########################################
adf.test(ts_train, alternative="stationary")
#2012 onwards
#Dickey-Fuller = -3.6698, Lag order = 3, p-value = 0.04168
##not stationary null hupothesis cna be rejected


##auto vr
y <- ts_train                       
nob <- length(y)
r <- log(y[2:nob])-log(y[1:(nob-1)])      
Auto.VR(r)
#2012 onwards
# $stat
# [1] -0.7800081
# 
# $sum
# [1] 0.7634304

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
#2012 onwards
#1

# Compute and plot the ACF for the time series
acf(ts_train, type="partial",lag.max =11)
#2012 onwards all good

#############################################################################################################
##Let's check what model auto arima model does predicts
autoarima<-auto.arima(ts_train,stepwise=FALSE, approximation=FALSE)
saveRDS(autoarima, "E:/MS/Timeseries/code/autoarima_2010_14_pune_So2_onwards.rds")

autoarima
#2012 onwrads
# Series: ts_train 
# ARIMA(1,0,0) with non-zero mean 
# 
# Coefficients:
#   ar1     mean
# 0.3790  19.7448
# s.e.  0.1511   0.9134
# 
# sigma^2 estimated as 12.68:  log likelihood=-95.85
# AIC=197.7   AICc=198.45   BIC=202.45
######2010-14
# ARIMA(1,1,1) 
# 
# Coefficients:
#   ar1      ma1
# 0.5781  -0.9562
# s.e.  0.1581   0.1074
# 
# sigma^2 estimated as 35.98:  log likelihood=-150.49
# AIC=306.98   AICc=307.54   BIC=312.53


plot(ts_train, col="black")
lines(fitted(autoarima), col="red")## no drift
##residual
resi_auto_arima <- ts_train - fitted(autoarima)
#tets for stationarity in rsidual
plot(as.numeric(resi_auto_arima))
kpss.test(resi_auto_arima)
## residual is stationary , 2012
#KPSS Level = 0.081585, Truncation lag parameter = 3, p-value = 0.1

fcast_auto_arima <- forecast(autoarima, h = 12)
names(fcast_auto_arima)
colnames(Validate)


MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$SO2_Median)
MAPE_auto_arima
#2004-1015 oct
# ME     RMSE      MAE       MPE     MAPE
# Test set 0.4030046 5.513002 4.703291 -6.657129 27.16234

##2004-14
# ME     RMSE      MAE      MPE     MAPE
# Test set 2.740473 5.143293 3.722563 9.778986 16.06382
#2010-2012
# ME     RMSE      MAE       MPE     MAPE
# Test set 0.4232739 4.601068 3.623112 -2.074387 17.26826

##only chnage here
#2010 onwards
# ME     RMSE     MAE      MPE     MAPE
# Test set -0.003772365 7.194892 6.15598 -13.3882 34.74388
#2012 onwards
# ME     RMSE     MAE       MPE     MAPE
# Test set 0.205653 7.183896 6.15097 -12.16072 34.33364


res<-as.data.frame(cbind(pred=fcast_auto_arima$mean,actual=Validate$SO2_Median))
View(res)
####################################
?stl
##seasonal model
Stl = stl(ts_train,s.window='periodic')

## 2012 onwards
Stl = stl(ts_train,s.window=1,t.window=1,
          l.window=4,s.jump=2,t.jump=4,
          l.jump=5)
#2010-14
Stl = stl(ts_train,s.window=1,t.window=4,
          l.window=1,s.jump=3,t.jump=4,
          l.jump=1)

##2004-1014 14076
options[2141,]
# i j k l m n
# 2141 1 4 1 3 4 1

Stl = stl(ts_train,s.window=1,t.window=4,
          l.window=1,s.jump=3,t.jump=4,
          l.jump=1)


#####
#2004-2014 oct
Stl = stl(ts_train,s.window=1,t.window=9,
          l.window=5,s.jump=1,t.jump=13,
          l.jump=17)
# i j k l  m  n
# 35041 1 9 5 1 13 17
#i j k l  m  n
#5905 1 3 4 3 13 27
Stl = stl(ts_train,s.window=1,t.window=3,
          l.window=4,s.jump=3,t.jump=13,
          l.jump=27)

##2004## 34842	14.04544
options[34842,]
# i j k l m n o p q
# 34842 9 1 9 1 9 9 1 1 1

Stl = stl(ts_train,s.window=9,t.window=1,
          l.window=9,s.jump=1,t.jump=9,
          l.jump=9, robust =TRUE,s.degree=1,t.degree=1,l.degree=1)

fcast_auto_arima <- forecast(Stl, h = 12,method ='ets')
fcast_auto_arima_ets<-fcast_auto_arima
fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
fcast_auto_arima_arima<-fcast_auto_arima
fcast_auto_arima <- forecast(Stl, h = 12,method ='naive',allow.multiplicative.trend = TRUE)
fcast_auto_arima_Naive<-fcast_auto_arima
View(cbind(ets=fcast_auto_arima_ets$mean,arima=fcast_auto_arima_arima$mean,
           naive=fcast_auto_arima_Naive$mean,actual=Validate$SO2_Median))

#Stl = stl(ts_train,s.window=1)
#Stl=stl(ts_train,s.window=10,t.window=100,l.window=106,t.jump=80,l.jump=80)
Stl
plot(Stl)
names(Stl)


fcast_auto_arima <- forecast(Stl, h = 12,method ='ets')
fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$SO2_Median)
MAPE_auto_arima

#2004-2015 oct arima
# i j k l  m  n
# 35041 1 9 5 1 13 17
#ME     RMSE      MAE       MPE     MAPE
#Test set 0.1863524 3.951444 2.834949 -4.337811 16.51946

View(cbind(fcast_auto_arima$mean,Validate$SO2_Median))
max(Validate$SO2_Median)#29
View(cbind(fcast_auto_arima$mean,fcast_auto_arima$mean*1.1,Validate$SO2_Median))

pred_data=as.data.frame(cbind(month_number=Validate$month_number,
                              predicted=fcast_auto_arima$mean,actual=Validate$SO2_Median))

pred_data[which(pred_data$month_number %in% c(9,11)), 'predicted']=pred_data[which(pred_data$month_number %in% c(9,11)), 'predicted']=pred_data[which(pred_data$month_number %in% c(9,11)), 'predicted']*1.35


pred_data[which(pred_data$month_number %in% c(9,11)), 'predicted']*1.4

ggplot(pred_data, aes(x=month_number)) + 
  geom_line(aes(y = predicted), color = "darkred") + 
  geom_line(aes(y = actual), color="steelblue", linetype="twodash") 
axis(side=1, at=1:12)

#####model wil ariam teh remaide of stl
names(Stl)
Stl$time.series[,'remainder']

localpred <- Stl$time.series[,'remainder']
armafit_local <- auto.arima(localpred,stepwise=FALSE, approximation=FALSE)
armafit_local
global_pred_out <-fcast_auto_arima$mean
auto_arima_pred_out <- predict(armafit_local, n.ahead = 12)
global_pred_out
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
global_pred_out
MAPE_sales <- accuracy(global_pred_out,Validate$SO2_Median)
MAPE_sales
View(cbind(global_pred_out,Validate$SO2_Median))
####################################################

fcast_auto_arima <- forecast(Stl, h = 180,method ='ets')
fcast_auto_arima <- forecast(Stl, h = 16,method ='arima')
class(fcast_auto_arima$mean)
names(fcast_auto_arima$mean)
fcast_auto_arima$mean
var<- as.data.frame(fcast_auto_arima$mean)
var[5:64,]
plot(cbind(1:length(fcast_auto_arima$mean)-4,fcast_auto_arima$mean))
plot(cbind(1:60,var[5:64,]))
var[5:64,]
max(fcast_auto_arima$mean)#22.49054
data_120<-as.data.frame(cbind(month=1:length(fcast_auto_arima$mean),so2=fcast_auto_arima$mean))

data_120<-as.data.frame(cbind(month=1:12,SO2=var[5:16,]))

g<-ggplot(data_120,aes(x=month,y=SO2, colour=SO2))+geom_line()+ 
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  labs(title="Pune SO2 Seasonality",
       x ="Months", y = "SO2 level", size=30)+
  theme(axis.text.x=element_text(size=rel(2)),axis.text.y=element_text(size=rel(2)),
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
g
ggsave('E:/MS/Timeseries/Images/Pune_SO2_12.png', plot = g,width = 10, height = 7.5)




View(data_120)
ggplot(data_120,aes(x=month,y=SO2, colour=SO2))+geom_line()+ 
  scale_x_continuous(breaks = seq(6, 60, by = 6))

##2004-14 ets 1 4 1 3 4 1
# ME     RMSE      MAE    MPE     MAPE
# Test set 1.895776 5.171365 3.488658 6.0798 14.63144
##2004-14 arima 1 4 1 3 4 1
#ME     RMSE      MAE      MPE     MAPE
#Test set 1.582687 5.085389 3.504314 4.487595 14.91553
##2010-14 ets
#ME     RMSE     MAE      MPE     MAPE
#Test set 2.143473 5.246961 3.43887 7.298187 14.23593
##2010-14 arima
#ME     RMSE      MAE      MPE     MAPE
#Test set 2.143491 5.246968 3.438876 7.298277 14.23596

##2012 onwards ets
## 1 1 4 2 4 5
# ME    RMSE      MAE       MPE     MAPE
# Test set 0.1934294 5.86389 4.459457 -8.583154 24.06066
##2012 onwards arima
## 1 1 4 2 4 5
#ME    RMSE      MAE       MPE     MAPE
#Test set 0.2329686 5.88161 4.495577 -8.424636 24.20177


##2012 onwards ets
# ME   RMSE      MAE       MPE     MAPE
# Test set -1.881555 7.2842 5.869161 -22.50189 35.19376
##2012 onwards arima
# ME    RMSE      MAE      MPE     MAPE
# Test set -0.1093344 7.01549 5.324016 -12.3187 29.53767
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
#1 9 5 1 13 17
options <- expand.grid(i=seq(1,3), j=seq(3,30,5),k=seq(1,5),l=seq(1,3),m=seq(1,21,4),n=seq(15,40,4))
options <- expand.grid(i=seq(7,13,2), j=seq(1,9,2),k=seq(1,12,4),l=seq(1,7,2),m=seq(1,30,8),
                       n=seq(1,40,8),o=seq(0,1,1),p=seq(0,1,1),q=seq(0,1,1))
options
View(options)
STL.window<-function(x) {
  Stl = stl(ts_train,s.window=options[x,]$i,t.window=options[x,]$j,
            l.window=options[x,]$k,s.jump=options[x,]$l,t.jump=options[x,]$m,
            l.jump=options[x,]$n,robust =TRUE,
            s.degree=options[x,]$o,t.degree=options[x,]$p,l.degree=options[x,]$q)
  fcast_auto_arima <- forecast(Stl, h = 12,method ='arima')
  MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,Validate$SO2_Median)
  rbind(x,mape=MAPE_auto_arima[1,5])
}
STL.window.results=sapply(1:nrow(options),STL.window)
View(t(STL.window.results))
##with degree
##2004##34842	14.04544
##2004## 34842	14.04544
options[34842,]
##201-2015 oct
#5041	16.51946
options[35041,]
# i j k l  m  n
# 35041 1 9 5 1 13 17
##5905 16.62699
options[5905,]
#i j k l  m  n
#5905 1 3 4 3 13 27


#2012 onwars,   
# i j k l m n
# 14576 1 1 4 2 4 5
##2010-14
#    i j k l m n
#2141 1 4 1 3 4 1
options[2141,]
##2004-1014 14076
options[2141,]
# i j k l m n
# 2141 1 4 1 3 4 1

Stl = stl(ts_train,s.window=1,t.window=4,
          l.window=1,s.jump=3,t.jump=4,
          l.jump=1)



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

########################################
##tbats
tbats_model<-tbats(ts_train)
plot(forecast(tbats_model))
fc2 <- forecast(tbats_model, h=12)
fc2

###### arima fwith fourier terms
bestfit <- list(aicc=Inf)
for(i in 1:25){
  fit <- auto.arima(ts_train, xreg=fourier(ts_train, K=i), seasonal=TRUE)
  if(fit$aicc < bestfit$aicc){
    bestfit <- fit
    print(i)}
  else break;
}
bestfit #ARIMA(1,1,1) errors 
fc <- forecast(bestfit, xreg=fourier(ts_train, K=1, h=12))
plot(fc)
summary(fc)

####

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




# we will use the width 3 as teh curve pattern  has a single chnage in curve till then, 
smoothedseries <- stats::filter(ts_train, filter=rep(1/3, 3),
                                method="convolution", sides=2)

sum(is.na(smoothedseries)) ##3

smoothedseries
length(smoothedseries)#48

#first  and last are na, since we don't have the range, 
#we will use the last steps as differnce to fill in these values
#intial 1 values
smoothedseries[1]<-smoothedseries[2]-(smoothedseries[3]-smoothedseries[2])
##for 18
#diff=smoothedseries[10]-smoothedseries[9]
#for (i in 8:1){smoothedseries[i]=smoothedseries[i+1]-diff}

#last  values
#onwards 2004 onwards
length(smoothedseries)#36
smoothedseries[36]<-smoothedseries[35]+(smoothedseries[35]-smoothedseries[34])
#smoothedseries[132]<-smoothedseries[131]+(smoothedseries[131]-smoothedseries[130])
# ##2010 onwards
# #onwards 2004 onwards
# length(smoothedseries)#60
# smoothedseries[59]<-smoothedseries[58]+(smoothedseries[58]-smoothedseries[57])
# smoothedseries[60]<-smoothedseries[59]+(smoothedseries[59]-smoothedseries[58])
# 
# 
# 
# ##18
# #diff=smoothedseries[150]-smoothedseries[149]
# #for (i in 151:159){smoothedseries[i]=smoothedseries[i-1]+diff}

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

##no soothing
smoothedseries<-cbind.data.frame(date_order=1:length(ts_train),
                                 Total_sales=as.vector(ts_train))
timevals<-c(1:length(smoothedseries))
timevals
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
options <- expand.grid(i=1:3, j=seq(.1,1,by=.05))
options
#running regression for additive model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_additive,timevals)
plot(sum_of_sq_err)
axis(side=1, at=1:57)
#18, 30
head(options[order(sum_of_sq_err),],20)
##2004-14 no smmothing


##2012 onwards
# i    j
# 30 3 0.55
# 18 3 0.35
# 29 2 0.55
# 21 3 0.40
# 17 2 0.35
# 28 1 0.55

##lets look at teh actual prediction
lmfit <- lm(Total_sales ~ poly(date_order,3)+sin(.7*date_order) + cos(.3*date_order), data=smoothedseries)
summary(lmfit)
##2012 onwards
# Residual standard error: 1.995 on 31 degrees of freedom
# Multiple R-squared:  0.5427,	Adjusted R-squared:  0.4837 
# F-statistic: 9.196 on 4 and 31 DF,  p-value: 5.097e-05
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
##2012 onwards
##all good
acf(localpred,lag.max =11, type="partial")
##9

## let's try and fit a series to it using auto.arima
armafit_local <- auto.arima(localpred)
armafit_local
##2012 onwards
# Series: localpred 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 estimated as 8.916:  log likelihood=-90.46
# AIC=182.93   AICc=183.04   BIC=184.51

#calculation residual
resi <- localpred-fitted(armafit_local)
# Compute and plot the ACF for the time series
acf(resi,lag.max =11)
#seems fine now##4 month smothed

acf(resi, type="partial",lag.max =11)
##no improvement

adf.test(resi, alternative="stationary")
# Augmented Dickey-Fuller Test
# 
# data:  resi
# Dickey-Fuller = -3.9731, Lag order = 3, p-value = 0.02187
# alternative hypothesis: stationary
##stationary hypothesis cna be rejected
kpss.test(resi)
# KPSS Level = 0.067009, Truncation lag parameter = 3, p-value = 0.1
##stationaty null hypothesis can;t be rejected
########################################################
#Prediction and Accuracy mesurements
#########################################################
#lets predict and see the mape value


global_pred_out <- predict(lmfit,data.frame(date_order = 121:132))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 12)
global_pred_out
auto_arima_pred_out
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
global_pred_out
MAPE_sales <- accuracy(global_pred_out,Validate$SO2_Median)
MAPE_sales
#2012
##Test set -2.563706 8.517395 7.395254 -29.35938 45.91225


View(cbind(global_pred_out,Validate$SO2_Median))
################################################################################################
################################################################################################
##########$$$$$$$$$$$$$$$ running regression for multiplicative model $$$$$$$$$$$$$###########
################################################################################################
################################################################################################
#running regression for multiplicative model
sum_of_sq_err<-sapply(1:nrow(options),get_residual_multiplicative,timevals)
plot(sum_of_sq_err)
axis(side=1, at=1:57)
head(options[order(sum_of_sq_err),],20)
# ####2 month smothing
# i    j
# 51 3 0.90
# 18 3 0.35
# 21 3 0.40
# 45 3 0.80
# 12 3 0.25
# 30 3 0.55
# 24 3 0.45
# 36 3 0.65
# 27 3 0.50
# 39 3 0.70
# 6  3 0.15
# 33 3 0.60
# 17 2 0.35
# 23 2 0.45
# 20 2 0.40
# 29 2 0.55
# 26 2 0.50
# 15 3 0.30
# 11 2 0.25
# 28 1 0.55
sum_of_sq_err[order(sum_of_sq_err)]## any fom top 4 are fine## choosing 4
sum_of_sq_err[27]
options[27,]

###lets tets out itsprediction capabilities
lmfit <- lm(Total_sales ~ poly(date_order,3)*sin(.25*date_order)
            +   poly(date_order,3)*cos((.75)*date_order)
            , data=smoothedseries)
summary(lmfit)

## smmothes 3 months
##3 degree , .35
# Residual standard error: 1.503 on 27 degrees of freedom
# Multiple R-squared:  0.7741,	Adjusted R-squared:  0.7072 
# F-statistic: 11.57 on 8 and 27 DF,  p-value: 5.763e-07

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
### all good


# Compute and plot the ACF for the time series
acf(localpred,lag.max =11, type="partial")
#9 pokes out

armafit_local <- auto.arima(localpred)
armafit_local
##4 weke smothing 
#degree 2 , .35
# Series: localpred 
#ARIMA(0,0,0) with zero mean 
#sigma^2 estimated as 6.922:  log likelihood=-85.91
#AIC=173.81   AICc=173.93   BIC=175.4

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

global_pred_out <- predict(lmfit,data.frame(date_order = 121:132))
auto_arima_pred_out <- predict(armafit_local, n.ahead = 12)
global_pred_out<-global_pred_out+auto_arima_pred_out$pred
MAPE_sales <- accuracy(global_pred_out,Validate$SO2_Median)
MAPE_sales
## smotheed 4  months curve
#degree 2 3.65
# ME     RMSE      MAE       MPE    MAPE
#Test set -0.6197719 23.88332 18.79832 -44.04844 104.179
plot(ts_validate, col="black")
lines(global_pred_out, col="red")##ORIGINAL NOT IN VIEW
plot(global_pred_out)
View(cbind(Validate$NO2_Median,global_pred_out))

res<-as.data.frame(cbind(actual=Validate$SO2_Median,base_pred=global_pred_out))

res$base_20<-res$base_pred*1.2
View(res)

