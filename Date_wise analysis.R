Base_poll_date <- readRDS("D:/MS/processes_date_pollution.rds")

#write.csv(Base_poll_date,file='D:/personal/MS/Base_poll_date.csv')
names(Base_poll_date)
# [1] "Sampling_Date"           "Sampling Date"           "State"                   "City/Town/Village/Area"  "Type of Location"       
# [6] "SO2"                     "NO2"                     "RSPM/PM10"               "SPM"                     "Sampling_Date_processed"
# [11] "month_number"            "year_number"  
sapply(Base_poll_date,function(x) sum(is.na(x)))
Base_poll_date$month_number<-as.factor(Base_poll_date$month_number)
Base_poll_date$year_number<-as.factor(Base_poll_date$year_number)

#yearwise
summary(Base_poll_date$year_number)

sum(as.numeric(as.character(Base_poll_date$year_number))>=2004)#57529
sum(as.numeric(as.character(Base_poll_date$year_number))<=2003)#2891

# 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 
# 9   24   20  154  148   52  122  144  156  201  237  252  286  279  254  290  263 1739 2161 3090 4280 4040 3837 4954 6356 6998 6655 
# 2014 2015 
# 6425 6994 
##2004 has higher numbers

summary(Base_poll_date$month_number)
# 1    2    3    4    5    6    7    8    9   10   11   12   13 
# 5007 4708 5117 4912 5135 4924 4984 4897 5050 5179 5142 5329   36 

###city wise
names(Base_poll_date)[4]<-'city'
Base_poll_date$city<-as.factor(Base_poll_date$city)
summary(Base_poll_date$city)
##changing Bombay to Mumbai
Base_poll_date[which(Base_poll_date$city=='Bombay'),'city']<-'Mumbai'
summary(Base_poll_date$city)
##merging Aurangabad and Aurangabad (MS)
Base_poll_date[which(Base_poll_date$city=='Aurangabad (MS)'),'city']<-'Aurangabad'
summary(Base_poll_date$city)
##merging Chandarpur with Chandrapur
Base_poll_date[which(Base_poll_date$city=='Chandarpur'),'city']<-'Chandrapur'
summary(Base_poll_date$city)
##dropping levels:-
Base_poll_date<-drop.levels(Base_poll_date)

###struding city wise spread
city_wise_division<-as.data.frame(summary(Base_poll_date$city))
city_wise_division$city<-rownames(city_wise_division)
names(city_wise_division)[1]<-'CountOfRec'
city_wise_division$record_ratio<-city_wise_division$CountOfRec/sum(city_wise_division$CountOfRec)
city_wise_division$record_ratio<-round(city_wise_division$record_ratio*100,2)
View(city_wise_division)
city_wise_division[order(-city_wise_division$record_ratio),]
# CountOfRec           city record_ratio
# Nagpur               7841         Nagpur        12.98
# Navi Mumbai          5541    Navi Mumbai         9.17
# Pune                 5184           Pune         8.58
# Nashik               5148         Nashik         8.52
# Chandrapur           4862     Chandrapur         8.05
# Thane                3623          Thane         6.00
# Kolhapur             3180       Kolhapur         5.26
# Aurangabad           3015     Aurangabad         4.99
# Mumbai               2890         Mumbai         4.78
# Solapur              2630        Solapur         4.35
# Amravati             2456       Amravati         4.06
# Jalgaon              1853        Jalgaon         3.07
# Latur                1776          Latur         2.94
# Sangli               1637         Sangli         2.71
# Nanded               1432         Nanded         2.37
# Akola                1048          Akola         1.73
# Lote                  957           Lote         1.58
# Ulhasnagar            950     Ulhasnagar         1.57
# Jalna                 905          Jalna         1.50
# Dombivli              829       Dombivli         1.37
# Greater Mumbai        689 Greater Mumbai         1.14
# Tarapur               592        Tarapur         0.98
# Badlapur              585       Badlapur         0.97
# Mahad                 543          Mahad         0.90
# Roha                  254           Roha         0.42



#####correcteding columan dn avlues
names(Base_poll_date)[8]<-'RSPMPM10'
View(head(Base_poll_date,10))

Base_poll_date$`Type of Location`<-as.factor(Base_poll_date$`Type of Location`)
summary(Base_poll_date$`Type of Location`)
# Industrial                    Industrial Area                   Industrial Areas 
# 4                              13578                               6055 
# Residential             Residential and others Residential, Rural and other Areas 
# 3                              10961                              28526 
# Sensitive Area                    Sensitive Areas                               NA's 
# 602                                392                                299 

Base_poll_date$`Type of Location`<-as.character(Base_poll_date$`Type of Location`)

######
Base_poll_date[which(Base_poll_date$`Type of Location`=='Industrial Area'),c('Type of Location')]<-'Industrial'
Base_poll_date[which(Base_poll_date$`Type of Location`=='Industrial Areas'),c('Type of Location')]<-'Industrial'
Base_poll_date[which(Base_poll_date$`Type of Location`=='Sensitive Areas'),c('Type of Location')]<-'Sensitive Area'
Base_poll_date[which(Base_poll_date$`Type of Location`=='Residential and others'),c('Type of Location')]<-'Residential'
Base_poll_date[which(Base_poll_date$`Type of Location`=='Residential, Rural and other Areas'),c('Type of Location')]<-'Residential'

Base_poll_date$`Type of Location`<-as.factor(Base_poll_date$`Type of Location`)
summary(Base_poll_date$`Type of Location`)
# Industrial    Residential Sensitive Area           NA's 
#          19637          39490            994            299 

###############################
##Lets validate year and city wise what is our data spread
###############################


City_year_wise_spread<-Base_poll_date%>%
group_by(city,year_number,`Type of Location`)%>%
summarise(countOfRecords=n(),SO2mean=mean(SO2,na.rm = T),NO2mean=mean(NO2,na.rm = T),RSPMPM10mean=mean('RSPMPM10',na.rm = T),SPMmean=mean(SPM))
City_year_wise_spread
#names(City_year_wise_spread)
#plot(City_year_wise_spread)
ggplot(City_year_wise_spread,aes(x=year_number,y=countOfRecords,col=city, size=-countOfRecords))+geom_point()
## number of counts are much higher post 2014

#library(ggrepel)
ggplot(City_year_wise_spread, aes(x=year_number,y=countOfRecords,col=`Type of Location`,size=-SO2mean,label =city ))+
  geom_point()+geom_label_repel(size=3,nudge_y = .03,nudge_x = .03,hjust=-.3, vjust=-.3)+ theme(legend.position = "none")
##lets explore city wise
unique(City_year_wise_spread$city)
# [1] Akola          Amravati       Aurangabad     Badlapur       Chandrapur     Dombivli       Greater Mumbai Jalgaon       
# [9] Jalna          Kolhapur       Latur          Lote           Mahad          Mumbai         Nagpur         Nanded        
# [17] Nashik         Navi Mumbai    Pune           Roha           Sangli         Solapur        Tarapur        Thane         
# [25] Ulhasnagar    
##
ggplot(City_year_wise_spread[which(City_year_wise_spread$city==c('Pune','Mumbai','Nagpur','Nashik')),], 
       aes(x=year_number,y=SO2mean,col=`Type of Location`,size=NO2mean,label =city ))+
  geom_point()+geom_label_repel(size=3,nudge_y = .03,nudge_x = .03,hjust=-.3, vjust=-.3)



