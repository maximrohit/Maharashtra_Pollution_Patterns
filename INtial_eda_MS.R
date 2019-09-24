#####Data source
setwd("D:/MS")
files = list.files(pattern="*.csv")
View(files)
Base_air_poll <- do.call(rbind, lapply(files, data.table::fread))
Base_air_poll <- do.call(rbindlist, lapply(files, data.table::fread), use.names=T)

nrow(Base_air_poll)#2004 57529 #####1987 60420
ncol(Base_air_poll)#9  ##8 monitoring location removed

View(Base_air_poll)
unique(Base_air_poll$`City/Town/Village/Area`)
# [1] "Pune"            "Bombay"          "Nagpur"          "Chandarpur"      "Aurangabad"      "Chandrapur"      "Nashik"         
# [8] "Dombivli"        "Solapur"         "Thane"           "Mumbai"          "Aurangabad (MS)" "Greater Mumbai"  "Kolhapur"       
# [15] "Amravati"        "Lote"            "Navi Mumbai"     "Tarapur"         "Mahad"           "Badlapur"        "Jalgaon"        
# [22] "Latur"           "Roha"            "Sangli"          "Ulhasnagar"      "Akola"           "Jalna"           "Nanded"   
# ?do.call
# 
# ncolumns <- sapply(files, function(f) {
#   header.line <- readLines(f, n=1)
#   unique(strsplit(header.line, ",")[[1]])
#   
# })
# ncolumns
# ?rbind
###########################
## Date
## Processing
####################
names(Base_air_poll)
##taking backup
Base_air_poll$Sampling_Date<-Base_air_poll$`Sampling Date`
##basic chnage -- %d-%m-%Y format
Base_air_poll$Sampling_Date_processed<-as.Date(Base_air_poll$Sampling_Date,'%d-%m-%Y')
sum(is.na(Base_air_poll$Sampling_Date_processed))##2597
## Other format '%d/%m/%Y'
View(Base_air_poll[which(is.na(Base_air_poll$Sampling_Date_processed)==1),])
##changing dtae ro 30 june  as 31 june done;t exists
Base_air_poll[which(Base_air_poll$Sampling_Date=='31/6/2014'),'Sampling_Date_processed']<-as.Date('30/6/2014',format='%d/%m/%Y')
##remaing values
sum(is.na(Base_air_poll$Sampling_Date_processed))##2595
t<-Base_air_poll[which(is.na(Base_air_poll$Sampling_Date_processed)==1),c('Sampling_Date')]
t$month<-trim(t(as.data.frame(strsplit(t$Sampling_Date,'-')))[,1])
str(t)
###month to number
t$month_number<-match(tolower(substr(t$month,1,3)), tolower(month.abb))
View(t)

###year
t$year<-trim(t(as.data.frame(strsplit(t$Sampling_Date,'-')))[,2])

t$year_number<-substr(t$year,nchar(t$year)-3,nchar(t$year))
View(t)
sapply(t,function(x) sum(is.na(x)))
#Sampling_Date         month  month_number          year   year_number 
#          0             0            56             0             0 

t[which(is.na(t$month_number)==1),]

##settoing annual_1995 to 13 represneting the annual number
t[which(Sampling_Date=='Annual_1995'),'month_number']=13
## setting annual month to 13 as well
t[which(month=='Annual'),'month_number']=13
#####
t[which(is.na(month_number)==1),]

t[which(is.na(month_number)==1),'month_number']<-sapply(t[which(is.na(month_number)==1),'Sampling_Date'], function(x) substr(x,2,3))
sapply(t,function(x) sum(is.na(x)))

###merging the not formed dates values
Base_air_poll_processed<-merge(x=Base_air_poll,y=t[,c("month_number","year_number","Sampling_Date" )],by = 'Sampling_Date',all.x = T)
nrow(Base_air_poll_processed)

####getitng remaining pot values
names(Base_air_poll_processed)
# [1] "Sampling_Date"           "Sampling Date"           "State"                   "City/Town/Village/Area"  "Type of Location"       
# [6] "SO2"                     "NO2"                     "RSPM/PM10"               "SPM"                     "Sampling_Date_processed"
# [11] "month_number"            "year_number"    
Base_air_poll_processed[which(is.na(Base_air_poll_processed$month_number)==1),'month_number']<-format(Base_air_poll_processed[which(is.na(Base_air_poll_processed$month_number)==1),'Sampling_Date_processed'],'%m')

Base_air_poll_processed[which(is.na(Base_air_poll_processed$year_number)==1),'year_number']<-format(Base_air_poll_processed[which(is.na(Base_air_poll_processed$year_number)==1),'Sampling_Date_processed'],'%Y')

sapply(Base_air_poll_processed,function(x) sum(is.na(x)))

# Sampling_Date           Sampling Date                   State  City/Town/Village/Area        Type of Location 
# 0                       0                       0                       0                     299 
# SO2                     NO2               RSPM/PM10                     SPM Sampling_Date_processed 
# 2062                    1217                    4981                   38043                    2595 
# month_number             year_number 
# 0                       0 

saveRDS(Base_air_poll_processed, file="D:/MS/processes_date_pollution.rds")
Check_load <- readRDS("D:/MS/processes_date_pollution.rds")
nrow(Check_load)
class(Check_load)
####################################################
library(gdata)


city_list<-c('solapur','Pune','nashik','Aurangabad','Kolhapur','Lote','Tarapur','mahad','Roha','Sangli')

Neigh_city<-Base_air_poll[which(trim(Base_air_poll$`City/Town/Village/Area`) %in% city_list),]
nrow(Base_air_poll)#57529  ## 1987 60420
nrow(Neigh_city)#13241 ## 1987  13643

nrow(Base_air_poll_pune)#4806

###############################
##Neigh_city
Neigh_city$`City/Town/Village/Area`<-as.factor(Neigh_city$`City/Town/Village/Area`)
summary(Neigh_city$`City/Town/Village/Area`)
# Aurangabad   Kolhapur       Lote       Pune       Roha     Sangli    Tarapur 
# 1815       3180        957       4806        254       1637        592 
Base_air_poll_pune$ins_date<-as.Date(Base_air_poll_pune$'Sampling Date','%d-%m-%Y')
t<-Base_air_poll_pune[which(is.na(Base_air_poll_pune$ins_date)==1),'Sampling Date']
t<-sapply(t,function(x) gsub("[^[:alnum:][:blank:]?&/\\-]", "", x))
Base_air_poll_pune[which(is.na(Base_air_poll_pune$ins_date)==1),'ins_date']<-as.Date(t,'%d/%m/%Y')
unique(as.Date(t,'%d/%m/%Y'))


##filtering for pune
str(Base_air_poll)
Base_air_poll_pune<-Base_air_poll[which(toupper(trim(Base_air_poll$`City/Town/Village/Area`))=='PUNE'),]

##setting location inside pune
nrow(Base_air_poll_pune)#4806
str(Base_air_poll_pune)
Base_air_poll_pune$Location<-as.factor(Base_air_poll_pune$`Location of Monitoring Station`)
summary(Base_air_poll_pune$Location)
Base_air_poll_pune[which(Base_air_poll_pune$Location=='Bhosari, Pune'),'Location']='Bhosari'
Base_air_poll_pune[which(Base_air_poll_pune$Location=='Nal Stop, Pune'),'Location']='Nalstop'
Base_air_poll_pune[which(Base_air_poll_pune$Location=='Swargate, Pune'),'Location']='Swargate'
summary(Base_air_poll_pune$Location)
summary(droplevels(Base_air_poll_pune$Location))
Base_air_poll_pune$Location<-droplevels(Base_air_poll_pune$Location)

## 1100 records over 
1100/12#91 records per year
365/81# 1 record in per 4 days
names(Base_air_poll_pune)

###date colum 
##
library(lubridate)
sum(is.na(Base_air_poll_pune$'Sampling Date'))
sum(is.na(as.Date(Base_air_poll_pune$'Sampling Date','%d-%m-%Y')))

unique(year(as.Date(Base_air_poll_pune$'Sampling Date','%d-%m-%Y')))
# 2004 2005   NA 2007 2010 2011 2012 2013 2014 2015
#6, 8,9

Base_air_poll_pune$Year<-year(as.Date(Base_air_poll_pune$'Sampling Date','%d-%m-%Y'))
class(Base_air_poll_pune$'Sampling Date')
Base_air_poll_pune[which(is.na(Base_air_poll_pune$Year)==0),'Sampling Date']
t<-Base_air_poll_pune[which(is.na(Base_air_poll_pune$Year)==1),'Sampling Date']
t<-sapply(t,function(x) gsub("[^[:alnum:][:blank:]?&/\\-]", "", x))
# View(t)
# 1/4/2006
# 
# t<-trim(as.character(t))
# class(t)
# as.Date("3/5/2009", "%m/%d/%Y")
Base_air_poll_pune[which(is.na(Base_air_poll_pune$Year)==1),'Year']<-year(as.Date(t,'%d/%m/%Y'))
unique(Base_air_poll_pune$Year)
Base_air_poll_pune$Year<-as.factor(Base_air_poll_pune$Year)
summary(Base_air_poll_pune$Year)


Base_air_poll_pune$ins_date<-as.Date(Base_air_poll_pune$'Sampling Date','%d-%m-%Y')
t<-Base_air_poll_pune[which(is.na(Base_air_poll_pune$ins_date)==1),'Sampling Date']
t<-sapply(t,function(x) gsub("[^[:alnum:][:blank:]?&/\\-]", "", x))
Base_air_poll_pune[which(is.na(Base_air_poll_pune$ins_date)==1),'ins_date']<-as.Date(t,'%d/%m/%Y')
unique(as.Date(t,'%d/%m/%Y'))

Base_air_poll_pune$month<-month(Base_air_poll_pune$ins_date)
Base_air_poll_pune$month<-as.factor(Base_air_poll_pune$month)
summary(Base_air_poll_pune$month)

nrow(Base_air_poll_pune)#4806
summary(Base_air_poll_pune$Year)#12
4806/12

50000/400; 125
