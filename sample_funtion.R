QuartileNHist <- function(data) 
{
  print(quantile(unlist(data),seq(0, 1, 0.01),na.rm=T))
  print(ggplot(data.frame(data),aes(x=data))+geom_histogram() + 
          scale_x_continuous(breaks = round(seq(min(data,na.rm=T), max(data,na.rm=T),
                                                by = (max(data,na.rm=T)-min(data,na.rm=T))/30 ),1))+
          theme(axis.text=element_text(size=8)))
  
}

QuartileNHist(merged_df$No.of.dependents)


s<-'tets for split'
st<-as.data.frame(strsplit(s,' '))
names(st)<-'col_name'
st
substring(s,6,8)
library(stringr)
str_extract(s, "for")
t<-as.data.frame(str_locate(s,'for'))
class(t)
t$start
