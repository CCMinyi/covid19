library(ggplot2)
library(showtext)
library(lubridate)
taiwan_data <- read.csv('overall_covid.csv')
date <- seq(as.Date("2022/03/01"), as.Date("2022/09/10"), "day")
date <- data.frame(個案研判日=date)
taiwan_data$個案研判日<- ymd(taiwan_data$個案研判日)
index_len <- nrow(date)
taiwan_data$index <- 1:index_len


desired_breaks <- c(ymd("2022-03-01"),ymd("2022-03-25"), ymd("2022-05-27"), ymd("2022-08-26"),ymd("2022-09-10"))

ggplot(taiwan_data, aes(x=個案研判日)) + 
  geom_line(aes(x = 個案研判日,y = 新增確診人數, colour = "var1"))+
  geom_line(aes(x = 個案研判日,y = 新增確診人數, colour = "var1"))+
  ylab('General Scale')+xlab('Date')+ guides(colour = FALSE)+
  scale_x_date(date_labels = "%b %d",breaks = desired_breaks)+
  geom_point(data=subset(taiwan_data , 個案研判日 == ymd("2022-03-25")), aes(y=新增確診人數),color="black", size=4)+
  geom_point(data=subset(taiwan_data , 個案研判日 == ymd("2022-05-27")), aes(y=新增確診人數),color="black", size=4)+
  geom_point(data=subset(taiwan_data , 個案研判日 == ymd("2022-08-26")), aes(y=新增確診人數),color="black", size=4)+
  geom_vline(xintercept = ymd("2022-03-25") ,linetype = 1)+
  geom_vline(xintercept = ymd("2022-05-27") ,linetype = 1)+
  geom_vline(xintercept = ymd("2022-08-26") ,linetype = 1)