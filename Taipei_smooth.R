data_taipei <- read.csv('taipei_district.csv')

date <- seq(as.Date("2022/03/25"), as.Date("2022/08/26"), "day")
date <- data.frame(個案研判日=date)
index_len <- nrow(date)
data_taipei$個案研判日<- as.Date(data_taipei$個案研判日, format= "%Y-%m-%d")
data_taipei <- data_taipei[(data_taipei$個案研判日>= "2022-03-25"),]
data_taipei <- data_taipei[order(as.Date(data_taipei$個案研判日, format="%Y-%m-%d")),]



data_taipei_2 <- subset(data_taipei,鄉鎮 == '中正區')
data_taipei_2 <- merge(date,data_taipei_2,by = '個案研判日', all = T)
data_taipei_2$index <- 1:index_len
data_taipei_2$鄉鎮[is.na(data_taipei_2$鄉鎮)] <- '中正區'
data_taipei_2[is.na(data_taipei_2)] <- 0



data_taipei_5 <- subset(data_taipei,鄉鎮 == '北投區')
data_taipei_5 <- merge(date,data_taipei_5,by = '個案研判日', all = T)
data_taipei_5$index <- 1:index_len
data_taipei_5$鄉鎮[is.na(data_taipei_5$鄉鎮)] <- '北投區'
data_taipei_5[is.na(data_taipei_5)] <- 0


data_taipei_6 <- subset(data_taipei,鄉鎮 == '南港區')
data_taipei_6 <- merge(date,data_taipei_6,by = '個案研判日', all = T)
data_taipei_6$index <- 1:index_len
data_taipei_6$鄉鎮[is.na(data_taipei_6$鄉鎮)] <- '南港區'
data_taipei_6[is.na(data_taipei_6)] <- 0


data_taipei_7 <- subset(data_taipei,鄉鎮 == '士林區')
data_taipei_7 <- merge(date,data_taipei_7,by = '個案研判日', all = T)
data_taipei_7$index <- 1:index_len
data_taipei_7$鄉鎮[is.na(data_taipei_7$鄉鎮)] <- '士林區'
data_taipei_7[is.na(data_taipei_7)] <- 0


data_taipei_12 <- subset(data_taipei,鄉鎮 == '萬華區')
data_taipei_12 <- merge(date,data_taipei_12,by = '個案研判日', all = T)
data_taipei_12$index <- 1:index_len
data_taipei_12$鄉鎮[is.na(data_taipei_12$鄉鎮)] <- '萬華區'
data_taipei_12[is.na(data_taipei_12)] <- 0


#smooth
seven_days_smooth <- function(data){
  bind_data <-  rbind(head(data,n=1),head(data,n=1),head(data,n=1),data,tail(data,n=1),tail(data,n=1),tail(data,n=1))
  smooth_result <-  c()
  len <- length(bind_data$當日確診率)-6
  for(i in 1:len){
    j = i+6
    #seven_days <- c(bind_data$當日確診率[i:j])
    smooth_value <-  mean(bind_data$當日確診率[i:j])
    smooth_result <- append(smooth_result,smooth_value)
  }
  return(smooth_result)
}
#smooth again
seven_days_smooth_2 <- function(data){
  bind_data <-  rbind(head(data,n=1),head(data,n=1),head(data,n=1),data,tail(data,n=1),tail(data,n=1),tail(data,n=1))
  smooth_result <-  c()
  len <- length(bind_data$smooth)-6
  for(i in 1:len){
    j = i+6
    #seven_days <- c(bind_data$當日確診率[i:j])
    smooth_value <-  mean(bind_data$smooth[i:j])
    smooth_result <- append(smooth_result,smooth_value)
  }
  return(smooth_result)
}

data_taipei_2['smooth'] <- seven_days_smooth(data_taipei_2)
data_taipei_2['smooth_2'] <- seven_days_smooth_2(data_taipei_2)
data_taipei_5['smooth'] <- seven_days_smooth(data_taipei_5)
data_taipei_5['smooth_2'] <- seven_days_smooth_2(data_taipei_5)
data_taipei_6['smooth'] <- seven_days_smooth(data_taipei_6)
data_taipei_6['smooth_2'] <- seven_days_smooth_2(data_taipei_6)
data_taipei_7['smooth'] <- seven_days_smooth(data_taipei_7)
data_taipei_7['smooth_2'] <- seven_days_smooth_2(data_taipei_7)
data_taipei_12['smooth'] <- seven_days_smooth(data_taipei_12)
data_taipei_12['smooth_2'] <- seven_days_smooth_2(data_taipei_12)

taipei_data_2 <- data_taipei_2
taipei_data_5 <- data_taipei_5
taipei_data_6 <- data_taipei_6
taipei_data_7 <- data_taipei_7
taipei_data_12 <- data_taipei_12

taipei_data_2$個案研判日<- ymd(taipei_data_2$個案研判日)
taipei_data_5$個案研判日<- ymd(taipei_data_5$個案研判日)
taipei_data_6$個案研判日<- ymd(taipei_data_6$個案研判日)
taipei_data_7$個案研判日<- ymd(taipei_data_7$個案研判日)
taipei_data_12$個案研判日<- ymd(taipei_data_12$個案研判日)



##############
##台北五區
##############
#zhong zheng

filtered_data_9_2 <- subset(taipei_data_2,smooth_2 > 0.9*max(taipei_data_2$smooth_2))
filtered_data_9_2['horizon'] <- rep(0.9*max(taipei_data_2$smooth_2),nrow(filtered_data_9_2))
filtered_data_8_2 <- subset(taipei_data_2,smooth_2 > 0.8*max(taipei_data_2$smooth_2))
filtered_data_8_2['horizon'] <- rep(0.8*max(taipei_data_2$smooth_2),nrow(filtered_data_8_2))
filtered_data_7_2 <- subset(taipei_data_2,smooth_2 > 0.7*max(taipei_data_2$smooth_2))
filtered_data_7_2['horizon'] <- rep(0.7*max(taipei_data_2$smooth_2),nrow(filtered_data_7_2))
filtered_data_6_2 <- subset(taipei_data_2,smooth_2 > 0.6*max(taipei_data_2$smooth_2))
filtered_data_6_2['horizon'] <- rep(0.6*max(taipei_data_2$smooth_2),nrow(filtered_data_6_2))
filtered_data_5_2 <- subset(taipei_data_2,smooth_2 > 0.5*max(taipei_data_2$smooth_2))
filtered_data_5_2['horizon'] <- rep(0.5*max(taipei_data_2$smooth_2),nrow(filtered_data_5_2))
filtered_data_4_2 <- subset(taipei_data_2,smooth_2 > 0.4*max(taipei_data_2$smooth_2))
filtered_data_4_2['horizon'] <- rep(0.4*max(taipei_data_2$smooth_2),nrow(filtered_data_4_2))
filtered_data_3_2 <- subset(taipei_data_2,smooth_2 > 0.3*max(taipei_data_2$smooth_2))
filtered_data_3_2['horizon'] <- rep(0.3*max(taipei_data_2$smooth_2),nrow(filtered_data_3_2))
filtered_data_2_2 <- subset(taipei_data_2,smooth_2 > 0.2*max(taipei_data_2$smooth_2))
filtered_data_2_2['horizon'] <- rep(0.2*max(taipei_data_2$smooth_2),nrow(filtered_data_2_2))
filtered_data_1_2 <- subset(taipei_data_2,smooth_2 > 0.1*max(taipei_data_2$smooth_2))
filtered_data_1_2['horizon'] <- rep(0.1*max(taipei_data_2$smooth_2),nrow(filtered_data_1_2))
#bei tou

filtered_data_9_5 <- subset(taipei_data_5,smooth_2 > 0.9*max(taipei_data_5$smooth_2))
filtered_data_9_5['horizon'] <- rep(0.9*max(taipei_data_5$smooth_2),nrow(filtered_data_9_5))
filtered_data_8_5 <- subset(taipei_data_5,smooth_2 > 0.8*max(taipei_data_5$smooth_2))
filtered_data_8_5['horizon'] <- rep(0.8*max(taipei_data_5$smooth_2),nrow(filtered_data_8_5))
filtered_data_7_5 <- subset(taipei_data_5,smooth_2 > 0.7*max(taipei_data_5$smooth_2))
filtered_data_7_5['horizon'] <- rep(0.7*max(taipei_data_5$smooth_2),nrow(filtered_data_7_5))
filtered_data_6_5 <- subset(taipei_data_5,smooth_2 > 0.6*max(taipei_data_5$smooth_2))
filtered_data_6_5['horizon'] <- rep(0.6*max(taipei_data_5$smooth_2),nrow(filtered_data_6_5))
filtered_data_5_5 <- subset(taipei_data_5,smooth_2 > 0.5*max(taipei_data_5$smooth_2))
filtered_data_5_5['horizon'] <- rep(0.5*max(taipei_data_5$smooth_2),nrow(filtered_data_5_5))
filtered_data_4_5 <- subset(taipei_data_5,smooth_2 > 0.4*max(taipei_data_5$smooth_2))
filtered_data_4_5['horizon'] <- rep(0.4*max(taipei_data_5$smooth_2),nrow(filtered_data_4_5))
filtered_data_3_5 <- subset(taipei_data_5,smooth_2 > 0.3*max(taipei_data_5$smooth_2))
filtered_data_3_5['horizon'] <- rep(0.3*max(taipei_data_5$smooth_2),nrow(filtered_data_3_5))
filtered_data_2_5 <- subset(taipei_data_5,smooth_2 > 0.2*max(taipei_data_5$smooth_2))
filtered_data_2_5['horizon'] <- rep(0.2*max(taipei_data_5$smooth_2),nrow(filtered_data_2_5))
filtered_data_1_5 <- subset(taipei_data_5,smooth_2 > 0.1*max(taipei_data_5$smooth_2))
filtered_data_1_5['horizon'] <- rep(0.1*max(taipei_data_5$smooth_2),nrow(filtered_data_1_5))
#nan gang

filtered_data_9_6 <- subset(taipei_data_6,smooth_2 > 0.9*max(taipei_data_6$smooth_2))
filtered_data_9_6['horizon'] <- rep(0.9*max(taipei_data_6$smooth_2),nrow(filtered_data_9_6))
filtered_data_8_6 <- subset(taipei_data_6,smooth_2 > 0.8*max(taipei_data_6$smooth_2))
filtered_data_8_6['horizon'] <- rep(0.8*max(taipei_data_6$smooth_2),nrow(filtered_data_8_6))
filtered_data_7_6 <- subset(taipei_data_6,smooth_2 > 0.7*max(taipei_data_6$smooth_2))
filtered_data_7_6['horizon'] <- rep(0.7*max(taipei_data_6$smooth_2),nrow(filtered_data_7_6))
filtered_data_6_6 <- subset(taipei_data_6,smooth_2 > 0.6*max(taipei_data_6$smooth_2))
filtered_data_6_6['horizon'] <- rep(0.6*max(taipei_data_6$smooth_2),nrow(filtered_data_6_6))
filtered_data_5_6 <- subset(taipei_data_6,smooth_2 > 0.5*max(taipei_data_6$smooth_2))
filtered_data_5_6['horizon'] <- rep(0.5*max(taipei_data_6$smooth_2),nrow(filtered_data_5_6))
filtered_data_4_6 <- subset(taipei_data_6,smooth_2 > 0.4*max(taipei_data_6$smooth_2))
filtered_data_4_6['horizon'] <- rep(0.4*max(taipei_data_6$smooth_2),nrow(filtered_data_4_6))
filtered_data_3_6 <- subset(taipei_data_6,smooth_2 > 0.3*max(taipei_data_6$smooth_2))
filtered_data_3_6['horizon'] <- rep(0.3*max(taipei_data_6$smooth_2),nrow(filtered_data_3_6))
filtered_data_2_6 <- subset(taipei_data_6,smooth_2 > 0.2*max(taipei_data_6$smooth_2))
filtered_data_2_6['horizon'] <- rep(0.2*max(taipei_data_6$smooth_2),nrow(filtered_data_2_6))
filtered_data_1_6 <- subset(taipei_data_6,smooth_2 > 0.1*max(taipei_data_6$smooth_2))
filtered_data_1_6['horizon'] <- rep(0.1*max(taipei_data_6$smooth_2),nrow(filtered_data_1_6))

#shi lin

filtered_data_9_7 <- subset(taipei_data_7,smooth_2 > 0.9*max(taipei_data_7$smooth_2))
filtered_data_9_7['horizon'] <- rep(0.9*max(taipei_data_7$smooth_2),nrow(filtered_data_9_7))
filtered_data_8_7 <- subset(taipei_data_7,smooth_2 > 0.8*max(taipei_data_7$smooth_2))
filtered_data_8_7['horizon'] <- rep(0.8*max(taipei_data_7$smooth_2),nrow(filtered_data_8_7))
filtered_data_7_7 <- subset(taipei_data_7,smooth_2 > 0.7*max(taipei_data_7$smooth_2))
filtered_data_7_7['horizon'] <- rep(0.7*max(taipei_data_7$smooth_2),nrow(filtered_data_7_7))
filtered_data_6_7 <- subset(taipei_data_7,smooth_2 > 0.6*max(taipei_data_7$smooth_2))
filtered_data_6_7['horizon'] <- rep(0.6*max(taipei_data_7$smooth_2),nrow(filtered_data_6_7))
filtered_data_5_7 <- subset(taipei_data_7,smooth_2 > 0.5*max(taipei_data_7$smooth_2))
filtered_data_5_7['horizon'] <- rep(0.5*max(taipei_data_7$smooth_2),nrow(filtered_data_5_7))
filtered_data_4_7 <- subset(taipei_data_7,smooth_2 > 0.4*max(taipei_data_7$smooth_2))
filtered_data_4_7['horizon'] <- rep(0.4*max(taipei_data_7$smooth_2),nrow(filtered_data_4_7))
filtered_data_3_7 <- subset(taipei_data_7,smooth_2 > 0.3*max(taipei_data_7$smooth_2))
filtered_data_3_7['horizon'] <- rep(0.3*max(taipei_data_7$smooth_2),nrow(filtered_data_3_7))
filtered_data_2_7 <- subset(taipei_data_7,smooth_2 > 0.2*max(taipei_data_7$smooth_2))
filtered_data_2_7['horizon'] <- rep(0.2*max(taipei_data_7$smooth_2),nrow(filtered_data_2_7))
filtered_data_1_7 <- subset(taipei_data_7,smooth_2 > 0.1*max(taipei_data_7$smooth_2))
filtered_data_1_7['horizon'] <- rep(0.1*max(taipei_data_7$smooth_2),nrow(filtered_data_1_7))

#wan hua

filtered_data_9_12 <- subset(taipei_data_12,smooth_2 > 0.9*max(taipei_data_12$smooth_2))
filtered_data_9_12['horizon'] <- rep(0.9*max(taipei_data_12$smooth_2),nrow(filtered_data_9_12))
filtered_data_8_12 <- subset(taipei_data_12,smooth_2 > 0.8*max(taipei_data_12$smooth_2))
filtered_data_8_12['horizon'] <- rep(0.8*max(taipei_data_12$smooth_2),nrow(filtered_data_8_12))
filtered_data_7_12 <- subset(taipei_data_12,smooth_2 > 0.7*max(taipei_data_12$smooth_2))
filtered_data_7_12['horizon'] <- rep(0.7*max(taipei_data_12$smooth_2),nrow(filtered_data_7_12))
filtered_data_6_12 <- subset(taipei_data_12,smooth_2 > 0.6*max(taipei_data_12$smooth_2))
filtered_data_6_12['horizon'] <- rep(0.6*max(taipei_data_12$smooth_2),nrow(filtered_data_6_12))
filtered_data_5_12 <- subset(taipei_data_12,smooth_2 > 0.5*max(taipei_data_12$smooth_2))
filtered_data_5_12['horizon'] <- rep(0.5*max(taipei_data_12$smooth_2),nrow(filtered_data_5_12))
filtered_data_4_12 <- subset(taipei_data_12,smooth_2 > 0.4*max(taipei_data_12$smooth_2))
filtered_data_4_12['horizon'] <- rep(0.4*max(taipei_data_12$smooth_2),nrow(filtered_data_4_12))
filtered_data_3_12 <- subset(taipei_data_12,smooth_2 > 0.3*max(taipei_data_12$smooth_2))
filtered_data_3_12['horizon'] <- rep(0.3*max(taipei_data_12$smooth_2),nrow(filtered_data_3_12))
filtered_data_2_12 <- subset(taipei_data_12,smooth_2 > 0.2*max(taipei_data_12$smooth_2))
filtered_data_2_12['horizon'] <- rep(0.2*max(taipei_data_12$smooth_2),nrow(filtered_data_2_12))
filtered_data_1_12 <- subset(taipei_data_12,smooth_2 > 0.1*max(taipei_data_12$smooth_2))
filtered_data_1_12['horizon'] <- rep(0.1*max(taipei_data_12$smooth_2),nrow(filtered_data_1_12))

#plot



ggplot(taipei_data_2, aes(個案研判日)) +
  geom_line(aes(y = smooth_2, colour = "Zhongzheng_District"))+ 
  geom_line(data = taipei_data_5,aes(y = smooth_2, colour = "Beitou_District"))+
  geom_line(data = taipei_data_6,aes(y = smooth_2, colour = "Nangang_District"))+
  geom_line(data = taipei_data_7,aes(y = smooth_2, colour = "Shilin_District"))+
  geom_line(data = taipei_data_12,aes(y = smooth_2, colour = "Wanhua_District"))+
  scale_colour_manual(values=c(Shilin_District="darkorchid",Zhongzheng_District="darkkhaki",Beitou_District="coral",Nangang_District="darkturquoise",Wanhua_District="chartreuse3"))+
  ylab('Rate')+xlab('Date')+
  geom_point(data=taipei_data_2[which.max(taipei_data_2$smooth_2),], aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_9_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2)  + 
  geom_point(data=head(filtered_data_8_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) +
  geom_point(data=tail(filtered_data_8_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_7_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) +
  geom_point(data=tail(filtered_data_7_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_6_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) +
  geom_point(data=tail(filtered_data_6_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_5_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) +
  geom_point(data=tail(filtered_data_5_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_4_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) +
  geom_point(data=tail(filtered_data_4_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_3_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) +
  geom_point(data=tail(head(filtered_data_3_2,n=-5),n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_2_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=head(filtered_data_1_2,n=1), aes(y=smooth_2),color="darkkhaki", size=2) + 
  geom_point(data=taipei_data_5[which.max(taipei_data_5$smooth_2),], aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_9_5,n=1), aes(y=smooth_2),color="coral", size=2)  + 
  geom_point(data=head(filtered_data_8_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=tail(filtered_data_8_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_7_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=tail(filtered_data_7_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_6_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=tail(filtered_data_6_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_5_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=tail(filtered_data_5_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_4_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=tail(filtered_data_4_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_3_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=tail(filtered_data_3_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_2_5,n=1), aes(y=smooth_2),color="coral", size=2) + 
  geom_point(data=head(filtered_data_1_5,n=1), aes(y=smooth_2),color="coral", size=2) +
  geom_point(data=taipei_data_6[which.max(taipei_data_6$smooth_2),], aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_9_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2)  + 
  geom_point(data=head(filtered_data_8_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=tail(filtered_data_8_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_7_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=tail(filtered_data_7_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_6_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=tail(filtered_data_6_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_5_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=tail(filtered_data_5_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_4_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=tail(filtered_data_4_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_3_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=tail(filtered_data_3_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_2_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) + 
  geom_point(data=head(filtered_data_1_6,n=1), aes(y=smooth_2),color="darkturquoise", size=2) +
  geom_point(data=taipei_data_7[which.max(taipei_data_7$smooth_2),], aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_9_7,n=1), aes(y=smooth_2),color="darkorchid", size=2)  + 
  geom_point(data=head(filtered_data_8_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=tail(filtered_data_8_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_7_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=tail(filtered_data_7_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_6_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=tail(filtered_data_6_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_5_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=tail(filtered_data_5_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_4_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=tail(filtered_data_4_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_3_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=tail(head(filtered_data_3_7,n=-5),n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_2_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) + 
  geom_point(data=head(filtered_data_1_7,n=1), aes(y=smooth_2),color="darkorchid", size=2) +
  geom_point(data=taipei_data_12[which.max(taipei_data_12$smooth_2),], aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_9_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2)  + 
  geom_point(data=head(filtered_data_8_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  geom_point(data=tail(filtered_data_8_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_7_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  geom_point(data=tail(filtered_data_7_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_6_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  geom_point(data=tail(filtered_data_6_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_5_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  geom_point(data=tail(filtered_data_5_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_4_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  geom_point(data=tail(filtered_data_4_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_3_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  geom_point(data=tail(filtered_data_3_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_2_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) + 
  geom_point(data=head(filtered_data_1_12,n=1), aes(y=smooth_2),color="chartreuse3", size=2) +
  guides(col=guide_legend("District"))
