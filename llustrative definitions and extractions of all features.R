library(ggplot2)
library(showtext)
library(lubridate)
taipei_data <- read.csv('df_taipei.csv')
taipei_data$個案研判日<- ymd(taipei_data$個案研判日)
filtered_data_9 <- subset(taipei_data,smooth_2 > 0.9*max(taipei_data$smooth_2))
filtered_data_9['horizon'] <- rep(0.9*max(taipei_data$smooth_2),nrow(filtered_data_9))
filtered_data_8 <- subset(taipei_data,smooth_2 > 0.8*max(taipei_data$smooth_2))
filtered_data_8['horizon'] <- rep(0.8*max(taipei_data$smooth_2),nrow(filtered_data_8))
filtered_data_7 <- subset(taipei_data,smooth_2 > 0.7*max(taipei_data$smooth_2))
filtered_data_7['horizon'] <- rep(0.7*max(taipei_data$smooth_2),nrow(filtered_data_7))
filtered_data_6 <- subset(taipei_data,smooth_2 > 0.6*max(taipei_data$smooth_2))
filtered_data_6['horizon'] <- rep(0.6*max(taipei_data$smooth_2),nrow(filtered_data_6))
filtered_data_5 <- subset(taipei_data,smooth_2 > 0.5*max(taipei_data$smooth_2))
filtered_data_5['horizon'] <- rep(0.5*max(taipei_data$smooth_2),nrow(filtered_data_5))
filtered_data_4 <- subset(taipei_data,smooth_2 > 0.4*max(taipei_data$smooth_2))
filtered_data_4['horizon'] <- rep(0.4*max(taipei_data$smooth_2),nrow(filtered_data_4))
filtered_data_3 <- subset(taipei_data,smooth_2 > 0.3*max(taipei_data$smooth_2))
filtered_data_3['horizon'] <- rep(0.3*max(taipei_data$smooth_2),nrow(filtered_data_3))
filtered_data_2 <- subset(taipei_data,smooth_2 > 0.2*max(taipei_data$smooth_2))
filtered_data_2['horizon'] <- rep(0.2*max(taipei_data$smooth_2),nrow(filtered_data_2))
filtered_data_1 <- subset(taipei_data,smooth_2 > 0.1*max(taipei_data$smooth_2))
filtered_data_1['horizon'] <- rep(0.1*max(taipei_data$smooth_2),nrow(filtered_data_1))



desired_breaks <- c(head(filtered_data_8$個案研判日,n=1),head(filtered_data_9$個案研判日,n=1), filtered_data_9[6,]$個案研判日,taipei_data[which.max(taipei_data$smooth_2),]$個案研判日, tail(filtered_data_9$個案研判日,n=1),tail(filtered_data_8$個案研判日,n=1))
ggplot(taipei_data, aes(個案研判日)) + 
  geom_point(aes(y = 當日確診率, colour = "var0")) + 
  geom_line(aes(y = smooth_2, colour = "var1"))+
  ylab('Rate')+xlab('Date')+
  geom_point(data=taipei_data[which.max(taipei_data$smooth_2),], aes(y=smooth_2),color="black", size=4) + 
  geom_text(data = taipei_data[which.max(taipei_data$smooth_2),], aes(y=smooth_2, label=paste0("h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5)+
  geom_hline(yintercept = max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.9*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.8*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.7*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.6*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.5*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.4*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.3*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.2*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_hline(yintercept = 0.1*max(taipei_data$smooth_2) ,linetype = 2)+
  geom_point(data=head(filtered_data_9,n=1), aes(y=horizon),color="black", size=4)  + 
  geom_text(data = tail(filtered_data_9,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.9*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_8,n=1), aes(y=horizon),color="black", size=4) +
  geom_point(data=tail(filtered_data_8,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = tail(filtered_data_8,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.8*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_7,n=1), aes(y=horizon),color="black", size=4) +
  geom_point(data=tail(filtered_data_7,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = tail(filtered_data_7,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.7*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_6,n=1), aes(y=horizon),color="black", size=4) +
  geom_point(data=tail(filtered_data_6,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = tail(filtered_data_6,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.6*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_5,n=1), aes(y=horizon),color="black", size=4) +
  geom_point(data=tail(filtered_data_5,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = tail(filtered_data_5,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.5*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_4,n=1), aes(y=horizon),color="black", size=4) +
  geom_point(data=tail(filtered_data_4,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = tail(filtered_data_4,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.4*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_3,n=1), aes(y=horizon),color="black", size=4) +
  geom_point(data=tail(head(filtered_data_3,n=-3),n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = tail(head(filtered_data_3,n=-3),n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.3*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_2,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = head(filtered_data_2,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.2*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+
  geom_point(data=head(filtered_data_1,n=1), aes(y=horizon),color="black", size=4) + 
  geom_text(data = head(filtered_data_1,n=1), aes(x=ymd("2022-03-25"),y=horizon, label=paste0("0.1*h[max]")),parse=TRUE, color="black",
            position = position_dodge(width = 3),
            vjust = -0.5, size = 5,hjust = 1)+ guides(colour = FALSE)+
  geom_vline(xintercept = taipei_data[which.max(taipei_data$smooth_2),]$個案研判日 ,linetype = 2)+
  geom_vline(xintercept = filtered_data_9[6,]$個案研判日 ,linetype = 2)+
  geom_vline(xintercept = head(filtered_data_9$個案研判日,n=1) ,linetype = 2)+
  geom_vline(xintercept = head(filtered_data_8$個案研判日,n=1) ,linetype = 2)+
  geom_vline(xintercept = tail(filtered_data_9$個案研判日,n=1) ,linetype = 2)+
  geom_vline(xintercept = tail(filtered_data_8$個案研判日,n=1) ,linetype = 2)+
  theme(text = element_text(size=15))
