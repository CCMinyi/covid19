library(tmap)
#Fig7 Fig 8
peak_data <- read.csv('peak/peak_data.csv')
taiwan_shape2 = readOGR('taiwan_data/gadm36_TWN_2.shp')
taiwan_shape2 <-  taiwan_shape2[taiwan_shape2$NL_NAME_2 !='金門縣',]
taiwan_shape2 <-  taiwan_shape2[taiwan_shape2$NL_NAME_2 !='馬祖列島',]
taiwan_shape2$zh_NAME = taiwan_shape2$NL_NAME_2
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='台中']='台中市'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='台南']='台南市'
peak_data$City = as.character(peak_data$City)
peak_data$最高點日期<- as.Date(peak_data$最高點日期, format= "%Y-%m-%d")
taiwan_shape2@data = merge(taiwan_shape2@data,peak_data,by.x = 'zh_NAME',by.y='City',sort=F,fill)
#change name
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='高雄市']='Kaohsiung'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='新北市']='New Taipei'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='台中市']='Taichung'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='台南市']='Tainan'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='台北市']='Taipei'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='彰化縣']='Changhua'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='嘉義市']='Chiayi City'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='嘉義縣']='Chiayi County'#CY
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='新竹市']='Hsinchu City'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='新竹縣']='Hsinchu County'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='花蓮縣']='Hualien'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='基隆市']='Keelung'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='苗栗縣']='Miaoli'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='南投縣']='Nantou'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='澎湖縣']='Penghu'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='屏東縣']='Pingtung'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='台東縣']='Taitung'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='桃園市']='Taoyuan'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='宜蘭縣']='Yilan'
taiwan_shape2$zh_NAME[taiwan_shape2$zh_NAME=='雲林縣']='Yunlin'

#Figure 7
tm_shape(taiwan_shape2) +                      
  tm_fill(col = "最高點日期",                  
          alpha = 1,                 
          style = "quantile",       
          palette = "Greens",           
          title = "Peak Date") + 
  tm_text(text="zh_NAME",size = 0.7,col="black")+
  tm_layout(legend.outside.position=c('right', 'bottom'),legend.outside= TRUE,frame = F)+
  tm_borders(col = "grey60",lwd = 0.5)


#Figure 8
tm_shape(taiwan_shape2) +                      
  tm_fill(col = "累積確診率",                   
          alpha = 1,                   
          style = "quantile", n=5,       
          palette = "Greens",           
          title = "Cumulative Rate") + 
  tm_layout(legend.outside.position=c('right', 'bottom'),legend.outside= TRUE,frame = F)+tm_text(text="zh_NAME",size = 0.7,col="black")+
  tm_borders(col = "grey60",lwd = 0.5)



