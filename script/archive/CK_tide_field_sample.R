#an exercise to plot the tides in CK over a week using the reference
#points for the construction work

#install.packages('rtide') 
# install the package on your computer

library(rtide) 
library(ggplot2)
library(scales) # for the date_format function


#load the package whenever you start a new R session

#tide_stations() 
#this returns all stations in US

#tide height from tide_stations is in MLLW in meters

dat = tide_height('Cedar Key',from = as.Date('2018-10-25'),
                  to = as.Date('2019-04-30'), minutes = 15, 
                  tz = 'America/New_York')

#the dates here are the dates you are interested in. So you enter a from and to date in YYYY-MM-DD format.  minutes are the minutes for the prediction so a value of 15 is a predicted tide every 15 minutes

x_conversion<- -0.687
dat$TideHeight_NAVD_m=dat$TideHeight + x_conversion 
#convert from MLLW to NAVD using conversion from Peter's table

ft_conversion<- 3.281
dat$TideHeight_NAVD_ft=dat$TideHeight_NAVD_m*ft_conversion
#convert from m to ft using 1 m = 3.281 ft

#windows(record=T)

#Adding the lines variables and the colors for each line, for easier calling in scale_color_manual

cols<- c("Predicted Tidal Height"=  "#0072B2","Target"="black", "Target +3"="#D55E00", "Target -6"="#E69F00", "Refrigerator"="#999999")

#Plot

ggplot(data = dat, aes(x = DateTime, y = TideHeight_NAVD_ft)) +
  
  geom_line(aes(color= "Predicted Tidal Height"), size =1.2, linetype=1) + #<- wanted to add this as part of the legend
  
  #Adding the day and every 4 hours, with breaks = date_breaks(), for the time in the x-axis legend
  scale_x_datetime(name = "Apr 21-30, 2019", #<- can be in labs(), also, but fine here, since we need to use scale_x_dateime anyways
                   labels = date_format("%d-%H:%M", tz="America/New_York"), breaks = date_breaks("4 hours")) +
  
  #Notes- Used the link that was here, and some other links, but the `yintercep`t has to be inside the aes() for the legend to show for that geom_hline object, this was difficult to discover and might have been different before the new ggplot2 update
  
  geom_hline(aes(color = "Target",yintercept = -2.5),size=1.2,linetype = 2) +
  
  # geom_hline(aes(color = "Target +3",yintercept = -1.20),size=1.5, linetype = 3) +
  # 
  # geom_hline(aes(color = "Target -6",yintercept = -1.95),size=1.5, linetype = 3) +
  # 
  # geom_hline(aes(color = "Refrigerator",yintercept = -0.7),size=1.3, linetype = 1) +
  
  #Rearranged the scale_color_manual, to show the legend in the order of the lines shown in "breaks=c()" instead of in alpahebtical order (which is the default)
  scale_color_manual(values =cols, breaks = c("Predicted Tidal Height","Target"),guide= 'legend') +
  
  guides(colour = guide_legend(override.aes = list(linetype=c(1,3))))+
  
 
  #Added labs() and removed ggtitle  and scale_y_continuous (this is more for y-axis control such as using limits=c(start,end)), so I can rename the legend in color = (legend title) 
  labs(main= "Cedar Key", ylab= "Tide Height NAVD (ft)", color= "Tidal Lines") +

  #Added some themes, can change legend to "top", "bottom", and "left" if desired
 theme(legend.position=("top"),
       panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
       axis.text.x = element_text(angle = 90, hjust = 1)) #<- to make the a-axis ticks 90 degrees


names(dat)

#editing the file to just the heights Peter wants to see

dat2<- dat[dat$TideHeight_NAVD_ft<=-2.5,]

dat2$TideHeight_NAVD_ft<-round(dat2$TideHeight_NAVD_ft,digits=2)
dat2$TideHeight<-round(dat2$TideHeight,digits=2)
dat2$TideHeight_NAVD_m<-round(dat2$TideHeight_NAVD_m,digits=2)


colnames(dat2)<- c("station", "date_time", "tide_mllw_m","tide_navd_m","tide_navd_ft")
write.table(dat2,file="C:\\Users\\billpine\\Documents\\Float\\tide.txt", sep = ",", quote = FALSE, row.names = F)


round_stat_overall=lapply(stat_overall,round,2)


#-1.45 is Target
#-1.2 is Target +3
#-1.95 Target -6
#-0.7 is Refrigerator


