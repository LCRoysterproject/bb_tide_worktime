library("tidyverse")
library("dplyr")
library("ggplot2")
library("rtide") 
library("scales")
library("BBmisc")

#https://www.rdocumentation.org/packages/BBmisc/versions/1.10/topics/normalize
#https://www.vanessen.com/images/PDFs/Diver-ProductManual-en.pdf

#### Times for pressure and tides are GMT

setwd("C:/Users/Mel/Desktop/tide_inundation/data")

#### Reading all of the pressure files that are .csv
# Pressue is in millibars
pressure_temp <- list.files(pattern="*.csv")

# Combining all files into a data frame
pressure <- do.call(rbind, lapply(pressure_temp, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Time in is GMT
pressure$date<- as.POSIXct(pressure$DATE.TIME, format="%m/%d/%Y %H:%M")

colnames(pressure)<- c("date_time", "windspeed", "direction", "gust", "at", "baro_pressure", "relhum", "vis", "date")


#### Reading all of the observed tidal files that are .txt

#### Verified tidal
setwd("C:/Users/Mel/Desktop/tide_inundation/data/tidal")

tidal_temp = list.files(pattern="*.csv")

# Combining all files into a data frame
tidal <- do.call(rbind, lapply(tidal_temp, function(x) read.csv(x, stringsAsFactors = FALSE)))

#For tides need to round to the nearest hour to match with the pressure

tidal$date<- paste(tidal$Date, tidal$Time..GMT.)

tidal$date<- as.POSIXct(tidal$date, format="%m/%d/%Y %H:%M")

colnames(tidal)<- c("datex", "timex", "predicted", "prelim", "verified", "date")

tidal$normalization<- normalize(tidal$verified, method= "range", range= c(0,1), margin = 1L, on.constant = "quiet")


####Predicted tidal 

tidal$pred_normalization<- normalize(tidal$predicted, method= "range", range= c(0,1), margin = 1L, on.constant = "quiet")


##### Sensor 3 data
#### Time before January 2018 is subject to incorrect daylight saving time, time *****

setwd("C:/Users/Mel/Desktop/tide_inundation/data/wq")

site_3<-read.csv("wq.csv", header=T)

#Pressure data are cm
site_3<- site_3 %>% 
  filter(Site==3) %>% 
  select(Date, Pressure, Temperature)

colnames(site_3)<- c("date", "pressure", "temperaure")

site_3$kelvin<- (site_3$temperaure + 273.15)

site_3$date<- as.POSIXct(site_3$date)

sub.1 <- subset(site_3, date >= as.POSIXct('2017-03-12 02:00:00') &
                  date <= as.POSIXct('2017-11-05 01:00:00'))

sub.1$date<- sub.1$date + 4* 60 *60

sub.2 <- subset(site_3, date >= as.POSIXct('2017-11-05 02:00:00') &
                  date <= as.POSIXct('2018-05-11 05:00:00'))

sub.2$date<- sub.2$date + 5* 60 *60

site<- site_3 %>% 
  filter(!(date <= '2018-05-11 06:00:00'))

site.1<-site_3 %>% 
  filter(date >= '2018-05-11 06:00:00')


site.2<- rbind(sub.1, sub.2)
site_3<- rbind(site.2, site.1)


#Convert UTC EST to GMT
#site_3$Date<- as.POSIXct(site_3$date)
#site_3$Date<-format(site_3$date, tz="America/Los_Angeles")
#site_3$Date<- format(site_3$date, tz="EST+4")

# Need to convert sensor 3 cm to millibars, need to mulitply by 0.980665,


#Elevation formula as per Diver manual, below are the different variables needed

#Ph= atmospheric pressure at elevation height at H
#P0= atmospheric pressure at reference height
#m= 28.8 *10^-3
#g= 9.81 m/s (standard gravity)
#R= 8.314 j/mol/k
#t= temperature in Kelvin 
#H= height

#Ph= P0*e^- (M*g*H)/(R*T)

#WQ 3 is -1.422m 
#Cedar Key tide guage is at zero is -0.687

site_3$ph<- (site_3$pressure*(-(exp((0.0288*9.81*-1.422)/(8.314*site_3$kelvin)))))

#normalization using `normalize` from the package BBmisc

site_3$normalization<- normalize(site_3$ph, method= "range", range= c(0,1), margin = 1L, on.constant = "quiet")


#Plotting

#cols<- c("Predicted Tidal Height"=  "#0072B2","Target"="black", "Target +3"="#D55E00", "Target -6"="#E69F00", "Refrigerator"="#999999", "Ph" = "darkblue")

ggplot() +
  geom_line(data = tidal, aes(x = as.POSIXct(date), y = pred_normalization, color= "Predicted Tidal Height (m)"), size =1.2)  +
  
  geom_line(data = tidal, aes(x = as.POSIXct(date), y = normalization, color= "Tidal Height (m)"), size =1.2)  +
  
  geom_line(data = site_3, aes(x = as.POSIXct(date), y = normalization, color= "Atmospheric Pressure (Ph calcuation)"), size =1.2)  +
   
  scale_x_datetime(name = "January 3-4, 2019 GMT", #<- can be in labs(), also, but fine here, since we need to use scale_x_dateime anyways
                   labels = date_format("%d-%H:%M", tz="America/New_York"),
                   breaks = "4 hours",
                   limits = c(
                     as.POSIXct("2019-01-03 00:00:00 CET"),
                     as.POSIXct("2019-01-04 00:00:00 CET"))) +
  
  #scale_linetype_manual(values = c(1,2,3)) + 
  
  scale_colour_manual(values=c("#0072B2","#D55E00","#CC79A7")) +
  
  labs(title= "Cedar Key", y= "Tide Height NAVD (m)", color= "Tidal Lines") +

  theme(legend.position=("bottom"),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 45, hjust = 1)) #<- to make the a-axis ticks 90 degrees


setwd("C:\\Users\\melimore86\\Desktop\\tide_pressure")
ggsave("pic/20190103_inundation.png", dpi=300, width= 8, height= 6 )
