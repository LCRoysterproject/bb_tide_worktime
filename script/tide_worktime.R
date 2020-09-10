#an exercise to plot the tides in CK over a week using the reference
#point  from Peter as far as what tidal height he wants to work below

#code is by the wizard Ben Toh using small pieces from bp

# install.packages('rtide')
# install the package on your computer

library(rtide) 
library(tidyverse)
library(lubridate)
library(suncalc) #for sunrise/sunset

#load the package whenever you start a new R session

# tide_stations() 
#this returns all stations in US

#tide height from tide_stations is in MLLW in meters
start_date <- '2020-10-01'
end_date <- '2021-04-01'
start_date <- ymd(start_date)
end_date <- ymd(end_date)

dat = tide_height('Cedar Key',from = start_date,
                  to = end_date, minutes = 15, 
                  tz = 'America/New_York')

#the dates here are the dates you are interested in. 
#So you enter a from and to date in YYYY-MM-DD format.  
#minutes are the minutes for the prediction so a value of 15 is 
#a predicted tide every 15 minutes

#need to review all of these conversions with Peter as they 
#come from an Excel file he made

#down load is in MLLW in meters

x_conversion<- -0.687
dat$TideHeight_NAVD_m <- dat$TideHeight + x_conversion 
#convert from MLLW meters to NAVD meters using conversion from Peter's table

ft_conversion<- 3.281
dat$TideHeight_NAVD_ft=dat$TideHeight_NAVD_m*ft_conversion
#convert from m to ft using 1 m = 3.281 ft

#rename columns
colnames(dat) <- c("station", "datetime", "mllw_m", "navd_m","navd_ft")

#### Start from here, this is the tidal height threshold in NAVD ft Peter wants
threshold <- -2.5
suitable <- dat$navd_ft < threshold

# suitability of time t+1 minus time t
# 1 = start of suitable interval, -1 = end of suitable interval
startstop <- diff(suitable)
startstop <- c(0, startstop) # first observation has nothing to compare to, so we add a dummy here
dat$startstop <- startstop

# Now it is possible that the first observation is already in the low tide period, this will
# screw up our algorithm later. So we cut away all data before the first "1" in our startstop 
# vector. Likewise on the tail end.
start <- which(startstop == 1)[1]
end <- which(startstop == -1)
end <- end[length(end)]
dat_spliced <- dat[start:end, ]

# Using this "scheme", our "start" time is already below -2.5
# But the "stop" time is above -2.5.
start_index <- which(dat_spliced$startstop == 1)
stop_index <- which(dat_spliced$startstop == -1)
start_datetime <- dat_spliced$datetime[start_index]
start_navd_ft <- dat_spliced$navd_ft[start_index]
stop_datetime <- dat_spliced$datetime[stop_index]
stop_navd_ft <- dat_spliced$navd_ft[stop_index]

# Now build a table of the intervals
interval_df <- data.frame(start_datetime = start_datetime, stop_datetime = stop_datetime,
                          start_navd_ft = start_navd_ft, stop_navd_ft = stop_navd_ft)
interval_df <- interval_df %>%
  mutate(length_min = as.numeric(stop_datetime - start_datetime),
         start_date = date(start_datetime),
         start_time = strftime(start_datetime, "%H:%M:%S", usetz = T),
         stop_date = date(stop_datetime),
         stop_time = strftime(stop_datetime, "%H:%M:%S", usetz = T),
         weekday = wday(start_datetime, label = T),
         
         ampm = ifelse(am(start_datetime), "AM", "PM"))

# grab the sunrise/sunset for everyday throughout the period, the day before 
# and day after for safety purpose
sun_df <- getSunlightTimes(date = seq(start_date - 1, end_date + 1, by = 1), 
                           lat = 29.14, lon = -83.04, keep = c("sunrise", "sunset"),
                           tz = "America/New_York")
sun_df$date <- date(sun_df$date)

# Create a function to check how many minutes of a given start stop time is within the
# period between sunrise and sunset
daytime_minutes <- function (start, stop) {
  dates <- date(start)
  dates <- c(dates - 1, dates, dates + 1)
  sun_df_subset <- sun_df %>%
    filter(date %in% dates) %>%
    dplyr::select(sunrise, sunset)
  
  # Create sequence of minutes between start stop
  # Note that a 10 min interval will have 11 values, so need to remove one
  # (Removing first one out of convenient)
  startstop_minutes <- seq(start, stop, by = "min")[-1]
  
  # For each day (row in sun_df), find if each minute is within the sunrise-sunset 
  # period of that day
  daytime <- apply(sun_df_subset, 1, 
                   function (x) x[1] <= startstop_minutes & x[2] >= startstop_minutes)
  daytime <- max(colSums(daytime))
  
  return(daytime)
}

interval_df$mins_sun <- NA

# Hate to use for loop but seems to be easiest in this case
# Loop through every pair of start and stop time, apply daytime_minutes() function
# to find out how many minutes in the interval is in the sun

for (i in 1:nrow(interval_df)) {
  interval_df$mins_sun[i] <- with(interval_df, daytime_minutes(start_datetime[i], stop_datetime[i]))
}

# the data frame below interval_df is the full output
interval_df <- interval_df %>%
  mutate(sun_perc = mins_sun/length_min * 100)

# just removing a few columns we don't need
clean_df <- interval_df %>%
  select(-start_datetime,-stop_datetime,-stop_date)

# now rounding the columns we are interested in

clean_df$start_navd_ft<-round(clean_df$start_navd_ft,digits=2)
clean_df$stop_navd_ft<-round(clean_df$stop_navd_ft,digits=2)
clean_df$sun_perc<-round(clean_df$sun_perc,digits=2)

#now ordering the columns as we are interested
final<-clean_df[c("start_date","start_time", "weekday","ampm","start_navd_ft","stop_navd_ft","length_min","mins_sun","sun_perc")]

write.csv(final,file="work_time_2020.csv")


