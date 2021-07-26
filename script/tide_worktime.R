### ESTIMATE OYSTER SAMPLING DAYS AND TIMES ###

# an exercise to estimate the working tides in CK over a week using the reference
# point from Peter as far as what tidal height he wants to work below

# code is by wizard Ben Toh using small pieces from BPine

# install needed packages
# install.packages('rtide')
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("suncalc")

# load packages
library(rtide)     # for tides
library(tidyverse)
library(lubridate)
library(suncalc)   # for sunrise/sunset



##### GET TIDE DATA #####
# view all tide stations in US
# tide_stations() 

# tide height from tide_stations is in MLLW in meters 
start_date <- "2021-10-01"      # change to your start date "YYYY-MM-DD" 
end_date   <- "2022-04-01"      # change to your end date "YYYY-MM-DD"
start_date <- ymd(start_date)   # correct format 
end_date   <- ymd(end_date)     # correct format

dat <- tide_height("Cedar Key",
                   from = start_date,
                   to = end_date, 
                   minutes = 15, 
                   tz = "America/New_York")

# dates here are dates you are interested in 
# you enter a from and to date in YYYY-MM-DD format  
# minutes are the minutes for the prediction so a value of 15 is 
# a predicted tide every 15 minutes

# need to review all of these conversions with Peter as they 
# come from an Excel file he made

# download is in MLLW in meters

# convert from MLLW meters to NAVD meters using conversion from Peter's table
x_conversion <- -0.687
dat$TideHeight_NAVD_m <- dat$TideHeight + x_conversion 

# convert from m to ft using 1 m = 3.281 ft
ft_conversion<- 3.281
dat$TideHeight_NAVD_ft <- dat$TideHeight_NAVD_m * ft_conversion

# rename columns
colnames(dat) <- c("station", "datetime", 
                   "mllw_m", "navd_m",
                   "navd_ft")



##### NOW WE CAN BEGIN ESTIMATING DATES #####
# this is the tidal height threshold in NAVD that Peter wants
threshold <- -2.5
suitable <- dat$navd_ft < threshold

# suitability of time t+1 minus time t
# 1  = start of suitable interval
# -1 = end of suitable interval
startstop <- diff(suitable)
startstop <- c(0, startstop) # 1st obs has nothing to compare, so add a dummy here
dat$startstop <- startstop

# now it is possible that the first observation is 
# already in the low tide period, this will screw up 
# our algorithm later; so we cut away all data before 
# the first "1" in our startstop vector, likewise on the tail end
start <- which(startstop == 1)[1]
end   <- which(startstop == -1)
end   <- end[length(end)]

# create new dataframe 
dat_spliced <- dat[start:end, ]
head(dat_spliced)

# using this "scheme", our "start" time is already below -2.5
# but the "stop" time is above -2.5
# use new data as well (dat_spliced)
start_index <- which(dat_spliced$startstop == 1)
stop_index  <- which(dat_spliced$startstop == -1)
start_datetime <- dat_spliced$datetime[start_index]
start_navd_ft  <- dat_spliced$navd_ft[start_index]
stop_datetime  <- dat_spliced$datetime[stop_index]
stop_navd_ft   <- dat_spliced$navd_ft[stop_index]

# now build a table of intervals
interval_df <- data.frame(start_datetime = start_datetime, 
                          stop_datetime = stop_datetime,
                          start_navd_ft = start_navd_ft, 
                          stop_navd_ft = stop_navd_ft)
interval_df <- interval_df %>%
  mutate(length_min = as.numeric(stop_datetime - start_datetime),
         start_date = date(start_datetime),
         start_time = strftime(start_datetime, "%H:%M:%S", usetz = T),
         stop_date  = date(stop_datetime),
         stop_time  = strftime(stop_datetime, "%H:%M:%S", usetz = T),
         weekday    = wday(start_datetime, label = T),
         ampm = ifelse(am(start_datetime), "AM", "PM"))

# grab the sunrise/sunset for everyday throughout the period, 
# the day before and day after for safety purpose
sun_df <- getSunlightTimes(date = seq(start_date - 1, 
                                      end_date + 1, 
                                      by = 1), 
                           lat = 29.14, lon = -83.04, 
                           keep = c("sunrise", "sunset"),
                           tz = "America/New_York")
sun_df$date <- date(sun_df$date)

# function to check how many minutes of a 
# given start stop time is within the
# period between sunrise and sunset
daytime_minutes <- function (start, stop) {
  dates <- date(start)
  dates <- c(dates - 1, dates, dates + 1)
  sun_df_subset <- sun_df %>%
    filter(date %in% dates) %>%
    dplyr::select(sunrise, sunset)
  
  # create sequence of minutes between start stop
  # note that a 10 min interval will have 11 values, so need to remove one
  # (removing first one out of convenience)
  startstop_minutes <- seq(start, stop, by = "min")[-1]
  
  # for each day (row in sun_df), find if each minute 
  # is within the sunrise-sunset period of that day
  daytime <- apply(sun_df_subset, 1, 
                   function (x) x[1] <= startstop_minutes & x[2] >= startstop_minutes)
  daytime <- max(colSums(daytime))
  
  # return daytime 
  return(daytime)
}

interval_df$mins_sun <- NA

# hate to use for loop but seems to be easiest in this case
# loop through every pair of start and stop time, 
# apply daytime_minutes() function to find out how many 
# minutes in interval is in sunlight (can't sample in dark)

for (i in 1:nrow(interval_df)) {
  interval_df$mins_sun[i] <- with(interval_df, 
                                  daytime_minutes(start_datetime[i],
                                                  stop_datetime[i]))
}

# data frame below interval_df is the full output
interval_df <- interval_df %>%
  mutate(sun_perc = mins_sun / length_min * 100)

# just removing a few columns we don't need
clean_df <- interval_df %>%
  select(-start_datetime, -stop_datetime, -stop_date)

# now rounding the columns we are interested in
clean_df$start_navd_ft <- round(clean_df$start_navd_ft, digits = 2)
clean_df$stop_navd_ft  <- round(clean_df$stop_navd_ft, digits = 2)
clean_df$sun_perc      <- round(clean_df$sun_perc, digits = 2)

# now ordering the columns as we are interested
final <- clean_df[c("start_date",      # date to sample
                    "start_time",      # time to start sample
                    "weekday",         # day of week
                    "ampm",            # AM or PM
                    "start_navd_ft",   # NAVD tidal feet at sample start
                    "stop_navd_ft",    # NAVD tidal feet at sample end
                    "length_min",      # total minutes to sample
                    "mins_sun",        # total minutes in daylight to sample
                    "sun_perc")]       # % total minutes in daylight

# save final file as "work_time_period_##.csv" 
# where period is what sampling period it is
write.csv(final, file = "data/work_time_period_24.csv")


