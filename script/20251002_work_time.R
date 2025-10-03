### ESTIMATE OYSTER SAMPLING DAYS AND TIMES (timezone-locked to America/New_York) ###

# required packages
# install.packages('rtide')
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("suncalc")

library(rtide)
library(tidyverse)
library(lubridate)
library(suncalc)

##### PARAMETERS #####
start_date <- "2025-10-01"
end_date   <- "2025-12-01"
tz_eastern <- "America/New_York"

start_date <- ymd(start_date)
end_date   <- ymd(end_date)

##### GET TIDE DATA (ensure tz requested) #####
dat <- tide_height("Cedar Key",
                   from = start_date,
                   to = end_date,
                   minutes = 15,
                   tz = tz_eastern)

# convert from MLLW (m) to NAVD (m) then to ft
x_conversion <- -0.687
ft_conversion <- 3.281

dat$TideHeight_NAVD_m  <- dat$TideHeight + x_conversion
dat$TideHeight_NAVD_ft <- dat$TideHeight_NAVD_m * ft_conversion

# rename to consistent column names
colnames(dat) <- c("station", "datetime", "mllw_m", "navd_m", "navd_ft")

# Ensure datetime column is POSIXct and locked to Eastern time
dat$datetime <- with_tz(as.POSIXct(dat$datetime, tz = attr(dat$datetime, "tzone") %||% "UTC"),
                        tzone = tz_eastern)

##### NOW WE CAN BEGIN ESTIMATING DATES TO WORK #####
threshold <- -2.5
suitable <- dat$navd_ft < threshold

# find start (1) and stop (-1) transitions
startstop <- diff(suitable)
startstop <- c(0, startstop)
dat$startstop <- startstop

# trim to first start and last stop to avoid partial tails
start_idx <- which(startstop == 1)[1]
end_idx   <- which(startstop == -1)
end_idx   <- end_idx[length(end_idx)]

dat_spliced <- dat[start_idx:end_idx, ]

# Force Eastern Time zone on spliced datetimes again (safety)
dat_spliced$datetime <- with_tz(as.POSIXct(dat_spliced$datetime, tz = attr(dat_spliced$datetime, "tzone") %||% "UTC"),
                                tzone = tz_eastern)

# robust pairing: find, for each start index, the first stop index after it
start_idx_all <- which(dat_spliced$startstop == 1)
stop_idx_all  <- which(dat_spliced$startstop == -1)

paired_stop_idx <- sapply(start_idx_all, function(si) {
  later <- stop_idx_all[stop_idx_all > si]
  if (length(later) == 0) NA_integer_ else later[1]
})

valid <- !is.na(paired_stop_idx)
start_index <- start_idx_all[valid]
stop_index  <- paired_stop_idx[valid]

start_datetime <- with_tz(dat_spliced$datetime[start_index], tzone = tz_eastern)
stop_datetime  <- with_tz(dat_spliced$datetime[stop_index],  tzone = tz_eastern)
start_navd_ft  <- dat_spliced$navd_ft[start_index]
stop_navd_ft   <- dat_spliced$navd_ft[stop_index]

# build intervals dataframe (POSIXct columns keep tz attribute)
interval_df <- data.frame(start_datetime = start_datetime,
                          stop_datetime  = stop_datetime,
                          start_navd_ft  = start_navd_ft,
                          stop_navd_ft   = stop_navd_ft,
                          stringsAsFactors = FALSE)

# Ensure POSIXct tz attributes are correct
interval_df$start_datetime <- with_tz(as.POSIXct(interval_df$start_datetime), tzone = tz_eastern)
interval_df$stop_datetime  <- with_tz(as.POSIXct(interval_df$stop_datetime),  tzone = tz_eastern)

# Use explicit tz-aware formatting and compute weekday without tz argument (datetime already eastern)
interval_df <- interval_df %>%
  mutate(length_min = as.numeric(difftime(stop_datetime, start_datetime, units = "mins")),
         start_date  = as.Date(start_datetime, tz = tz_eastern),
         start_time  = format(start_datetime, "%H:%M:%S", tz = tz_eastern),
         stop_date   = as.Date(stop_datetime,  tz = tz_eastern),
         stop_time   = format(stop_datetime,  "%H:%M:%S", tz = tz_eastern),
         weekday     = wday(start_datetime, label = TRUE, week_start = 1),
         ampm        = ifelse(am(start_datetime), "AM", "PM"))

# grab sunrise/sunset for the whole period (includes day before and after)
sun_df <- getSunlightTimes(date = seq(start_date - 1, end_date + 1, by = 1),
                           lat = 29.14, lon = -83.04,
                           keep = c("sunrise", "sunset"),
                           tz = tz_eastern)
sun_df$date <- as.Date(sun_df$date)

# helper to compute minutes in daylight for an interval (all times are Eastern)
daytime_minutes <- function(start, stop) {
  dates <- as.Date(start, tz = tz_eastern)
  dates <- c(dates - 1, dates, dates + 1)
  sun_df_subset <- sun_df %>% filter(date %in% dates) %>% dplyr::select(sunrise, sunset)
  
  # sequence of minutes between start and stop (exclude the first to match previous behavior)
  mins_seq <- seq(from = start, to = stop, by = "min")[-1]
  # ensure minutes have Eastern tz
  mins_seq <- with_tz(as.POSIXct(mins_seq, tz = tz_eastern), tzone = tz_eastern)
  
  if (length(mins_seq) == 0) return(0)
  
  # check per-day sunrise-sunset membership
  daytime_matrix <- apply(sun_df_subset, 1, function(x) {
    sr <- with_tz(as.POSIXct(x["sunrise"], tz = tz_eastern), tzone = tz_eastern)
    ss <- with_tz(as.POSIXct(x["sunset"],  tz = tz_eastern), tzone = tz_eastern)
    (sr <= mins_seq) & (ss >= mins_seq)
  })
  
  # column sums give minutes in daylight; take max across rows (covers cross-midnight intervals)
  max(colSums(daytime_matrix))
}

interval_df$mins_sun <- NA_real_

for (i in seq_len(nrow(interval_df))) {
  interval_df$mins_sun[i] <- daytime_minutes(interval_df$start_datetime[i],
                                             interval_df$stop_datetime[i])
}

interval_df <- interval_df %>%
  mutate(sun_perc = ifelse(length_min > 0, mins_sun / length_min * 100, 0))

# clean up and format final table, keep stop_time
clean_df <- interval_df %>%
  select(start_date, start_time, stop_time, weekday, ampm,
         start_navd_ft, stop_navd_ft, length_min, mins_sun, sun_perc)

clean_df$start_navd_ft <- round(clean_df$start_navd_ft, 2)
clean_df$stop_navd_ft  <- round(clean_df$stop_navd_ft, 2)
clean_df$sun_perc      <- round(clean_df$sun_perc, 2)

# append explicit timezone label to time strings so outputs are clearly Eastern wall-clock
final <- clean_df %>%
  mutate(start_time = paste0(start_time, " EDT/EST"),
         stop_time  = paste0(stop_time,  " EDT/EST"))

# write CSV (times are strings labeled as Eastern)
if (!dir.exists("data")) dir.create("data")
write.csv(final, file = "data/work_time_Oct_Nov_2025.csv", row.names = FALSE)


# simple plot of minutes of sun by date (start_date is Date)
final$start_date <- as.Date(final$start_date)

# base plot without x axis, points only
plot(final$start_date, final$mins_sun, xaxt = "n",
     xlab = "Date", ylab = "Minutes of Sun",
     type = "p", pch = 16)

# sequence of daily ticks
ticks.at <- seq(from = min(final$start_date, na.rm = TRUE),
                to   = max(final$start_date, na.rm = TRUE),
                by   = "1 day")

# labels for the ticks (daily); reduce label frequency if too crowded
ticks.lab <- format(ticks.at, "%b %d")

# draw axis with daily ticks; use las = 2 for vertical labels if needed
axis(1, at = ticks.at, labels = ticks.lab, las = 2, cex.axis = 0.8)
