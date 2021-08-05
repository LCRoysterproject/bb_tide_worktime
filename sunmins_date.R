a <- read.csv("data/work_time_period_24.csv", header = T)

head(a)

a$start_date <- as.Date(a$start_date, "%m/%d/%Y")

plot(a$start_date, a$mins_sun, xaxt = 'n', xlab = "Date", ylab = "Minutes of Sun")

ticks.at <- seq(min(a$start_date), max(a$start_date),
                by = "months")
## format the labels as abbreviated month names
ticks.lab <- format(ticks.at, format = "%b")

axis(1, at = ticks.at, labels = ticks.lab)
