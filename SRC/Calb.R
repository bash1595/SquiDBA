### Derive SquiDBA ###

# Open Packages

library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(devtools)

# Read in necessary data files #

# Calibration data 
calib_file <- read.csv("Data/8_21_calb2.CSV")
#View(calib_file)

# Raw deployment data
data_file <- read.csv("Data/1_26_19_.squid16_raw.CSV")
#View(data_file)

# Manipulate data # 

# time off by 12 hours, fix raw calibration data 
calib_data <- calib_file %>% 
  mutate(timestamp = mdy_hms(paste(`Date.MM.DD.YYYY.`, `Time.hh.mm.ss.`)) + hours(12))

# Calibration calculations #

# Pick times for orientation begin and end 
# -z +y +z -y +x -x

neg_z_start <- mdy_hms("08/21/2018 19:57:00")
neg_z_end <- mdy_hms("08/21/2018 19:58:10")
pos_y_start <- mdy_hms("08/21/2018 19:58:15")
pos_y_end <- mdy_hms("08/21/2018 19:58:50")
pos_z_start <- mdy_hms("08/21/2018 19:59:00")
pos_z_end <- mdy_hms("08/21/2018 19:59:30")
neg_y_start <- mdy_hms("08/21/2018 19:59:45")
neg_y_end <- mdy_hms("08/21/2018 20:00:15")
pos_x_start <- mdy_hms("08/21/2018 20:00:45")
pos_x_end <- mdy_hms("08/21/2018 20:01:20")
neg_x_start <- mdy_hms("08/21/2018 20:01:30")
neg_x_end <- mdy_hms("08/21/2018 20:02:00")

# Calculate median acc value during each orientation

neg_z_val <- median(filter(calib_data, between(timestamp, neg_z_start, neg_z_end))$ACCELZ)
pos_y_val <- median(filter(calib_data, between(timestamp, pos_y_start, pos_y_end))$ACCELY)
pos_z_val <- median(filter(calib_data, between(timestamp, pos_z_start, pos_z_end))$ACCELZ)
neg_y_val <- median(filter(calib_data, between(timestamp, neg_y_start, neg_y_end))$ACCELY)
pos_x_val <- median(filter(calib_data, between(timestamp, pos_x_start, pos_x_end))$ACCELX)
neg_x_val <- median(filter(calib_data, between(timestamp, neg_x_start, neg_x_end))$ACCELX)

# Solve for m and b in each axis

xcal <- solve(matrix(c(neg_x_val, 1, pos_x_val, 1), nrow = 2, byrow = TRUE), c(-1, 1))
mx <- xcal[1]
bx <- xcal[2]
ycal <- solve(matrix(c(neg_y_val, 1, pos_y_val, 1), nrow = 2, byrow = TRUE), c(-1, 1))
my <- ycal[1]
by <- ycal[2]
zcal <- solve(matrix(c(neg_z_val, 1, pos_z_val, 1), nrow = 2, byrow = TRUE), c(-1, 1))
mz <- zcal[1]
bz <- zcal[2]

# Calibrate data

addMS <- function(t) {
  # assumes all timestamps fall on the same second
  ms <- seq(0, 1, length.out = length(t) + 1)
  ms <- ms[-length(ms)]
  t + ms
}
data <- data_file %>%
  rename(rawaccx = ACCELX,
         rawaccy = ACCELY,
         rawaccz = ACCELZ) %>%
  mutate(timestampBAD = mdy_hms(paste(`Date.MM.DD.YYYY.`, `Time.hh.mm.ss.`)),
         accx = mx * rawaccx + bx,
         accy = my * rawaccy + by,
         accz = mz * rawaccz + bz) %>%
  group_by(timestampBAD) %>%
  mutate(timestamp = addMS(timestampBAD)) %>%
  ungroup %>%
  mutate(t = as.numeric(timestamp - min(timestamp), unit = "secs"))
View(data)

# Plot # 
data %>%
  filter(between(timestamp,
                 mdy_hms("1/26/2019 15:40:30"),
                 mdy_hms("1/26/2019 15:40:45"))) %>%
  gather(axis, value, accx:accz) %>% 
  ggplot(aes(timestamp, value, color = axis)) +
  geom_line() +
  theme_classic() +
  labs(x = "Time (s)",
       y = "Acceleration (g)") + 
  ylim(-1.1, .6)
