# Load libraries ----------------------------------------------------------

library("dplyr")
library("lubridate")


# Read cumulated video statistics -----------------------------------------

videos <- read.csv("data/videos.csv")                    
colnames(videos) <- c("date", "videos")
viewtime <- read.csv("data/viewtime.csv", header = TRUE) 
colnames(viewtime) <- c("date", "viewtime")
subs <- read.csv("data/subs.csv", header = TRUE)        
colnames(subs) <- c("date", "subs")
statistics <- merge(videos, viewtime, by = "date")
statistics <- merge(statistics, subs, by = "date")
statistics <- as_tibble(statistics)
statistics$date <- ymd(statistics$date)


# Read viewtime per video -------------------------------------------------

# Load data
viewtime_v1 <- read.csv("data/viewtime_by_video1.csv", header = TRUE)
viewtime_v2 <- read.csv("data/viewtime_by_video2.csv", header = TRUE)
viewtime_v3 <- read.csv("data/viewtime_by_video3.csv", header = TRUE)

# Combine
viewtime <- rbind(viewtime_v1, viewtime_v2, viewtime_v3)
colnames(viewtime) <- c("date", "video", "title", "release", "hours")
viewtime <- as_tibble(viewtime)
viewtime$date <- ymd(viewtime$date)
viewtime$release <- mdy(viewtime$release)

# Release date of "Hi"-video missing, filled in here 
viewtime <- mutate(viewtime, release = replace(release, is.na(release), "2020-01-12")) 

# order by 'date'
viewtime <- arrange(viewtime, date)


# Collect garbage ---------------------------------------------------------

rm(subs, videos, viewtime_v1, viewtime_v2, viewtime_v3)
