# Load libraries ----------------------------------------------------------

library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)


# Prepare cumulated video statistics --------------------------------------

videos <- read.csv("data/videos.csv")                    # indicator for release
colnames(videos) <- c("date","videos")
viewtime <- read.csv("data/viewtime.csv", header = TRUE) # viewtime in hours
colnames(viewtime) <- c("date","viewtime")
subs <- read.csv("data/subs.csv", header = TRUE)         # change in subscribers
colnames(subs) <- c("date","subs")
data <- merge(videos, viewtime, by = "date")
data <- merge(data, subs, by = "date")
data$date <- as.Date(data$date)
head(data)


# Plot cumulated video statistics -----------------------------------------

base_plot <- ggplot(data, aes(x = date)) + 
  # mark video release dates in red
  geom_vline(aes(xintercept = date), data = filter(data, videos != 0), color = "red") +
  theme_minimal()

# total viewtime
base_plot + geom_line(aes(y = viewtime))

# number of new subscribers
base_plot + geom_line(aes(y = subs))


# Prepare individual video statistics -------------------------------------

viewtime_by_video <- rbind(read.csv("data/viewtime_by_video1.csv"),
                           read.csv("data/viewtime_by_video2.csv"),
                           read.csv("data/viewtime_by_video3.csv"))
colnames(viewtime_by_video) <- c("date","video_id","video_titel","release","viewtime")

viewtime_by_video$date <- as.Date(viewtime_by_video$date)
viewtime_by_video$release <- parse_date(viewtime_by_video$release, "%b %d, %Y", local = locale("en"))

# add missing data entry
viewtime_by_video[which(viewtime_by_video$video_id == "LqhE2Fm6cJc"), "release"] <- as.Date("2020-01-12")

# table of video ids
unique_video_ids <- viewtime_by_video %>% distinct(video_id, .keep_all = TRUE) %>% arrange(release) %>% select(video_id, video_titel, release)

# remove release data and video title
viewtime_by_video %<>% select(-c(release, video_titel))

# relabel the video ids by order of release
viewtime_by_video$video_id <- factor(viewtime_by_video$video_id, levels = unique_video_ids$video_id)
levels(viewtime_by_video$video_id) <- 1:30
viewtime_by_video %<>% arrange(video_id) 

# optionally reshape data set in wide format
reshape(viewtime_by_video, idvar = "date", timevar = "video_id", direction = "wide") %>% head()


# Plot individual video statistics ----------------------------------------

ggplot(viewtime_by_video %>% filter(video_id == c(1,25,30)), # select video ids
       aes(x = date, y = viewtime, color = video_id, group = video_id)) +
  geom_line() +
  scale_x_date() + 
  theme_minimal()
