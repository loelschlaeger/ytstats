videos <- read.csv("data/videos.csv")
colnames(videos) <- c("date","videos")
viewtime <- read.csv("data/viewtime.csv", header = TRUE)
colnames(viewtime) <- c("date","viewtime")
subs <- read.csv("data/subs.csv", header = TRUE)
colnames(subs) <- c("date","subs")
data <- merge(videos, viewtime, by = "date")
data <- merge(data, subs, by = "date")
data$date <- as.Date(data$date)

head(data)

library(ggplot2)

plot <- ggplot(data, aes(x = date)) + 
  geom_vline(aes(xintercept = date), data = dplyr::filter(data, videos != 0), color = "red") +
  theme_minimal()

plot + geom_line(aes(y = viewtime))

plot + geom_line(aes(y = subs))

