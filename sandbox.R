# Read and save YouTube data ----------------------------------------------

ytdata_example <- read_yt(
  email = "oelschlaeger.lennart@gmail.com",
  from = "2020-01-01",
  to = "2022-09-01"
)

usethis::use_data(ytdata_example, compress = "xz", overwrite = TRUE)

# Load YouTube data -------------------------------------------------------

data <- ytstats::ytdata_example
print(data)
summary(data)
attributes(data)$videos  # access video data
attributes(data)$channel # access channel data

# HMM application ---------------------------------------------------------

library("ggplot2", warn.conflicts = FALSE)
library("dplyr", warn.conflicts = FALSE)

ts <- data %>% 
  group_by(day) %>% 
  summarize(x = mean(viewmins)) %>% 
  pull(x) %>% 
  scale(center = FALSE)
model <- fit_hmm(ts, N = 3)
states <- decode_states(ts, model)
plot(ts, type = "l")
points(ts, col = states)

ts <- data$likes
model <- fit_hmm(ts, N = 3)
states <- decode_states(ts, model)
plot(ts, type = "l")
points(ts, col = states)
