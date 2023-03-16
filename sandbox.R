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
  scale()
theta <- mle_hmm(x = ts, N = 3, dist = "gaussian", max_runs = 20)
separate_theta(theta = theta, N = 3, dist = "gaussian")
states <- decode_states(x = ts, theta = theta, dist = "gaussian", N = 3)
plot(ts, type = "l")
points(x = ts, col = states)

ts <- data %>%
  group_by(day) %>%
  summarize(x = sum(likes)) %>%
  pull(x) 
theta <- mle_hmm(x = ts, N = 2, dist = "poisson", max_runs = 20)
separate_theta(theta = theta, N = 2, dist = "poisson")
states <- decode_states(x = ts, theta = theta, dist = "poisson", N = 2)
plot(ts, type = "h")
points(x = ts, col = states)


