# Load data ---------------------------------------------------------------

source("R/transformation.R")


# Load libraries ----------------------------------------------------------

library("ggplot2")
#library("readr")


# Plot cumulated video statistics -----------------------------------------

base_plot <- statistics %>% ggplot(aes(x = date)) + 
  geom_vline(aes(xintercept = date), data = filter(statistics, videos != 0), color = "red") +
  theme_minimal()

# total viewtime
base_plot + geom_line(aes(y = viewtime))

# number of new subscribers
base_plot + geom_line(aes(y = subs))


# Plot individual video statistics ----------------------------------------

data %>% ggplot(aes(x = date)) +
  geom_line(aes(y = v01), color = "red") +
  geom_line(aes(y = v10), color = "green") +
  geom_line(aes(y = v20), color = "blue") +
  scale_x_date() + 
  theme_minimal()