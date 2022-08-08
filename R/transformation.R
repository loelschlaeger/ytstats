# Load data ---------------------------------------------------------------

source("R/reading.R")


# Load libraries ----------------------------------------------------------

library("magrittr")
library("tidyr")


# Create coding "book": encoded variables v01, v02, ... -------------------

coding <- viewtime %>% 
  group_by(title) %>% 
  summarise(release = unique(release)) %>% 
  arrange(!is.na(release), release) %>%  
  mutate(key = ifelse(1:30 < 10,
                      yes  = paste0("v0", 1:30),
                      no   = paste0("v", 1:30)), 
         n = count(viewtime, video)$n) %>% 
  relocate(key, n, release, title)


# Encode titles -----------------------------------------------------------

viewtime <- inner_join(coding, viewtime, by = "title") %>%
  arrange(date, key) %>%  
  select(key, date, hours)


# Replace 0 by NA before release ------------------------------------------

for(k in coding$key) {
  k_rel <- coding %>% filter(key == k) %>% select(release) %>% pull()
  viewtime[viewtime$key == k & viewtime$date < k_rel, "hours"] <- NA_real_
}


# Create dummy variables --------------------------------------------------

data <- viewtime %>% 
  group_by(key) %>% 
  spread(key, hours) %>%
  mutate(
    y = year(date), 
    m = month(date),
    w = week(date),
    d = day(date),
    v_total = rowSums(across(starts_with("v")), na.rm = TRUE), 
    ws1 = as.numeric(m == 2), # WT exam A
    ws2 = as.numeric(m == 3), # WT exam B
    ss1 = as.numeric(m == 7), # ST exam A
    ss2 = as.numeric(m == 9), # ST exam B
    day = strftime(date,'%u'),
    mon = as.numeric(day == 1), 
    tue = as.numeric(day == 2),
    wed = as.numeric(day == 3),
    thu = as.numeric(day == 4),
    fri = as.numeric(day == 5),
    sat = as.numeric(day == 6),
    sun = as.numeric(day == 7),
    we  = as.numeric(sat | sun), 
    wd  = as.numeric(!we),
  ) %>%
  select(-day)


# Collect garbage ---------------------------------------------------------

rm(k, k_rel)
