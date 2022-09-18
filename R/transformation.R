# Load libraries ----------------------------------------------------------

source("R/reading.R")
library("magrittr")
library("tidyr")


# Create encoding book ----------------------------------------------------

# encoded variables v01, v02, ...
encoding <- viewtime %>% 
  group_by(title) %>% 
  summarise(release = unique(release)) %>% 
  arrange(!is.na(release), release) %>%  
  mutate(key = paste0("v", 1:30), 
         n = count(viewtime, video)$n) %>% 
  relocate(key, n, release, title)


# Encode titles -----------------------------------------------------------

viewtime <- inner_join(encoding, viewtime, by = "title") %>%
  arrange(date, key) %>%  
  select(key, date, hours)


# Replace 0 by NA before release ------------------------------------------

for(k in encoding$key) {
  k_rel <- encoding %>% 
    filter(key == k) %>% 
    select(release) %>% 
    pull()
  viewtime[viewtime$key == k & viewtime$date < k_rel, "hours"] <- NA_real_
}


# Create dummy variables --------------------------------------------------

data <- viewtime %>% 
  spread(key, hours) %>%
  mutate(
    y = year(date), 
    m = month(date),
    w = week(date),
    doy = yday(date), # day of year
    dow = wday(date, week_start = 1), # day of week, week starting on mondays
    dom = mday(date), # day of month
    h_per_v = rowSums(across(starts_with("v")), na.rm = TRUE) / rowSums(!is.na(across(starts_with("v")))),
    v_total = rowSums(across(starts_with("v")), na.rm = TRUE),
    wt_a = as.numeric(m == 1 | m == 2), # WT exam A
    wt_b = as.numeric(m == 3), # WT exam B
    st_a = as.numeric(m == 6 | m == 7), # ST exam A
    st_b = as.numeric(m == 9), # ST exam B
    mon = as.numeric(dow == 1), 
    tue = as.numeric(dow == 2),
    wed = as.numeric(dow == 3),
    thu = as.numeric(dow == 4),
    fri = as.numeric(dow == 5),
    sat = as.numeric(dow == 6),
    sun = as.numeric(dow == 7),
    we  = as.numeric(sat | sun), 
    wd  = as.numeric(!we),
    pandemic = date %within% lubridate::interval("2020-04-15", "2021-04-15") %>% as.numeric(),
    christmas = ( (christmas = (day(date) >= 24)) & (month(date) == 12) ) %>% as.numeric(),
    summer = (month(date) == 8) %>% as.numeric # August low
  )

# Add upload effect dummy variables
effect_length <- 2 # in weeks

# Add end of effect date to encoding 
encoding <- encoding %>% 
  mutate(end_of_effect = release + weeks(effect_length) - days(1)) %>% 
  relocate(key, n, release, end_of_effect, title)

# create dummy if upload after first observation
for (k in encoding$key){
  k_rel <- filter(encoding, key == k)$release
  k_eoe <- filter(encoding, key == k)$end_of_effect
  if (k_eoe >= min(data$date)){
    data[, gsub("v", "e", k)] <- data$date %within% lubridate::interval(k_rel, k_eoe) %>% as.numeric()
  }
}

# Long data ---------------------------------------------------------------

data_long <- data %>% 
  select(date, y, m, w, doy, dow, dom, wt_a, wt_b, st_a, st_b, h_per_v, v1:v30) %>% 
  mutate(exam_period = factor(wt_a + wt_b + st_a + st_b,
                              levels = 0:1, 
                              labels = c("no", "yes"))) %>% 
  pivot_longer(c(h_per_v, v1:v30), names_to = "v_id", values_to = "hours")


# Collect garbage ---------------------------------------------------------

rm(k, k_rel, effect_length, k_eoe)
