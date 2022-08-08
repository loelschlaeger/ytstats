library("dplyr")
library("lubridate")
library("magrittr")
library("tidyr")

# Load data of view time per video
viewtime_v1 <- read.csv("data/viewtime_by_video1.csv", header = TRUE)
viewtime_v2 <- read.csv("data/viewtime_by_video2.csv", header = TRUE)
viewtime_v3 <- read.csv("data/viewtime_by_video3.csv", header = TRUE)

# Combine into one data.frame
viewtime <- rbind(viewtime_v1, viewtime_v2, viewtime_v3)
colnames(viewtime) <- c("date", "video", "title", "release", "hours")
viewtime <- viewtime %>% as_tibble()
viewtime$date <- ymd(viewtime$date)
viewtime$release <- mdy(viewtime$release)
viewtime <- viewtime %>% 
  mutate(release = replace(release, is.na(release), "2020-01-12")) # Release date of "Hi"-video missing, filled in here 
viewtime <- viewtime %>% arrange(!is.na(release), release, date) # show NA first, then chronologically by release

# Create coding "book": encoded variables v01, v02, ... can be traced back
coding_df <- viewtime %>% 
  group_by(title) %>% 
  summarise(release = unique(release)) %>% # Attention: breaks arrangement from above
  arrange(!is.na(release), release) %>%  # Therefore: arrange in chronological order anew
  mutate(key = ifelse(1:30 < 10,
                      yes  = paste0("v0", 1:30),
                      no   = paste0("v", 1:30)), 
         n = count(viewtime, video)$n) %>% 
  relocate(key, n, release, title)

# Abbreviate titles to be used as variable names further below
titles <- unique(viewtime$title)
for (i in seq_along(titles)){
  if (i < 10){
    viewtime <- viewtime %>% 
      mutate(title = replace(title, title == titles[i], paste0("v0",i)))  
  } else {
    viewtime <- viewtime %>% 
      mutate(title = replace(title, title == titles[i], paste0("v",i)))  
  }
}

# Spread out rows to columns
viewtime_spread <- viewtime %>% 
  select(-release, -video) %>% 
  group_by(title) %>% 
  spread(title, hours)

# Create dummy variables
XY <- viewtime_spread %>%
  mutate(y = year(date), 
        m = month(date),
        w = week(date),
        d = day(date),
        v_total = v01 + v02 + v03 + v04 + v05 +
          v06 + v07 + v08 + v09 + v10 +
          v11 + v12 + v13 + v14 + v15 +
          v16 + v17 + v18 + v19 + v20 +
          v21 + v22 + v23 + v24 + v25 +
          v26 + v27 + v28 + v29 + v30,
        ws1 = as.numeric(m == 2), # WS Klausurtermin 1
        ws2 = as.numeric(m == 3), # WS Klausurtermin 2
        ss1 = as.numeric(m == 7), # SoSe Klausurtermin 1
        ss2 = as.numeric(m == 9), # SoSe Klausurtermin 2
        mon = as.numeric(d == 3), 
        tue = as.numeric(d == 4),
        wed = as.numeric(d == 5),
        thu = as.numeric(d == 6),
        fri = as.numeric(d == 7),
        sat = as.numeric(d == 1),
        sun = as.numeric(d == 2),
        we = as.numeric(d %in% c(1,2)), # weekend, Wochenende
        wd = as.numeric(!(d %in% c(1,2))),
        v18d = between(coding_df[18, "release"] + make_date(year, month, day))  # weekdays, Werktage (ohne Samstag)
        ) %>%
  relocate(date, y, m, w, d,
           ws1, ws2, ss1, ss2, 
           mon, tue, wed, thu, fri, sat, sun,
           we, wd,
           v_total,
           .before = v01)

# Replace _zero_ observations for videos that have not been uploaded with _NA_
# Then the date of the first non-NA entry is recognized as the upload date
XY <- XY %>%
  mutate(v18 = replace(v18, date < coding$release[18], NA))
XY <- XY %>%
  mutate(v19 = replace(v19, date < coding$release[19], NA))
XY <- XY %>%
  mutate(v20 = replace(v20, date < coding$release[20], NA))
XY <- XY %>%
  mutate(v21 = replace(v21, date < coding$release[21], NA))
XY <- XY %>%
  mutate(v22 = replace(v22, date < coding$release[22], NA))
XY <- XY %>%
  mutate(v23 = replace(v23, date < coding$release[23], NA))
XY <- XY %>%
  mutate(v24 = replace(v24, date < coding$release[24], NA))
XY <- XY %>%
  mutate(v25 = replace(v25, date < coding$release[25], NA))
XY <- XY %>%
  mutate(v26 = replace(v26, date < coding$release[26], NA))
XY <- XY %>%
  mutate(v27 = replace(v27, date < coding$release[27], NA))
XY <- XY %>%
  mutate(v28 = replace(v28, date < coding$release[28], NA))
XY <- XY %>%
  mutate(v29 = replace(v29, date < coding$release[29], NA))
XY <- XY %>%
  mutate(v30 = replace(v30, date < coding$release[30], NA))

# Check by example whether NAs were correctly set
# Example v19
max(which(is.na(XY$v19)))
XY[89,]
coding[19,]

# Example v25
max(which(is.na(XY$v25)))
XY[232,]
XY[232,"v25"]
coding[25,]
