library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("tsibble")
library("lubridate")

viewtime_v1 <- read.csv("data/viewtime_by_video1.csv", header = TRUE)
viewtime_v2 <- read.csv("data/viewtime_by_video2.csv", header = TRUE)
viewtime_v3 <- read.csv("data/viewtime_by_video3.csv", header = TRUE)

viewtime <- rbind(viewtime_v1, viewtime_v2, viewtime_v3)
colnames(viewtime) <- c("date","video", "title", "date_upload", "hours")
viewtime <- viewtime %>% as_tibble()
viewtime$date <- ymd(viewtime$date)
viewtime$date_upload <- mdy(viewtime$date_upload)
viewtime <- viewtime %>% arrange(!is.na(date_upload), date_upload, date)

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

upload_dates <- unique(viewtime$date_upload)
length(titles) == length(upload_dates) # check whether multiple videos were uploaded on a single date, whether there are NA upload dates

videos_per_upload_date <- vector("numeric", length(upload_dates))

for (i in seq_along(upload_dates)){
  videos_per_upload_date[i] <- viewtime %>%
    filter(date_upload == upload_dates[i]) %>% 
    nrow()
}

upload_dates <- c(upload_dates[1:4], rep(upload_dates[5], 3), upload_dates[6:28])
coding <- tibble(key = paste0("v", 1:30), 
                 n = count(viewtime, video)$n,
                 date_upload = upload_dates, 
                 titles
                 )

viewtime_spread <- viewtime %>% 
  select(-date_upload, -video) %>% 
  group_by(title) %>% 
  spread(title, hours)

vt <- viewtime_spread %>%
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
        ws1 = as.numeric(m==2), # WS Klausurtermin 1
        ws2 = as.numeric(m==3), # WS Klausurtermin 2
        ss1 = as.numeric(m==7), # SoSe Klausurtermin 1
        ss2 = as.numeric(m==9), # SoSe Klausurtermin 2
        mo = as.numeric(d==3), 
        tu = as.numeric(d==4),
        we = as.numeric(d==5),
        th = as.numeric(d==6),
        fr = as.numeric(d==7),
        sa = as.numeric(d==1),
        su = as.numeric(d==2),
        we = as.numeric(d %in% c(1,2)), # weekend, Wochenende
        wd = as.numeric(!(d %in% c(1,2))) # weekdays, Werktage (ohne Samstag)
        )

vt <- vt %>% relocate(date, y, m, w, d, ws1, ws2, ss1, ss2, 
                      mo, tu, we, th, fr, sa, su, we, wd,
                      v_total, .before = v01)

vt <- vt %>%
  mutate(v18 = replace(v18, date < coding$date_upload[18], NA))
vt <- vt %>%
  mutate(v19 = replace(v19, date < coding$date_upload[19], NA))
vt <- vt %>%
  mutate(v20 = replace(v20, date < coding$date_upload[20], NA))
vt <- vt %>%
  mutate(v21 = replace(v21, date < coding$date_upload[21], NA))
vt <- vt %>%
  mutate(v22 = replace(v22, date < coding$date_upload[22], NA))
vt <- vt %>%
  mutate(v23 = replace(v23, date < coding$date_upload[23], NA))
vt <- vt %>%
  mutate(v24 = replace(v24, date < coding$date_upload[24], NA))
vt <- vt %>%
  mutate(v25 = replace(v25, date < coding$date_upload[25], NA))
vt <- vt %>%
  mutate(v26 = replace(v26, date < coding$date_upload[26], NA))
vt <- vt %>%
  mutate(v27 = replace(v27, date < coding$date_upload[27], NA))
vt <- vt %>%
  mutate(v28 = replace(v28, date < coding$date_upload[28], NA))
vt <- vt %>%
  mutate(v29 = replace(v29, date < coding$date_upload[29], NA))
vt <- vt %>%
  mutate(v30 = replace(v30, date < coding$date_upload[30], NA))

# Check whether NAs were correctly set
# Example v19
max(which(is.na(vt$v19)))
vt[89,]
coding[19,]

# Example v25
max(which(is.na(vt$v25)))
vt[232,]
vt[232,"v25"]
coding[25,]


ts_vt <- tsibble(vt)
plot(tsibble(vt$v_total))
plot.ts(vt$v06)  























