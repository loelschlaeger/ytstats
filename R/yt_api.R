# Read YouTube data -------------------------------------------------------

### authorization for Google Ads account
rgoogleads::gads_auth("oelschlaeger.lennart@gmail.com")

### get list of YouTube videos
videos <- rytstat::ryt_get_videos()

### get statistics from YouTube Analytics API
data <- rytstat::ryt_get_analytics(
  start_date = "2020-01-01", 
  end_date = "2022-09-01",
  ### https://developers.google.com/youtube/analytics/metrics
  metrics = c("views", "likes", "dislikes", "estimatedMinutesWatched"),
  ### https://developers.google.com/youtube/analytics/dimensions
  dimensions = c("day", "video"),
  filters = stringr::str_glue("video=={paste(videos$id_video_id, collapse=',')}")
)

### save data
saveRDS(data, "data/data.rds", compress = "xz")
