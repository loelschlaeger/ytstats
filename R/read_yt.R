#' Get data from YouTube Analytics API
#'
#' @description 
#' Use this function to download YouTube Analytics API data.
#' 
#' @param email 
#' A \code{character}, the email of the Google account.
#' @param from 
#' A \code{character} in \code{"YYYY-MM-DD"} format, the start date for 
#' fetching YouTube Analytics data.
#' By default, \code{Sys.Date() - 10}.
#' @param to
#' A \code{character} in \code{"YYYY-MM-DD"} format, the end date for 
#' fetching YouTube Analytics data.
#' By default, \code{Sys.Date()}.
#'
#' @return
#' An object of class \code{\link{ytdata}}.
#' 
#' @importFrom cli cat_bullet
#' @importFrom rgoogleads gads_auth
#' @importFrom rytstat ryt_token
#' @importFrom gargle request_build request_retry response_process
#' @importFrom dplyr %>%
#' @importFrom dplyr %>% mutate select arrange
#' @importFrom tidyr unnest_longer unnest_wider
#' @importFrom tibble as_tibble
#'
#' @export

read_yt <- function(email, from = Sys.Date() - 10, to = Sys.Date()) {

  ### authorization for Google Ads account
  cli::cat_bullet("authorize account")
  rgoogleads::gads_auth(email = email)
  token <- rytstat::ryt_token()
  
  ### get channel meta data
  cli::cat_bullet("get channel meta data")
  suppressMessages({
    channel <- rytstat::ryt_get_channels() %>%
      dplyr::mutate(
        id = id,                             # unique channel id
        title = title,                       # channel title
        desc = description,                  # channel description
        publ = as.Date(published_at),        # publication date
        views = as.integer(view_count),      # total views
        subs = as.integer(subscriber_count), # total subscribers
        .keep = "none"
      ) %>% 
      as.list()
  }, "cliMessage")

  ### get list of YouTube videos
  cli::cat_bullet("get list of videos")
  suppressMessages({
    videos <- rytstat::ryt_get_videos() %>%
      dplyr::select(
        id = id_video_id,                    # unique video id
        pub = published_at,                  # day and time of video release
        title = title,                       # video title
        desc = description,                  # video description
        tn = thumbnails_standard_url         # url to video thumbnail
      ) %>% 
      dplyr::mutate(
        pub = as.POSIXct(pub, format = "%Y-%m-%dT%H:%M:%SZ"),
      ) %>%
      dplyr::arrange(pub)                    # in order of video release
  }, "cliMessage")
  
  ### get statistics from YouTube Analytics API
  cli::cat_bullet("get video statistics")
  query <- gargle::request_build(
    method = "GET", 
    params = list(
      startDate = from, 
      endDate = to, 
      ids = "channel==MINE", 
      ### https://developers.google.com/youtube/analytics/dimensions
      dimensions = "day,video", 
      filters = stringr::str_glue("video=={paste(videos$id, collapse=',')}"), 
      ### https://developers.google.com/youtube/analytics/metrics
      metrics = "views,likes,dislikes,estimatedMinutesWatched"
    ), 
    token = ryt_token(), 
    path = "v2/reports", 
    base_url = "https://youtubeanalytics.googleapis.com/"
  ) %>% 
    gargle::request_retry(encode = "json") %>%
    gargle::response_process()
  headers <- query$columnHeaders %>% sapply(`[[`, "name")
  data <- query$rows %>% 
    unlist() %>% 
    matrix(ncol = length(headers), byrow = TRUE) %>%
    data.frame() %>%
    rlang::set_names(headers) %>%
    dplyr::select(
      day = day,                         # the day
      id = video,                        # video id
      views = views,                     # number of views               
      likes = likes,                     # number of likes
      dislikes = dislikes,               # number of dislikes
      viewmins = estimatedMinutesWatched # number of minutes watched
    ) %>% 
    dplyr::mutate(
      day = as.Date(day, format = "%Y-%m-%d"),
      views = as.integer(views),
      likes = as.integer(likes),
      dislikes = as.integer(dislikes),
      viewmins = as.numeric(viewmins)
    ) %>%
    dplyr::arrange(day) %>%                # in order of day
    tibble::as_tibble()                    # transform to tibble
    
  ### build and return 'ytdata' object
  ytdata(data = data, channel = channel, videos = videos)
}
