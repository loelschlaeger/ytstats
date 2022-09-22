#' Get statistics from YouTube Analytics API
#'
#' @description 
#' Use this function to download YouTube Analytics API data.
#' 
#' @param email 
#' A character, the email of the Google account.
#' @param from 
#' The start date for fetching YouTube Analytics data, see details.
#' @param to
#' The end date for fetching YouTube Analytics data, see details.
#' 
#' @details
#' The values for \code{from} and \code{to} should be in YYYY-MM-DD format.
#'
#' @return
#' A \code{tibble} with 6 columns and a row for each day and video
#' \describe{
#'   \item{day}{the date}
#'   \item{id}{the video id}
#'   \item{views}{the number of views}
#'   \item{likes}{the number of likes}
#'   \item{dislikes}{the number of dislikes}
#'   \item{viewmins}{the estimated number of minutes watched}
#' }
#' The output has the two attributes \code{"channel"} and \code{"videos"}.
#'
#' Attribute \code{"channel"} is a list with 5 elements
#' \describe{
#'   \item{id}{the channel id}
#'   \item{title}{the channel title}
#'   \item{desc}{the channel description}
#'   \item{views}{the total number of views}
#'   \item{subs}{the total number of subscribers}
#' }
#'
#' Attribute \code{"videos"} is a \code{tibble} with 5 columns and a row for 
#' each video
#' \describe{
#'   \item{id}{the video id}
#'   \item{pub}{the date and time of publication}
#'   \item{title}{the video title}
#'   \item{desc}{the video describtion}
#'   \item{tn}{the URL to the video thumbnail}
#' }
#' 
#' @importFrom rgoogleads gads_auth
#' @importFrom rytstat ryt_get_channels ryt_get_videos ryt_get_analytics
#' @importFrom dplyr %>% mutate select arrange
#' @importFrom stringr str_glue
#' @importFrom cli cli_alert_info

read_yt <- function(email, from = Sys.Date(), to = from - 10) {

  ### authorization for Google Ads account
  cli::cli_h1("Authorization for Google Ads account")
  rgoogleads::gads_auth(email = email)
  
  ### get channel meta data
  cli::cli_h1("Get channel meta data")
  channel <- rytstat::ryt_get_channels() %>%
    dplyr::mutate(
      id = id,                             # unique channel id
      title = title,                       # channel title
      desc = description,                  # channel description
      views = as.numeric(view_count),      # total views
      subs = as.numeric(subscriber_count), # total subscribers
      .keep = "none"
    ) %>% 
    as.list()
  
  ### get list of YouTube videos
  cli::cli_h1("Get list of YouTube videos")
  videos <- rytstat::ryt_get_videos() %>%
    dplyr::select(
      id = id_video_id,                    # unique video id
      pub = published_at,                  # day and time of video release
      title = title,                       # video title
      desc = description,                  # video description
      tn = thumbnails_standard_url         # url to video thumbnail
    ) %>% 
    dplyr::mutate(
      pub = as.POSIXct(pub, format="%Y-%m-%dT%H:%M:%SZ"),
    ) %>%
    dplyr::arrange(pub)                    # in order of video release
  
  ### get statistics from YouTube Analytics API
  cli::cli_h1("Get YouTube statistics (can take very long)")
  data <- rytstat::ryt_get_analytics(
    start_date = from, 
    end_date = to,
    ### https://developers.google.com/youtube/analytics/metrics
    metrics = c("views", "likes", "dislikes", "estimatedMinutesWatched"),
    ### https://developers.google.com/youtube/analytics/dimensions
    dimensions = c("day", "video"),
    filters = stringr::str_glue("video=={paste(videos$id, collapse=',')}")
  ) %>%
    dplyr::select(
      day = day,                           # the day
      id = video,                          # video id
      views = views,                       # number of views               
      likes = likes,                       # number of likes
      dislikes = dislikes,                 # number of dislikes
      viewmins = estimatedMinutesWatched   # estimated number of minutes watched
    ) %>% 
    dplyr::mutate(
      day = as.Date(day, format="%Y-%m-%d"),
    ) %>%
    dplyr::arrange(day)                    # in order of day
  
  ### return data
  structure(data, channel = channel, videos = videos)
}
