#' Get statistics from YouTube Analytics API
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
#' A \code{tibble} with 6 columns and a row for each day and video:
#' \describe{
#'   \item{day}{the date}
#'   \item{id}{the video id}
#'   \item{views}{the number of views}
#'   \item{likes}{the number of likes}
#'   \item{dislikes}{the number of dislikes}
#'   \item{viewmins}{the estimated number of minutes watched}
#' }
#'
#' The output has the two attributes \code{"channel"} and \code{"videos"}.
#' - Attribute \code{"channel"} is a list with 5 elements:
#' \describe{
#'   \item{id}{the channel id}
#'   \item{title}{the channel title}
#'   \item{desc}{the channel description}
#'   \item{views}{the total number of views}
#'   \item{subs}{the total number of subscribers}
#' }
#' - Attribute \code{"videos"} is a \code{tibble} with 5 columns and a row for 
#' each video:
#' \describe{
#'   \item{id}{the video id}
#'   \item{pub}{the date and time of publication}
#'   \item{title}{the video title}
#'   \item{desc}{the video describtion}
#'   \item{tn}{the URL to the video thumbnail}
#' }
#' 
#' @importFrom cli cat_bullet
#' @importFrom rgoogleads gads_auth
#' @importFrom rytstat ryt_token
#' @importFrom gargle request_build request_retry response_process
#' @importFrom dplyr %>%
#' @importFrom dplyr %>% mutate select arrange
#'
#' @export

read_yt <- function(email, from = Sys.Date() - 10, to = Sys.Date()) {

  ### authorization for Google Ads account
  cli::cat_bullet("authorize account")
  rgoogleads::gads_auth(email = email)
  token <- rytstat::ryt_token()
  
  ### get channel meta data
  cli::cat_bullet("get channel meta data")
  channel <- list()
  repeat{
    out <- gargle::request_build(
      method = "GET", 
      params = list(
        mine = TRUE, 
        part = "snippet,statistics", 
        fields = NULL, 
        maxResults = 5
      ), 
      token = token, 
      path = "youtube/v3/channels", 
      base_url = "https://www.googleapis.com/"
    ) %>% 
      gargle::request_retry(encode = "json") %>%
      gargle::response_process()
    channel <- append(channel, list(out$items))
    if (is.null(out$nextPageToken)) {
      break
    }
  }
  channel <- list(
    "id"    = channel$snippet$customUrl,                     # unique channel id
    "title" = channel$snippet$title,                         # channel title
    "desc"  = channel$snippet$description,                   # channel description
    "publ"  = channel$snippet$publishedAt,                   # publication date
    "views" = as.numeric(channel$statistics$viewCount),      # total views
    "subs"  = as.numeric(channel$statistics$subscriberCount) # total subscribers
  )

  ### get list of YouTube videos
  cli::cat_bullet("get list of videos")
  videos <- list()
  repeat{
    out <- request_build(
      method = "GET", 
      params = list(
        part = "snippet", 
        forMine = TRUE, 
        type = "video", 
        maxResults = 50
      ), 
      token = token, 
      path = "youtube/v3/search", 
      base_url = "https://www.googleapis.com/"
    ) %>%
      gargle::request_retry(encode = "json") %>%
      gargle::response_process()
    videos <- append(videos, list(out$items))
    if (is.null(out$nextPageToken)) {
      break
    }
  }
  videos <- tibble(items = videos) %>% 
    unnest_longer(.data$items) %>% 
    unnest_wider(.data$items) %>% 
    unnest_wider(.data$id, names_sep = "_") %>% 
    unnest_wider(.data$snippet) %>% 
    unnest_wider(.data$thumbnails, names_sep = "_") %>% 
    unnest_wider(.data$thumbnails_default, names_sep = "_") %>% 
    unnest_wider(.data$thumbnails_medium, names_sep = "_") %>% 
    unnest_wider(.data$thumbnails_high, names_sep = "_") %>% 
    unnest_wider(.data$thumbnails_standard, names_sep = "_") %>% 
    unnest_wider(.data$thumbnails_maxres, names_sep = "_") %>%
    dplyr::select(
      id = id_videoId,                     # unique video id
      pub = publishedAt,                   # day and time of video release
      title = title,                       # video title
      desc = description,                  # video description
      tn = thumbnails_standard_url         # url to video thumbnail
    ) %>% 
    dplyr::mutate(
      pub = as.POSIXct(pub, format="%Y-%m-%dT%H:%M:%SZ"),
    ) %>%
    dplyr::arrange(pub)                    # in order of video release
  
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
  structure(data, "channel" = channel, "videos" = videos)
}
