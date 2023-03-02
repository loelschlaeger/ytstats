#' Constructor of a \code{ytdata} object
#' 
#' @description 
#' This function creates a \code{ytdata} object which contains YouTube data
#' for modeling.
#' 
#' @param data
#' A \code{tibble} with 6 columns and a row for each day and video:
#' \describe{
#'   \item{day}{the date (\code{Date})}
#'   \item{id}{the video id (\code{character})}
#'   \item{views}{the number of views (\code{integer})}
#'   \item{likes}{the number of likes (\code{integer})}
#'   \item{dislikes}{the number of dislikes (\code{integer})}
#'   \item{viewmins}{the number of minutes watched (\code{numeric})}
#' }
#' @param channel
#' A \code{list} with 5 elements:
#' \describe{
#'   \item{id}{the channel id (\code{character})}
#'   \item{title}{the channel title (\code{character})}
#'   \item{desc}{the channel description (\code{character})}
#'   \item{publ}{the channel publication date (\code{Date})}
#'   \item{views}{the total number of views (\code{integer})}
#'   \item{subs}{the total number of subscribers (\code{integer})}
#' }
#' @param videos
#' A \code{tibble} with 5 columns and a row for  each video:
#' \describe{
#'   \item{id}{the video id (\code{character})}
#'   \item{pub}{the date and time of publication (\code{POSIXct})}
#'   \item{title}{the video title (\code{character})}
#'   \item{desc}{the video description (\code{character})}
#'   \item{tn}{the URL to the video thumbnail (\code{character})}
#' }
#' 
#' @return 
#' An object of class \code{\link{ytdata}}, which is the input \code{data}
#' with two attributes \code{channel} and \code{videos}.

ytdata <- function(data, channel, videos) {
  structure(
    data, 
    "channel" = channel, 
    "videos" = videos,
    class = c("ytdata", class(data))
  )
}

#' @rdname ytdata
#' @param object
#' An object of class \code{\link{ytdata}}.
#' @param ...
#' Currently not used.
#' @exportS3Method 
#' @importFrom glue glue

summary.ytdata <- function(object, ...) {
  structure(
    list(
      "channel_title" = attr(object, "channel")$title,
      "total_views" = attr(object, "channel")$views,
      "total_subs" = attr(object, "channel")$subs,
      "number_videos" = nrow(attr(object, "videos")),
      "obs_start" = min(object$day),
      "obs_end" = max(object$day)
    ),
    class = "summary.ytdata"
  )
}

#' @noRd
#' @exportS3Method 
#' @importFrom glue glue

print.summary.ytdata <- function(x, ...) {
  cat(glue::glue(
    "Channel title: {x$channel_title}\n",
    "Total views: {x$total_views}\n",
    "Total subs: {x$total_subs}\n",
    "Number videos: {x$number_videos}\n",
    "First observation: {x$obs_start}\n",
    "Last observation: {x$obs_end}\n"
  ))
}

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
#' @importFrom dplyr %>% mutate select arrange
#' @importFrom tidyr unnest_longer unnest_wider
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
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
        id = .data$id,                             # unique channel id
        title = .data$title,                       # channel title
        desc = .data$description,                  # channel description
        publ = as.Date(.data$published_at),        # publication date
        views = as.integer(.data$view_count),      # total views
        subs = as.integer(.data$subscriber_count), # total subscribers
        .keep = "none"
      ) %>% 
      as.list()
  }, "cliMessage")
  
  ### get list of YouTube videos
  cli::cat_bullet("get list of videos")
  suppressMessages({
    videos <- rytstat::ryt_get_videos() %>%
      dplyr::select(
        id = .data$id_video_id,                 # unique video id
        pub = .data$published_at,               # day and time of video release
        title = .data$title,                    # video title
        desc = .data$description,               # video description
        tn = .data$thumbnails_standard_url      # URL to video thumbnail
      ) %>% 
      dplyr::mutate(
        pub = as.POSIXct(.data$pub, format = "%Y-%m-%dT%H:%M:%SZ"),
      ) %>%
      dplyr::arrange(.data$pub)                 # in order of video release
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
      day = .data$day,                           # the day
      id = .data$video,                          # video id
      views = .data$views,                       # number of views               
      likes = .data$likes,                       # number of likes
      dislikes = .data$dislikes,                 # number of dislikes
      viewmins = .data$estimatedMinutesWatched   # number of minutes watched
    ) %>% 
    dplyr::mutate(
      day = as.Date(.data$day, format = "%Y-%m-%d"),
      views = as.integer(.data$views),
      likes = as.integer(.data$likes),
      dislikes = as.integer(.data$dislikes),
      viewmins = as.numeric(.data$viewmins)
    ) %>%
    dplyr::arrange(.data$day) %>%                # in order of day
    tibble::as_tibble()                          # transform to tibble
  
  ### build and return 'ytdata' object
  ytdata(data = data, channel = channel, videos = videos)
}

