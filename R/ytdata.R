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

