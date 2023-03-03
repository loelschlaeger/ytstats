#' Plot method for a \code{ytdata} object.
#' 
#' @description 
#' This function is the plotting method for a \code{ytdata} object to visualize
#' the time series data.
#' 
#' @param x
#' An object of class \code{\link{ytdata}}.
#' @param column
#' A \code{character}, the column name of \code{x} to plot.
#' @param from
#' A \code{character} of the form \code{"YYYY-MM-DD"}, the lower date.
#' Can be \code{NA} for no lower bound.
#' @param to
#' A \code{character} of the form \code{"YYYY-MM-DD"}, the upper date.
#' Can be \code{NA} for no upper bound.
#' @param ...
#' Currently not used.
#' 
#' @return 
#' A \code{ggplot} object.
#' 
#' @exportS3Method
#' 
#' @importFrom ggplot2 ggplot aes geom_line scale_x_date theme_minimal
#' @importFrom rlang .data

plot.ytdata <- function (x, column, from = NA, to = NA, ...) { # test
  
  stopifnot(
    is.character(column), is.vector(column), length(column) == 1,
    column %in% colnames(x)
  )
  
  base_plot <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$day)) + 
    ggplot2::scale_x_date(
      limits = c(as.Date(from), as.Date(to))
    ) +
    ggplot2::geom_line(aes(y = .data[[column]])) +
    ggplot2::theme_minimal()
  
  return(base_plot)
  
}