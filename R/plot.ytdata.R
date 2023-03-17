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

plot.ytdata <- function(x, column, ids = NULL, from = NULL, to = NULL, type = c("ts", "boxplot"), ...) {
  
  stopifnot(
    is.character(column), is.vector(column), length(column) == 1,
    column %in% colnames(x)
  )
  
  type = match.arg(type)
  
  # Subset plotting window
  if (is.null(from)) {
    from = x$day[1]
  } else {
    stopifnot(from %within% lubridate::interval(x$day[1], x$day[nrow(x)]))
  }
  
  if (is.null(to)) {
    to = x$day[nrow(x)]
  } else {
    stopifnot(to %within% lubridate::interval(x$day[1], x$day[nrow(x$day)]))
  }
  
  x <- x %>% filter(lubridate::as_datetime(day) %within% lubridate::interval(from, to))
  
  # Subset ids
  if (is.null(ids)){
    ids <- unique(x$id)
  } else {
    x <- x %>% filter(id %in% ids)
  }
  
  # Plot
  ytdata_plot <- switch(type,
                        ts = tsplot_ytdata(x, column, ids, ...),
                        boxplot = boxplot_ytdata(x, column, ids, ...)
  )
  
  return(ytdata_plot)
}

boxplot_ytdata <- function(x){
  x
}

tsplot_ytdata <- function(x, column, ids,
              f_col = NULL, f_id = c("mean", "sum"),
              month_coloring = c("line", "point")){
  f_id <- match.arg(f_id)
  month_coloring <- match.arg(month_coloring)
  
  if (is.null(f_col)){
    f_col <- function(z) z
  }
  
  colors <- brewer.pal(n = 12, name = 'Paired')
  months_abbr <- c("Jan", "Feb", "Mar", "Apr",
                   "May", "Jun", "Jul", "Aug",
                   "Sep", "Oct", "Nov", "Dec")
  names(colors) <- months_abbr
  colors["Nov"] <- "#DDA384"
  
  x_sub <- ytdata_example %>%
    filter(id %in% ids) %>% 
    select(day, id, y0 = !!column) %>%
    pivot_wider(names_from = id, 
                values_from = y0, 
                names_prefix = "v_") %>% 
    mutate(month = factor(lubridate::month(day), 
                          levels = 1:12,
                          labels = months_abbr)
           )
  
  x_sub <- switch(f_id,
                  mean = {
                    x_sub %>% mutate(ids_sum = rowSums(across(starts_with("v_")), na.rm = TRUE),
                                     y = ids_sum / rowSums(!is.na(across(starts_with("v_")))))
                  },
                  sum = x_sub %>% mutate(y = rowSums(across(starts_with("v_")), na.rm = TRUE)),
                  stop("Invalid value for `f_id`")
  )
  
  if (!is.null(f_col)) {
    x_sub <- x_sub %>% mutate(y = f_col(y))
  }
  
  tsplot <- switch(month_coloring,
                   line = {
                     x_sub %>% 
                      ggplot2::ggplot(aes(x = day, y = y, color = month, group = data.table::rleid(month))) +
                      ggplot2::scale_x_date() +
                      ggplot2::geom_line()
                   },
                   point = {
                     x_sub %>%  
                       ggplot2::ggplot() + 
                       ggplot2::geom_line(aes(x = day, y = y)) +
                       ggplot2::geom_point(aes(x = day, y = y, color = month))
                   },
                   stop("Invalid value for `month_coloring`")
  )
  
  tsplot <- tsplot + 
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_minimal()
  
  return(tsplot)
}