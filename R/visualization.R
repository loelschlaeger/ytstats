#' Plot
#' 
#' @exportS3Method
#' 
#' @import ggplot2

plot.ytdata <- function (x, columns, ...) {
  
  statistics <- x
  
  base_plot <- statistics %>% ggplot(aes(x = day)) + 
    geom_vline(aes(xintercept = day), color = "red") +
    theme_minimal()
  
  # total viewtime
  base_plot <- base_plot + geom_line(aes(y = viewmins))
  
  print(base_plot)
}



plot.ytmodel <- function () {

  # Plot individual video boxplots ------------------------------------------
  
  data_long %>% 
    mutate(v_id = factor(v_id, levels = c("h_per_v", paste0("v", 1:30)))) %>% 
    ggplot(aes(x = v_id, y = hours)) +
    geom_boxplot(na.rm = TRUE) +
    theme_minimal()
  
  
  # Plot cumulated video statistics -----------------------------------------
  
  base_plot <- statistics %>% ggplot(aes(x = date)) + 
    geom_vline(aes(xintercept = date), data = filter(statistics, videos != 0), color = "red") +
    theme_minimal()
  
  # total viewtime
  base_plot + geom_line(aes(y = viewtime))
  
  # number of new subscribers
  base_plot + geom_line(aes(y = subs))
  
  
  # Plot individual video statistics ----------------------------------------
  
  data_long %>% 
    filter(v_id %in% c("v1", "v10", "v20")) %>% 
    mutate(Video = factor(v_id)) %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = hours, color = Video), na.rm = TRUE) +
    theme_minimal()
  
  
  # Annual cycle ------------------------------------------------------------
  
  colors <- brewer.pal(n = 12, name = 'Paired')
  months_abbr  <- c("Jan", "Feb", "Mar", "Apr",
                    "May", "Jun", "Jul", "Aug",
                    "Sep", "Oct", "Nov", "Dec")
  names(colors) <- months_abbr
  colors["Nov"] <- "#DDA384"
  
  # Non-aggregated: Line black, points colored according to months (simple)
  
  data %>%
    select(date, Month = m, hours = v2) %>% 
    mutate(Month = factor(Month, levels = 1:12, labels = months_abbr)) %>% 
    ggplot() + 
    geom_line(aes(x = date, y = hours)) +
    geom_point(aes(x = date, y = hours, color = Month)) +
    scale_color_manual(values = colors)
  
  # Line coloured according to month
  
  data %>% 
    mutate(month = factor(m, levels = 1:12, labels = months_abbr[1:12])) %>%
    select(date, month, hours = v2) %>% 
    ggplot(aes(x = date, y = hours, color = month, group = data.table::rleid(month))) +
    scale_x_date() +
    geom_line() +
    scale_color_manual(values = colors)
  
  # Plotting aggregates (monthly)
  
  data_long %>%
    group_by(v_id, y, m) %>%
    summarize(monthly_hours = sum(hours), time_var = y + (m - 1) / 12, .groups = "drop") %>% 
    filter(v_id == "h_per_v") %>% 
    ggplot(aes(x = time_var)) +
    geom_line(aes(y = monthly_hours), size = 1.1) +
    theme_minimal()
  
  # Boxplotting months
  
  data_long %>% 
    filter(v_id == "h_per_v") %>% 
    select(Month = m, hours) %>% 
    mutate(Month = factor(Month, levels = 1:12, labels = months_abbr)) %>% 
    ggplot(aes(x = Month, y = hours, fill = Month)) + 
    geom_boxplot() +
    scale_fill_manual(values = colors)
  
  # Plotting aggregates (weekly)
  
  # Barplotting hours per video taking into account growth due to a larger number of videos
  
  data %>% 
    select(date, h_per_v) %>% 
    group_by(week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%
    summarize(weekly_hours = sum(h_per_v), Month = month(week), .groups = "drop") %>% 
    mutate(Month = factor(Month, levels = 1:12, labels = months_abbr)) %>% 
    ggplot(aes(x = week, y = weekly_hours)) +
    geom_bar(aes(fill = Month), stat = "identity") + 
    scale_x_date(date_labels = "%m-%Y") +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
    labs(y = "weekly hours (average over videos)")
  
  # upward trend at the start of the semester until peak in exam period,
  # steeper trend reversal until the end of the term
  # st: April; Peak in July, then trend reversal, continuity broken by summer break in Aug
  # st: therefore minor peak in September (Retake exam)
  # wt: October; Peak end of January to mid February,
  # wt: trend reversal until reaching trough in beginning of April, then pattern is repeating
  
  # Weekly cycle ------------------------------------------------------------
  
  # Boxplot
  
  days_abbr <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  
  data_long %>% 
    filter(v_id == "h_per_v") %>% 
    select(day_of_week = dow, hours) %>% 
    mutate(day_of_week = factor(day_of_week, levels = 1:7, labels = days_abbr)) %>% 
    ggplot(aes(x = day_of_week, y = hours, fill = day_of_week)) + 
    geom_boxplot()
  
  # Exam period vs. non-exam period
  
  data_long2 <- data %>% 
    mutate(exam_period = factor(wt_a + wt_b + st_a + st_b,
                                levels = 0:1, 
                                labels = c("no", "yes"))) %>% 
    select(date, exam_period, v1:v30, h_per_v) %>% 
    pivot_longer(c(v1:v30, h_per_v), names_to = "v_id", values_to = "hours") %>% 
    mutate(v_id = factor(v_id, levels = c("h_per_v", paste0("v", 1:30))))
  
  # Boxplot individual video
  
  data_long2 %>% 
    filter(v_id == "h_per_v") %>% 
    mutate(day_of_week = factor(wday(date, week_start = 1), levels = 1:7, labels = days_abbr)) %>% 
    ggplot() +
    geom_boxplot(aes(x = day_of_week, y = hours, fill = day_of_week)) + 
    facet_wrap(~ exam_period, labeller = label_both)
  
  # weekly cycles by video and for h_per_v
  
  data_long2 %>% 
    group_by(exam_period, v_id, day_of_week = lubridate::wday(date, week_start = 1)) %>% 
    summarize(daily_average = mean(hours, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = day_of_week)) +
    geom_line(aes(y = daily_average, color = exam_period), size = 1.1) +
    facet_wrap(~ v_id, scales = "free")
  
  # Average weekly cycles by month
  
  data_long %>% 
    filter(v_id == "h_per_v") %>% 
    select(date, month = m, hours) %>% 
    mutate(day_of_week = wday(date, week_start = 1)) %>% 
    group_by(month, day_of_week) %>% 
    summarize(mean_hours_per_day = mean(hours), .groups = "drop") %>% 
    ggplot(aes(x = day_of_week)) + 
    geom_line(aes(y = mean_hours_per_day), size = 1.1) +
    facet_wrap(~ factor(month), scale = "free", labeller = label_both)
  
  
  # Plot seasonal subseries: all monthly subseries in one plot --------------
  
  data %>% 
    select(date, v1) %>% 
    mutate(date = floor_date(date, unit = "month")) %>% 
    group_by(date) %>% 
    summarize(monthly_totals = sum(v1), .groups = "drop") %>% 
    mutate(month = factor(month(date),
                          levels = 1:12,
                          labels = months_abbr),
           date_align_year = floor_date(date, unit = "year")) %>% 
    ggplot(aes(x = date_align_year, y = monthly_totals)) +
    geom_line(aes(colour = month), size = 1.1) +
    geom_text(aes(label = month, color = month))
  
  
  # Plot yearly comparison --------------------------------------------------
  
  # All years in one plot: months
  
  data %>% 
    select(date, hours = h_per_v) %>% 
    mutate(date = floor_date(date, unit = "month")) %>% 
    group_by(date) %>% 
    summarize(monthly_totals = sum(hours), .groups = "drop") %>% 
    mutate(month = month(date),
           year  = factor(year(date))) %>% 
    ggplot(aes(x = month, y = monthly_totals, colour = year)) +
    geom_line(size = 1.1) +
    geom_text(aes(label = month)) +
    scale_x_discrete(labels = months_abbr, limits = factor(1:12))
  
  # All years in one plot: weeks
  
  data %>% 
    select(date, hours = h_per_v) %>% 
    mutate(date = floor_date(date, unit = "week", week_start = 1)) %>% 
    group_by(date) %>% 
    summarize(weekly_totals = sum(hours), .groups = "drop") %>% 
    mutate(week = week(date),
           year = factor(year(date))) %>% 
    ggplot(aes(x = week, y = weekly_totals, colour = year)) +
    geom_line(size = 1.1) +
    geom_text(aes(label = week)) 
  
  
  # Arma models -------------------------------------------------------------
  
  data_arma %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = h_per_v)) +
    geom_line(aes(y = arma_fitted), col = "red")
  
  data_arma %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = h_per_v)) +
    geom_line(aes(y = arma_fitted_xreg), color = "red")
  
  # Visualise seasonal cycles
  # 7 day cycle
  data_arma %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = cycle_7d))
  
  # 365 day cycle
  data_arma %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = cycle_365d))
  
  data_arma %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = seasonal_cycle))
  
  # One complete cycle only
  data_arma %>% 
    filter(date %within% lubridate::interval("2020-02-01", "2020-02-07")) %>% 
    ggplot(aes(x = date)) + 
    geom_line(aes(y = cycle_7d))
  
  data_arma %>% 
    filter(date %within% lubridate::interval("2020-02-01", "2021-01-31")) %>% 
    ggplot(aes(x = date)) + 
    geom_line(aes(y = cycle_365d))
  
  # Residuals diagnostic plots --------------------------------------------------------
  
  data_arma %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = arma_residuals)) +
    geom_hline(yintercept = 0)
  
  # Autocorrelation
  forecast::ggAcf(x = data_arma$arma_residuals) + theme_minimal()
  
  # Normality
  # QQ-Plot against normal distribution
  data_arma %>% 
    ggplot(aes(sample = arma_residuals)) +
    geom_qq() +
    geom_qq_line()
  
  # Histogram and density against normal pdf
  data_arma %>% 
    ggplot(aes(x = arma_residuals)) +
    geom_histogram(aes(y = ..density..), bins = 50) +
    geom_density(color = "red", size = 1) +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(data_arma$arma_residuals),
                  sd = sd(data_arma$arma_residuals)), 
      lwd = 1, 
      col = 'lightblue'
    ) +
    theme_minimal()
  
}
