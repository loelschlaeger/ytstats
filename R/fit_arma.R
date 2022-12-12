#' Fit an ARMA model
#' 
#' @description 
#' This function ...
#' 
#' @param ytdata
#' An object of class \code{\link{ytdata}}.
#' 
#' @return
#' An object of class \code{\link{ytfit}}.
#' 
#' @export 

fit_arma <- function(ytdata) {
  
  # Create encoding book ----------------------------------------------------
  
  # encoded variables v01, v02, ...
  encoding <- viewtime %>% 
    group_by(title) %>% 
    summarise(release = unique(release)) %>% 
    arrange(!is.na(release), release) %>%  
    mutate(key = paste0("v", 1:30), 
           n = count(viewtime, video)$n) %>% 
    relocate(key, n, release, title)
  
  
  # Encode titles -----------------------------------------------------------
  
  viewtime <- inner_join(encoding, viewtime, by = "title") %>%
    arrange(date, key) %>%  
    select(key, date, hours)
  
  
  # Replace 0 by NA before release ------------------------------------------
  
  for(k in encoding$key) {
    k_rel <- encoding %>% 
      filter(key == k) %>% 
      select(release) %>% 
      pull()
    viewtime[viewtime$key == k & viewtime$date < k_rel, "hours"] <- NA_real_
  }
  
  
  # Create dummy variables --------------------------------------------------
  
  data <- viewtime %>% 
    spread(key, hours) %>%
    mutate(
      y = year(date), 
      m = month(date),
      w = week(date),
      doy = yday(date), # day of year
      dow = wday(date, week_start = 1), # day of week, week starting on mondays
      dom = mday(date), # day of month
      h_per_v = rowSums(across(starts_with("v")), na.rm = TRUE) / rowSums(!is.na(across(starts_with("v")))),
      v_total = rowSums(across(starts_with("v")), na.rm = TRUE),
      wt_a = as.numeric(m == 1 | m == 2), # WT exam A
      wt_b = as.numeric(m == 3), # WT exam B
      st_a = as.numeric(m == 6 | m == 7), # ST exam A
      st_b = as.numeric(m == 9), # ST exam B
      mon = as.numeric(dow == 1), 
      tue = as.numeric(dow == 2),
      wed = as.numeric(dow == 3),
      thu = as.numeric(dow == 4),
      fri = as.numeric(dow == 5),
      sat = as.numeric(dow == 6),
      sun = as.numeric(dow == 7),
      we  = as.numeric(sat | sun), 
      wd  = as.numeric(!we),
      pandemic = date %within% lubridate::interval("2020-04-15", "2021-04-15") %>% as.numeric(),
      christmas = ( (christmas = (day(date) >= 24)) & (month(date) == 12) ) %>% as.numeric(),
      summer = (month(date) == 8) %>% as.numeric # August low
    )
  
  # Add upload effect dummy variables
  effect_length <- 2 # in weeks
  
  # Add end of effect date to encoding 
  encoding <- encoding %>% 
    mutate(end_of_effect = release + weeks(effect_length) - days(1)) %>% 
    relocate(key, n, release, end_of_effect, title)
  
  # create dummy if upload after first observation
  for (k in encoding$key){
    k_rel <- filter(encoding, key == k)$release
    k_eoe <- filter(encoding, key == k)$end_of_effect
    if (k_eoe >= min(data$date)){
      data[, gsub("v", "e", k)] <- data$date %within% lubridate::interval(k_rel, k_eoe) %>% as.numeric()
    }
  }
  
  # Long data ---------------------------------------------------------------
  
  data_long <- data %>% 
    select(date, y, m, w, doy, dow, dom, wt_a, wt_b, st_a, st_b, h_per_v, v1:v30) %>% 
    mutate(exam_period = factor(wt_a + wt_b + st_a + st_b,
                                levels = 0:1, 
                                labels = c("no", "yes"))) %>% 
    pivot_longer(c(h_per_v, v1:v30), names_to = "v_id", values_to = "hours")


  harmonics <- function(time_instants, K, period){
    SIN <- matrix(NA, length(time_instants), K)
    colnames(SIN) <- paste0("sin_", 1:K, "_by_", period)
    COS <- matrix(NA, length(time_instants), K)
    colnames(COS) <- paste0("cos_", 1:K, "_by_", period)
    for (k in 1:K){
      SIN[, k] <- sin(2 * (k / period) * pi * time_instants)
      COS[, k] <- cos(2 * (k / period) * pi * time_instants)
    }
    harmonics <- cbind(SIN, COS)
    return(harmonics)
  }
  
  deterministic_cycles <- function(arima_fit, 
                                   time_instants = NULL,
                                   K = c(),
                                   period = c()){
    # assumes arima(..., include.mean = TRUE)
    num_arma_parameters <- 1 + sum(!near(c(fit$model$theta, fit$model$phi), 0)) 
    cycle_ts <- vector("list", length(K))
    
    for (i in 1:length(K)){
      if (is.null(time_instants)){
        seasonal_cycle <- harmonics(1:period[i], K[i], period[i])
      } else if (is.vector(time_instants)){
        seasonal_cycle <- harmonics(time_instants, K[i], period[i])
      }
      
      bh <- arima_fit$coef[-(1:num_arma_parameters)]
      
      if (i == 1){
        bh <- bh[1:(2 * K[1])]
      } else {
        num_par_previous_K <- 2 * sum(K[1:(i-1)])
        num_par_current_K <- 2 * sum(K[1:i])
        bh <- bh[(num_par_previous_K + 1):num_par_current_K]
      }
      cycle_ts[[i]] <- drop(seasonal_cycle %*% bh)
    }
    return(cycle_ts)
  }
  
  data_arma <- data %>% 
    mutate(t = row_number(),
           exam_period = wt_a + wt_b + st_a + st_b) %>% 
    select(date, t, h_per_v, exam_period, pandemic, christmas, summer)
    
  
  xreg <- data_arma %>%
    select(t, exam_period, pandemic, christmas, summer) %>% 
    bind_cols(intercept = rep(1, nrow(data_arma)),
              harmonics(data_arma$t, K = 3, period = 7),
              harmonics(data_arma$t, K = 10, period = 365.25)) %>% 
    as.matrix()
  
  fit <- arima(data_arma$h_per_v,
               order = c(2,0,0),
               include.mean = FALSE,
               xreg = xreg,
               optim.control = list(maxit = 200))
  
  d_cycles <- deterministic_cycles(arima_fit = fit,
                                   time_instants = data_arma$t,
                                   K = c(3, 10),
                                   period = c(7, 365.25))
  #number of arma parameters w/o intercept
  num_arma_parameters <- sum(!near(c(fit$model$theta, fit$model$phi), 0))
  bh <- fit$coef[-(1:num_arma_parameters)]
  
  data_arma <- data_arma %>%
    bind_cols(arma_residuals = as.vector(fit$residuals),
              arma_fitted = as.vector(data$h_per_v - fit$residuals),
              arma_fitted_xreg = xreg %*% bh,
              intercept = fit$coef["intercept"],
              cycle_7d = d_cycles[[1]],
              cycle_365d = d_cycles[[2]]) %>%
    mutate(seasonal_cycle = intercept + cycle_7d + cycle_365d)

}
