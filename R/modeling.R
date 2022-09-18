# Load libraries ----------------------------------------------------------

source("R/transformation.R")
library("forecast")


# ARMA model --------------------------------------------------------------

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


# Collect garbage ---------------------------------------------------------

rm(num_arma_parameters)
