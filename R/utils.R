#' Title
#' 
#' @description 
#' Description ...
#' 
#' @param arima_fit
#' ...
#' @param time_instants
#' ...
#' 
#' @return 
#' ...

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