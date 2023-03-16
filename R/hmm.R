#' Sample parameter vector for an HMM
#'
#' @description 
#' Sample the parameter vector \code{theta} for a Hidden Markov model.
#'
#' @details
#' If the observation vector \code{x} is supplied, means and standard deviations
#' are scaled by the first and second order data moments.
#'
#' @inheritParams ll_hmm
#' @param sort_states
#' Set to \code{TRUE} to sort the states by their mean.
#' 
#' @return 
#' A \code{numeric} parameter vector. 
#' The first \code{N*(N-1)} elements are the off-diagonal transition 
#' probabilities.
#' The next \code{N} elements are the means.
#' Only if \code{dist = "gaussian"} or \code{dist = "gamma"}, the next \code{N} 
#' elements are the standard deviations.
#'
#' @examples
#' \dontrun{
#' sample_theta(N = 3, dist = "gamma", x = 1:10)
#' }
#'
#' @importFrom stats sd runif rnorm

sample_theta <- function(N, dist, x = NULL, sort_states = TRUE) {
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.null(x) || is.numeric(x))
  stopifnot(isTRUE(sort_states) || isFALSE(sort_states))
  n_params <- N * (N - 1) + N + ifelse(dist == "poisson", 0, N)
  scale <- if (is.null(x)) {
    c(1, 2) 
  } else {
    c(mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
  }
  theta <- rep(NA_real_, n_params)
  Gamma <- matrix(stats::runif(N^2), N, N) + diag(N)
  Gamma <- Gamma / rowSums(Gamma)
  theta[1:((N - 1) * N)] <- Gamma[row(Gamma) != col(Gamma)]
  if (dist == "gaussian") {
    mu <- stats::rnorm(N, mean = scale[1], sd = scale[2])
    if (sort_states) mu <- sort(mu)
    sigma <- stats::runif(N, 0, 1) * scale[2]
    theta[(N - 1) * N + 1:(2 * N)] <- c(mu, sigma)
  } else if (dist == "gamma") {
    mu <- abs(stats::rnorm(N, mean = scale[1], sd = scale[2]))
    if (sort_states) mu <- sort(mu)
    sigma <- stats::runif(N, min = 0, max = 1) * scale[2]
    theta[(N - 1) * N + 1:(2 * N)] <- c(mu, sigma)
  } else if (dist == "poisson") {
    lambda <- stats::runif(N, min = 0, max = 1) * scale[2]
    if (sort_states) lambda <- sort(lambda)
    theta[(N - 1) * N + 1:N] <- lambda
  }
  return(theta)
}

#' Simulate HMM data
#' 
#' @description 
#' Simulate time series data from a pre-specified Hidden Markov model.
#' 
#' @param T
#' An \code{integer}, the time series length. Must be greater or equal 2.
#' @inheritParams ll_hmm
#'
#' @return
#' A \code{numeric} vector of length \code{T} with observations. The true state
#' sequence is available as attribute \code{"states"}.
#' 
#' @examples
#' \dontrun{
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' simulate_hmm(T = 20, N = N, theta = theta, dist = dist)
#' }
#'
#' @importFrom stats rgamma rpois rnorm

simulate_hmm <- function(T, N, theta, dist) {
  stopifnot(length(T) == 1, T == as.integer(T), T > 1)
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  par <- separate_theta(theta = theta, N = N, dist = dist)
  s <- rep(0, T)
  s[1] <- sample(1:N, size = 1, prob = par$delta)
  x <- rep(0, T)
  x[1] <- if (dist == "gaussian") {
    stats::rnorm(1, mean = par$mean[s[1]], sd = par$sd[s[1]])
  } else if (dist == "gamma") {
    shape <- par$mean[s[1]]^2 / par$sd[s[1]]^2
    scale <- par$sd[s[1]]^2 / par$mean[s[1]]
    stats::rgamma(1, shape = shape, scale = scale)
  } else if (dist == "poisson") {
    stats::rpois(1, lambda = par$mean[s[1]])
  } 
  for (t in 2:T) {
    s[t] <- sample(1:N, size = 1, prob = par$Gamma[s[t - 1], ])
    x[t] <- if (dist == "gaussian") {
      stats::rnorm(1, mean = par$mean[s[t - 1]], sd = par$sd[s[t - 1]])
    } else if (dist == "gamma") {
      shape <- par$mean[s[t - 1]]^2 / par$sd[s[t - 1]]^2
      scale <- par$sd[s[t - 1]]^2 / par$mean[s[t - 1]]
      stats::rgamma(1, shape = shape, scale = scale)
    } else if (dist == "poisson") {
      stats::rpois(1, lambda = par$mean[s[t - 1]])
    } 
  }
  structure(x, states = s)
}

#' HMM log-likelihood
#' 
#' @description 
#' Compute the log-likelihood of a Hidden Markov model with state-dependent 
#' Gaussian, Gamma, or Poisson distributions.
#'
#' @param theta 
#' A \code{numeric} parameter vector, see \code{\link{sample_theta}}.
#' @param x 
#' A \code{numeric} vector of observed data.
#' @param N 
#' An \code{integer}, the number of states. Must be greater or equal 2.
#' @param dist 
#' A \code{character} indicating the type of state-dependent distribution. 
#' Can be \code{"gaussian"}, \code{"gamma"}, or \code{"poisson"}.
#' @param neg
#' Set to \code{TRUE} to return the negative log-likelihood value.
#'
#' @return 
#' The log-likelihood of the observed data.
#'
#' @examples
#' \dontrun{
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' x <- c(1, 2, NA, 4, 5)
#' ll_hmm(theta = theta, x = x, N = N, dist = dist)
#' }
#'
#' @importFrom stats dnorm dgamma dpois

ll_hmm <- function(theta, x, N, dist, neg = FALSE) {
  stopifnot(is.numeric(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  stopifnot(isTRUE(neg) || isFALSE(neg))
  T <- length(x)
  Gamma <- diag(N)
  Gamma[!Gamma] <- theta[1:((N - 1) * N)]
  diag(Gamma) <- 2 - rowSums(Gamma)
  delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
  probs <- matrix(NA_real_, nrow = T, ncol = N)
  if (dist == "gaussian") {
    mu <- theta[(N - 1) * N + 1:N]
    sigma <- theta[(N - 1) * N + (N + 1):(2 * N)]
    for (n in 1:N) {
      probs[, n] <- stats::dnorm(x, mean = mu[n], sd = sigma[n])
    }
  } else if (dist == "gamma") {
    mu <- theta[(N - 1) * N + 1:N]
    sigma <- theta[(N - 1) * N + (N + 1):(2 * N)]
    for (n in 1:N) {
      probs[, n] <- stats::dgamma(
        x, shape = mu[n]^2 / sigma[n]^2, scale = sigma[n]^2 / mu[n]
      )
    }
    probs[x <= 0, ] <- 1
  } else if (dist == "poisson") {
    lambda <- theta[(N - 1) * N + 1:N]
    for (n in 1:N) {
      probs[, n] <- stats::dpois(x, lambda[n])
    }
    probs[x < 0, ] <- 1
  }
  probs[is.na(x), ] <- 1
  foo <- delta * probs[1, ]
  phi <- foo / sum(foo)
  ll <- log(sum(foo))
  for (t in 2:T) {
    foo <- phi %*% Gamma * probs[t, ]
    ll <- ll + log(sum(foo))
    phi <- foo / sum(foo)
  }
  ifelse(neg, -ll, ll)
}

#' Separate parameter vector
#' 
#' @description 
#' Separate parameter vector \code{theta} into transition probability matrix
#' \code{Gamma}, stationary distribution \code{delta}, mean vector \code{mean},
#' and standard deviations \code{sd} (not if \code{dist = "poisson"}).
#' 
#' @inheritParams ll_hmm
#' @inheritParams sample_theta
#' 
#' @return 
#' A \code{list} of parameters.
#'
#' @examples
#' \dontrun{
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' separate_theta(theta = theta, N = N, dist = dist)
#' }

separate_theta <- function(theta, N, dist, sort_states = TRUE) {
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  stopifnot(isTRUE(sort_states) || isFALSE(sort_states))
  Gamma <- diag(N)
  Gamma[!Gamma] <- theta[1:((N - 1) * N)]
  diag(Gamma) <- 2 - rowSums(Gamma)
  delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
  mean <- theta[(N - 1) * N + 1:N]
  if (identical(dist, "poisson")) {
    sd <- NULL
  } else {
    sd <- theta[(N - 1) * N + (N + 1):(2 * N)]
  }
  if (sort_states) {
    permut <- diag(N)[order(mean), ]
    Gamma <- permut %*% Gamma %*% t(permut)
    mean <- as.vector(permut %*% mean)
    if (!is.null(sd)) sd <- as.vector(permut %*% sd)
  }
  list(Gamma = Gamma, delta = delta, mean = mean, sd = sd)
}

#' Maximum likelihood estimation of an HMM
#'
#' @description 
#' Numerical optimization of the Hidden Markov model likelihood function via 
#' \code{\link[stats]{optim}}. Optimization is initialized randomly, at least 
#' \code{min_runs} times. Afterwards, the best run is selected. If no run yet
#' converged, another run is tried, until one converges or \code{max_runs} is
#' reached.
#' 
#' @inheritParams ll_hmm
#' @param min_runs
#' An \code{integer}, the minimum number of randomly initialized optimization 
#' runs.
#' @param max_runs
#' An \code{integer}, the maximum number of randomly initialized optimization 
#' runs.
#' 
#' @return
#' The estimated parameter vector.
#' 
#' @examples
#' \dontrun{
#' N <- 2
#' dist <- "gaussian"
#' theta <- sample_theta(N = N, dist = dist)
#' x <- simulate_hmm(T = 100, N = N, theta = theta, dist = dist)
#' mle <- mle_hmm(x = x, N = N, dist = dist, min_runs = 5)
#' }
#'
#' @importFrom stats optim
#'
#' @export

mle_hmm <- function(x, N, dist, min_runs = 1, max_runs = min_runs * 2) {
  stopifnot(is.numeric(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(length(min_runs) == 1, min_runs == as.integer(min_runs))
  stopifnot(length(max_runs) == 1, max_runs == as.integer(max_runs))
  stopifnot(min_runs >= 1, max_runs >= min_runs)
  if (dist == "gaussian") {
    lower <- c(rep(0, N * (N - 1)), rep(-Inf, N), rep(0, N))
  } else if (dist == "gamma") {
    lower <- rep(0, N * (N - 1) + 2 * N)
  } else if (dist == "poisson") {
    lower <- rep(0, N * (N - 1) + N)
  }
  converged <- 0
  mle_hmm_single <- function() {
    theta <- sample_theta(N = N, dist = dist, x = x, sort_states = TRUE)
    out <- suppressWarnings(
      try(
        stats::optim(
          par = theta, fn = ll_hmm, gr = NULL, x = x, N = N, dist = dist, 
          neg = TRUE, method = "L-BFGS-B", lower = lower, upper = Inf,
          control = list(), hessian = FALSE
        ),
        silent = TRUE
      )
    ) 
    if (!inherits(out, "try-error")) {
      converged <<- converged + 1
      out
    } else {
      NULL
    }
  }
  mods <- list()
  for (run in 1:max_runs) {
    mods[[run]] <- mle_hmm_single()
    if (converged > 0 && run >= min_runs) {
      break
    }
  }
  values <- lapply(mods, `[[`, "value")
  if (length(values) == 0) {
    stop("Estimation failed, consider increasing 'max_runs'.")
  }
  values[sapply(values, is.null)] <- NA
  ind <- which.min(unlist(values))
  mods[[ind]][["par"]]
}

#' Decode HMM state sequence
#' 
#' @description 
#' Global Viterbi decoding of the underlying state sequence of a fitted Hidden
#' Markov model.
#' 
#' @inheritParams ll_hmm
#' 
#' @return 
#' The decoded state sequence.
#' 
#' @examples
#' \dontrun{
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' x <- simulate_hmm(T = 1000, N = N, theta = theta, dist = dist)
#' mle <- mle_hmm(x = x, N = N, dist = dist, min_runs = 5)
#' decode_states(x = x, theta = mle, dist = dist, N = N)
#' attr(x, "states")
#' }
#' 
#' @importFrom stats dgamma dnorm dpois
#' 
#' @export

decode_states <- function(x, theta, dist, N) {
  stopifnot(is.numeric(x), is.numeric(theta))
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  T <- length(x)
  par <- separate_theta(theta = theta, N = N, dist = dist, sort_states = TRUE)
  Gamma <- par$Gamma
  delta <- par$delta
  mean <- par$mean
  sd <- par$sd
  probs <- matrix(NA_real_, nrow = T, ncol = N)
  if (dist == "gaussian") {
    for (n in 1:N) {
      probs[, n] <- stats::dnorm(x, mean = mean[n], sd = sd[n])
    }
  } else if (dist == "gamma") {
    for (n in 1:N) {
      probs[, n] <- stats::dgamma(
        x, shape = mean[n]^2 / sd[n]^2, scale = sd[n]^2 / mean[n]
      )
    }
  } else if (dist == "poisson") {
    for (n in 1:N) {
      probs[, n] <- stats::dpois(x, mean[n])
    }
  }
  cond <- matrix(0, nrow = T, ncol = N)
  for (n in 1:N) {
    cond[1, n] <- log(delta[n]) + log(probs[1, n])
  }
  for (t in 2:T) {
    for (n in 1:N) {
      cond[t, n] <- max(cond[t - 1, ] + log(Gamma[, n])) + log(probs[t, n])
    }
  }
  states <- numeric(T)
  states[T] <- which.max(cond[T, ])
  for (t in rev(seq_len(T - 1))) {
    states[t] <- which.max(cond[t, ] + log(Gamma[, states[t + 1]]))
  }
  return(states)
}
