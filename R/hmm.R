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

sample_theta <- function(N, dist, x = NULL) {
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.null(x) || is.numeric(x))
  n_params <- N * (N - 1) + N + ifelse(dist == "poisson", 0, N)
  scale <- if (is.null(x)) {
    c(1, 10) 
  } else {
    c(mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
  }
  theta <- rep(NA_real_, n_params)
  Gamma <- matrix(stats::runif(N^2), N, N) + diag(N)
  Gamma <- Gamma / rowSums(Gamma)
  theta[1:((N - 1) * N)] <- Gamma[row(Gamma) != col(Gamma)]
  if (dist == "gaussian") {
    mu <- stats::rnorm(N, mean = scale[1], sd = scale[2])
    sigma <- stats::runif(N, 0, 1) * scale[2]
    theta[(N - 1) * N + 1:(2 * N)] <- c(mu, sigma)
  } else if (dist == "gamma") {
    mu <- abs(stats::rnorm(N, mean = scale[1], sd = scale[2]))
    sigma <- stats::runif(N, min = 0, max = 1) * scale[2]
    theta[(N - 1) * N + 1:(2 * N)] <- c(mu, sigma)
  } else if (dist == "poisson") {
    lambda <- stats::runif(N, min = 0, max = 1) * scale[2]
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

ll_hmm <- function(theta, x, N, dist = "gaussian", neg = FALSE) {
  stopifnot(is.numeric(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  stopifnot(isTRUE(neg) || isFALSE(neg))
  Gamma <- diag(N)
  Gamma[!Gamma] <- theta[1:((N - 1) * N)]
  Gamma <- Gamma / rowSums(Gamma)
  delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
  probs <- matrix(NA_real_, nrow = length(x), ncol = N)
  if (dist == "gaussian") {
    mu <- theta[(N - 1) * N + 1:N]
    sigma <- theta[(N - 1) * N + (N + 1):(2 * N)]
    for (j in 1:N) {
      probs[, j] <- stats::dnorm(x, mean = mu[j], sd = sigma[j])
    }
  } else if (dist == "gamma") {
    mu <- theta[(N - 1) * N + 1:N]
    sigma <- theta[(N - 1) * N + (N + 1):(2 * N)]
    for (j in 1:N) {
      probs[, j] <- stats::dgamma(
        x, shape = mu[j]^2 / sigma[j]^2, scale = sigma[j]^2 / mu[j]
      )
    }
    probs[x <= 0, ] <- 1
  } else if (dist == "poisson") {
    lambda <- theta[(N - 1) * N + 1:N]
    for (j in 1:N) {
      probs[, j] <- stats::dpois(x, lambda[j])
    }
    probs[x < 0, ] <- 1
  }
  probs[is.na(x), ] <- 1
  foo <- delta * probs[1, ]
  phi <- foo / sum(foo)
  ll <- log(sum(foo))
  for (t in 2:length(x)) {
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

separate_theta <- function(theta, N, dist) {
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  Gamma <- diag(N)
  Gamma[!Gamma] <- theta[1:((N - 1) * N)]
  Gamma <- Gamma / rowSums(Gamma)
  delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
  mean <- theta[(N - 1) * N + 1:N]
  if (identical(dist, "poisson")) {
    sd <- NULL
  } else {
    sd <- theta[(N - 1) * N + (N + 1):(2 * N)]
  }
  list(Gamma = Gamma, delta = delta, mean = mean, sd = sd)
}

#' Maximum likelihood estimation of an HMM
#'
#' @description 
#' Numerical optimization of the Hidden Markov model likelihood function via 
#' \code{\link[stats]{optim}}.
#' 
#' @inheritParams ll_hmm
#' @param runs
#' An \code{integer}, the number of randomly initialized optimization runs.
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
#' mle <- mle_hmm(x = x, N = N, dist = dist, runs = 5)
#' }
#'
#' @importFrom stats optim
#'
#' @export

mle_hmm <- function(x, N, dist, runs = 1) {
  stopifnot(is.numeric(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(length(runs) == 1, runs == as.integer(runs), N >= 1)
  if (dist == "gaussian") {
    lower <- c(rep(0, N * (N - 1)), rep(-Inf, N), rep(0, N))
  } else if (dist == "gamma") {
    lower <- rep(0, N * (N - 1) + 2 * N)
  } else if (dist == "poisson") {
    lower <- rep(0, N * (N - 1) + N)
  }
  mods <- list()
  for (run in 1:runs) {
    theta <- sample_theta(N = N, dist = dist, x = x)
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
      mods[[run]] <- out
    }
  }
  values <- lapply(mods, `[[`, "value")
  if (length(values) == 0) {
    stop("Estimation failed, consider increasing 'runs'.")
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
#' dist <- "gamma"
#' theta <- sample_theta(N = N, dist = dist)
#' x <- simulate_hmm(T = 100, N = N, theta = theta, dist = dist)
#' mle <- mle_hmm(x = x, N = N, dist = dist, runs = 5)
#' decode_states(x = x, theta = mle, dist = dist, N = N)
#' attr(x, "states")
#' }
#' 
#' @importFrom stats dgamma dnorm dpois
#' 
#' @export

decode_states <- function(x, theta, dist, N) {
  T <- length(x)
  par <- separate_theta(theta = theta, N = N, dist = dist)
  Gamma <- par$Gamma
  delta <- par$delta
  mean <- par$mean
  sd <- par$sd
  allprobs <- matrix(1, N, T)
  ### TODO: other sdds
  for (n in 1:N) {
    allprobs[n, ] <- stats::dgamma(
      x, shape = mean[n]^2 / sd[n]^2, scale = sd[n]^2 / mean[n]
    )
  }
  xi <- matrix(0, N, T)
  for (n in 1:N) {
    xi[n, 1] <- log(delta[n]) + log(allprobs[n, 1])
  }
  for (t in 2:T) {
    for (n in 1:N) {
      xi[n, t] <- max(xi[, t - 1] + log(Gamma[, n])) + log(allprobs[n, t])
    }
  }
  iv <- numeric(T)
  iv[T] <- which.max(xi[, T])
  for (t in rev(seq_len(T - 1))) {
    iv[t] <- which.max(xi[, t] + log(Gamma[, iv[t + 1]]))
  }
  return(iv)
  ### TODO: for identification, order states by mean
}
