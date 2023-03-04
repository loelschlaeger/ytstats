#' HMM log-likelihood
#' 
#' @description 
#' Compute the log-likelihood of a Hidden Markov Model with state-dependent 
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
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' x <- c(1, 2, NA, 4, 5)
#' ll_hmm(theta = theta, x = x, N = N, dist = dist)
#'
#' @importFrom stats dnorm dgamma dpois
#'
#' @export

ll_hmm <- function(theta, x, N, dist = "gaussian", neg = FALSE) {
  
  ### input checks
  stopifnot(is.numeric(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  stopifnot(isTRUE(neg) || isFALSE(neg))
  
  ### build probabilities
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
    mu <- exp(theta[(N - 1) * N + 1:N])
    sigma <- exp(theta[(N - 1) * N + (N + 1):(2 * N)])
    for (j in 1:N) {
      probs[, j] <- stats::dgamma(
        x, shape = mu[j]^2 / sigma[j]^2, scale = sigma[j]^2 / mu[j]
      )
    }
    probs[x <= 0, ] <- 1
  } else if (dist == "poisson") {
    lambda <- exp(theta[(N - 1) * N + 1:N])
    for (j in 1:N) {
      probs[, j] <- stats::dpois(x, lambda[j])
    }
    probs[x < 0, ] <- 1
  }
  
  ### compute likelihood from forward probabilities
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

#' Sample parameter vector
#'
#' @description 
#' Sample the parameter vector \code{theta} for an HMM.
#'
#' @details
#' If the observation vector \code{x} is supplied, means and standard deviations
#' are scaled by the first and second order data moments.
#'
#' @inheritParams ll_hmm
#' 
#' @return 
#' A \code{numeric} parameter vector. 
#' It is constrained if \code{constrain = TRUE} and unconstrained otherwise.
#' The first \code{N*(N-1)} elements are the off-diagonal transition 
#' probabilities.
#' The next \code{N} elements are the means.
#' Only if \code{dist = "gaussian"} or \code{dist = "gamma"}, the next \code{N} 
#' elements are the standard deviations.
#'
#' @examples
#' sample_theta(N = 3, dist = "gamma", x = 1:10)
#'
#' @importFrom stats sd runif rnorm
#'
#' @export

sample_theta <- function(N, dist, x = NULL) {
  
  ### input checks
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.null(x) || is.numeric(x))
  
  ### build theta
  n_params <- N * (N - 1) + N + ifelse(dist == "poisson", 0, N)
  scale <- if (is.null(x)) {
    c(1, 1) 
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
    sigma <- stats::runif(N, 0, 1) * scale[2]
    theta[(N - 1) * N + 1:(2 * N)] <- c(mu, sigma)
  } else if (dist == "poisson") {
    lambda <- abs(stats::rnorm(N, mean = scale[1], sd = scale[2]))
    theta[(N - 1) * N + 1:N] <- lambda
  }
  return(theta)
  
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
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' separate_theta(theta = theta, N = N, dist = dist)
#'
#' @export

separate_theta <- function(theta, N, dist) {
  
  ### input checks
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  
  ### separate theta
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
#' Numerical optimization of the HMM likelihood via \code{\link[stats]{optim}}.
#' 
#' @inheritParams ll_hmm
#' @param runs
#' An \code{integer}, the number of randomly initialized optimization runs.
#' 
#' @return
#' TODO
#' 
#' @examples
#' N <- 2
#' dist <- "gaussian"
#' theta <- sample_theta(N = N, dist = dist)
#' x <- simulate_hmm(T = 100, N = N, theta = theta, dist = dist)
#' mle_hmm(x = x, N = N, dist = dist, runs = 5)
#'
#' @importFrom stats optim
#'
#' @export

mle_hmm <- function(x, N, dist, runs = 1) {
  
  ### input checks
  stopifnot(is.numeric(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(length(runs) == 1, runs == as.integer(runs), N >= 1)
  
  ### define constraints
  if (dist == "gaussian") {
    lower <- c(rep(0, N * (N - 1)), rep(-Inf, N), rep(0, N))
  } else if (dist == "gamma") {
    lower <- rep(0, N * (N - 1) + 2 * N)
  } else if (dist == "poisson") {
    lower <- rep(0, N * (N - 1) + N)
  }
  
  ### optimize likelihood
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
  
  ### return results
  ind <- which.min(unlist(lapply(mods, `[[`, "value")))
  if (length(ind) == 0) {
    stop("Estimation failed. Consider increasing 'runs'.", call. = FALSE)
  }
  mods[[ind]][["par"]]
  
}

#' Simulate HMM data
#' 
#' @description 
#' Simulate time series data from an HMM.
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
#' N <- 2
#' dist <- "poisson"
#' theta <- sample_theta(N = N, dist = dist)
#' simulate_hmm(T = 20, N = N, theta = theta, dist = dist)
#'
#' @importFrom stats optim
#'
#' @export

simulate_hmm <- function(T, N, theta, dist) {
  
  ### input checks
  stopifnot(length(T) == 1, T == as.integer(T), T > 1)
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(is.numeric(theta), is.vector(theta))
  stopifnot(length(theta) == N * (N-1) + N + ifelse(dist == "poisson", 0, N))
  
  ### simulate data and states
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
  
  ### return data and states
  structure(x, states = s)
  
}

#' Decode HMM states
#' 
#' @description 
#' TODO
#' 
#' @inheritParams ll_hmm
#' 
#' @return 
#' TODO
#' 
#' @examples
#' # TODO
#' 
#' @importFrom stats dgamma dnorm dpois

decode_states <- function(x, theta, dist) {
  
  T <- length(x)
  N <- model$N
  Gamma <- model$Gamma
  delta <- model$delta
  mu <- model$mu
  sigma <- model$sigma
  allprobs <- matrix(1, N, T)
  for (n in 1:N) {
    allprobs[n, ] <- stats::dgamma(x, shape = mu[n]^2 / sigma[n]^2, scale = sigma[n]^2 / mu[n])
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
}
