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
#' An \code{integer}, the number of states in the HMM.
#' @param dist 
#' A \code{character} indicating the type of state-dependent distribution. 
#' Can be \code{"gaussian"}, \code{"gamma"}, or \code{"poisson"}.
#'
#' @return The negative log-likelihood of the observed data.
#'
#' @examples
#' theta <- c()
#' x <- c(1,2,NA,4,5)
#' ll_hmm(theta = theta, x = x, N = 2, dist = "poisson")
#'
#' @import stats
#'
#' @export

ll_hmm <- function(theta, x, N, dist = "gaussian", neg = FALSE) {
  
  ### input checks
  stopifnot(is.numeric(x), is.vector(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(check_theta(theta = theta, N = N, dist = dist, restricted = FALSE))
  stopifnot(isTRUE(neg) || isFALSE(neg))
  
  ### build probabilities
  Gamma <- diag(N)
  Gamma[!Gamma] <- exp(theta[1:((N - 1) * N)])
  Gamma <- Gamma / rowSums(Gamma)
  delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
  probs <- matrix(NA_real_, nrow = length(x), ncol = N)
  if (dist == "gaussian") {
    mu <- theta[(N - 1) * N + 1:N]
    sigma <- exp(theta[(N - 1) * N + (N + 1):(2 * N)])
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
  } else {
    stop("Invalid state-dependent distribution selected.")
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
#' sample_hmm_theta(N = 3, dist = "gamma")
#'
#' @import stats
#'
#' @export

sample_theta <- function(N, dist, restricted = FALSE) {
  n_params <- N^2 + 3*N
  theta <- rep(0, n_params)
  Gamma <- matrix(0, nrow = N, ncol = N)
  for (i in 1:(N-1)) {
    Gamma[i, i] <- runif(1, 0, 1)
    Gamma[i, (i+1):N] <- runif(N-i, 0, 1 - Gamma[i, i])
  }
  Gamma[N, ] <- rep(1, N)
  theta[1:((N - 1) * N)] <- log(Gamma[!diag(N)])
  if (dist == "gaussian") {
    mu <- runif(N, mu_range[1], mu_range[2])
    sigma <- runif(N, sigma_range[1], sigma_range[2])
    theta[(N - 1) * N + 1:(2 * N)] <- log(c(mu, sigma))
  } else if (dist == "gamma") {
    shape <- runif(N, shape_range[1], shape_range[2])
    rate <- runif(N, rate_range[1], rate_range[2])
    theta[(N - 1) * N + 1:(2 * N)] <- log(c(shape, rate))
  } else if (dist == "poisson") {
    lambda <- runif(N, mu_range[1], mu_range[2])
    theta[(N - 1) * N + 1:N] <- log(lambda)
  } else {
    stop("Invalid emission distribution type")
  }
  return(theta)
}

#' Transform parameter vector

transform_theta <- function(
    theta, N, dist, restricted = FALSE, separate = TRUE
  ) {
  Gamma <- diag(N)
  Gamma[!Gamma] <- exp(theta[1:((N - 1) * N)])
  Gamma <- Gamma / rowSums(Gamma)
  delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
  mu <- exp(theta[(N - 1) * N + 1:N])
  sigma <- exp(theta[(N - 1) * N + (N + 1):(2 * N)])
  list(Gamma = Gamma, delta = delta, mu = mu, sigma = sigma)
}

#' Check parameter vector

check_theta <- function(theta, N, dist, restricted) {
  
}

#' 

mle_hmm <- function(x, N, dist, runs = 1) {
  
  ### input checks
  stopifnot(is.numeric(x), is.vector(x))
  stopifnot(length(N) == 1, N == as.integer(N), N > 1)
  stopifnot(length(dist) == 1, dist %in% c("gaussian", "gamma", "poisson"))
  stopifnot(length(runs) == 1, runs == as.integer(runs), N >= 1)
  
  ### optimize likelihood
  mods <- list()
  for (run in runs) {
    theta <- sample_theta(N, dist, x = x, restricted = FALSE)
    mods[[run]] <- suppressWarnings(
      stats::nlm(f = nll_hmm, p = theta, x = x, N = N, dist = dist, neg = TRUE)
    )
  }
  
  ### return results
  ind <- which.min(lapply(mods, `[[`, "estimate"))
  mods[[ind]]
}

#' Simulate HMM data

sim_hmm <- function(T, N, theta, dist) {
  delta <- rep(1 / N, N)
  s <- rep(0, T)
  s[1] <- sample(1:N, size = 1, prob = delta)
  x <- rep(0, T)
  x[1] <- stats::rgamma(1, shape = mu[s[1]]^2 / sigma[s[1]]^2, scale = sigma[s[1]]^2 / mu[s[1]])
  for (t in 2:T) {
    s[t] <- sample(1:N, size = 1, prob = Gamma[s[t - 1], ])
    x[t] <- stats::rgamma(1, shape = mu[s[t - 1]]^2 / sigma[s[t - 1]]^2, scale = sigma[s[t - 1]]^2 / mu[s[t - 1]])
  }
  structure(x, states = s)
}

#' Decode HMM states

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