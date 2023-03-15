test_that("HMM parameter vector can be sampled", {
  combinations <- expand.grid(
    N = c(2, 4),
    dist = c("gaussian", "gamma", "poisson"),
    x = c(list(NULL), list(1:10)),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    N <- combinations[i, "N"]
    dist <- combinations[i, "dist"]
    x <- unlist(combinations[i, "x"])
    theta <- sample_theta(N = N, dist = dist, x = x)
    expect_true(is.numeric(theta))
    expect_true(is.vector(theta))
    expected_length <- N * (N-1) + N + ifelse(dist == "poisson", 0, N)
    expect_length(theta, expected_length)
  }
})

test_that("HMM data can be simulated", {
  combinations <- expand.grid(
    T = c(10, 50),
    N = c(2, 3),
    dist = c("gaussian", "gamma", "poisson"),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    T <- combinations[i, "T"]
    N <- combinations[i, "N"]
    dist <- combinations[i, "dist"]
    theta <- sample_theta(N = N, dist = dist)
    x <- simulate_hmm(T = T, N = N, theta = theta, dist = dist)
    expect_true(is.numeric(x))
    expect_length(x, T)
    expect_true(is.numeric(attr(x, "states")))
    expect_length(attr(x, "states"), T)
    expect_true(all(attr(x, "states") %in% 1:N))
  }
})

test_that("HMM log-likelihood can be computed", {
  combinations <- expand.grid(
    N = c(2, 3),
    dist = c("gaussian", "gamma", "poisson"),
    neg = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    N <- combinations[i, "N"]
    dist <- combinations[i, "dist"]
    neg <- combinations[i, "neg"]
    theta <- sample_theta(N = N, dist = dist)
    x <- 1:10
    ll <- ll_hmm(theta = theta, x = x, N = N, dist = dist, neg = neg)
    expect_true(is.numeric(ll))
    expect_true(is.vector(ll))
    expect_length(ll, 1)
  }
})

test_that("HMM parameters can be separated", {
  combinations <- expand.grid(
    N = c(2, 3),
    dist = c("gaussian", "gamma", "poisson"),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    N <- combinations[i, "N"]
    dist <- combinations[i, "dist"]
    theta <- sample_theta(N = N, dist = dist)
    par <- separate_theta(theta = theta, N = N, dist = dist)
    expect_true(is.list(par))
    expect_named(par, c("Gamma", "delta", "mean", "sd"))
  }
})

test_that("HMM mle can be computed", {
  for (dist in c("gaussian", "gamma", "poisson")) {
    N <- 2
    theta <- sample_theta(N = N, dist = dist)
    x <- simulate_hmm(T = 50, N = N, theta = theta, dist = dist)
    mle <- mle_hmm(x = x, N = N, dist = dist, runs = 5)
    expect_true(is.numeric(mle))
    expect_true(is.vector(mle))
    expected_length <- N * (N-1) + N + ifelse(dist == "poisson", 0, N)
    expect_length(mle, expected_length)
  }
})

test_that("HMM state decoding works", {
  combinations <- expand.grid(
    N = c(2, 3),
    T = c(50, 100),
    dist = c("gaussian", "gamma", "poisson"),
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(combinations)) {
    N <- combinations[i, "N"]
    dist <- combinations[i, "dist"]
    T <- combinations[i, "T"]
    theta <- sample_theta(N = N, dist = dist)
    x <- simulate_hmm(T = T, N = N, theta = theta, dist = dist)
    states <- decode_states(x = x, theta = theta, dist = dist, N = N)
    expect_true(is.numeric(states))
    expect_length(states, T)
    expect_true(all(states %in% 1:N))
  }
})



