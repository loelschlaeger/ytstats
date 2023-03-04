test_that("theta for HMM log-likelihood function can be sampled", {
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
    out <- simulate_hmm(T = T, N = N, theta = theta, dist = dist)
    expect_true(is.numeric(out))
    expect_length(out, T)
    expect_true(is.numeric(attr(out, "states")))
    expect_length(attr(out, "states"), T)
    expect_true(all(attr(out, "states") %in% 1:N))
  }
})



