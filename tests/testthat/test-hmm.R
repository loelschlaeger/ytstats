test_that("HMM log-likelihood can be computed", {
  N <- 2
  dist <- "poisson"
  theta <- sample_theta(N = N, dist = dist)
  x <- c(1, 2, NA, 4, 5)
  ll_hmm(theta = theta, x = x, N = N, dist = dist)
})

test_that("theta for HMM log-likelihood function can be sampled", {
   
})
