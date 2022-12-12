nll_hmm <- function(theta.star,x,N){
  Gamma <- diag(N)
  Gamma[!Gamma] <- exp(theta.star[1:((N-1)*N)])
  Gamma <- Gamma/rowSums(Gamma)
  delta <- solve(t(diag(N)-Gamma+1),rep(1,N))
  mu <- exp(theta.star[(N-1)*N+1:N])
  sigma <- exp(theta.star[(N-1)*N+(N+1):(2*N)])
  allprobs <- matrix(1,length(x),N)
  ind <- which(!is.na(x))
  for (j in 1:N){
    allprobs[ind,j] <- stats::dgamma(x[ind],shape=mu[j]^2/sigma[j]^2,scale=sigma[j]^2/mu[j])
  }
  foo <- delta%*%diag(allprobs[1,])
  l <- log(sum(foo))
  phi <- foo/sum(foo)
  for (t in 2:length(x)){
    foo <- phi%*%Gamma%*%diag(allprobs[t,])
    l <- l+log(sum(foo))
    phi <- foo/sum(foo)
  }
  return(-l)
}

fit_hmm <- function(data,N){
  theta.star <- c(rep(-2,(N-1)*N),log(seq(1,10,length=N)),log(seq(1,10,length=N)))
  mod <- suppressWarnings(nlm(nll_hmm,theta.star,x=data,N=N))
  theta.star <- mod$estimate
  Gamma <- diag(N)
  Gamma[!Gamma] <- exp(theta.star[1:((N-1)*N)])
  Gamma <- Gamma/rowSums(Gamma)
  delta <- solve(t(diag(N)-Gamma+1),rep(1,N))
  mu <- exp(theta.star[(N-1)*N+1:N])
  sigma <- exp(theta.star[(N-1)*N+(N+1):(2*N)]) 
  list(Gamma = Gamma, delta = delta, mu = mu, sigma = sigma)
}

#' Simulate HMM
#' 
#' @description 
#' TODO
#' 
#' @examples 
#' \dontrun{
#'   Gamma <- matrix(c(0.9,0.1,0.1,0.9),nrow=2)
#'   mu <- c(1,3)
#'   sigma <- c(0.5,0.5)
#'   ts <- sim_hmm(T = 100, N = 2, Gamma = Gamma, mu = mu, sigma = sigma)
#'   model <- fit_hmm(ts, N = 2)
#'   states <- decode_states(ts, model, N = 2)
#'   plot(ts, type = "l")
#'   points(ts, col = states)
#' }

sim_hmm <- function(T,N,Gamma,mu,sigma){
  delta <- rep(1/N,N)
  s <- rep(0,T)
  s[1] <- sample(1:N,size=1,prob=delta)
  x <- rep(0,T)
  x[1] <- stats::rgamma(1,shape=mu[s[1]]^2/sigma[s[1]]^2,scale=sigma[s[1]]^2/mu[s[1]])
  for(t in 2:T){
    s[t] <- sample(1:N,size=1,prob=Gamma[s[t-1],])
    x[t] <- stats::rgamma(1,shape=mu[s[t-1]]^2/sigma[s[t-1]]^2,scale=sigma[s[t-1]]^2/mu[s[t-1]])
  }
  structure(x, states = s)
}

decode_states <- function(data,model,N){
  T <- length(data)
  Gamma <- model$Gamma
  delta <- model$delta
  mu <- model$mu
  sigma <- model$sigma
  allprobs <- matrix(0,N,T)
  for (n in 1:N) {
    allprobs[n,] <- stats::dgamma(data,shape=mu[n]^2/sigma[n]^2,scale=sigma[n]^2/mu[n])
  }
  xi <- matrix(0,N,T)
  for (n in 1:N) {
    xi[n,1] <- log(delta[n]) + log(allprobs[n,1])
  }
  for (t in 2:T) {
    for (n in 1:N) {
      xi[n,t] <- max(xi[,t-1] + log(Gamma[,n])) + log(allprobs[n,t])
    }
  }
  iv <- numeric(T)
  iv[T] <- which.max(xi[,T])
  for (t in rev(seq_len(T-1))) {
    iv[t] <- which.max(xi[,t] + log(Gamma[,iv[t + 1]]))
  }
  return(iv)
}
