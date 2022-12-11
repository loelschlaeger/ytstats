# Read and save YouTube data ----------------------------------------------

data <- read_yt(
  email = "oelschlaeger.lennart@gmail.com",
  from = "2022-08-25",
  to = "2022-08-26"
)

saveRDS(data, "data/data.rds", compress = "xz")


# Load YouTube data -------------------------------------------------------

data <- readRDS("data/data.rds")


# HMM simulation ----------------------------------------------------------

Gamma <- matrix(c(0.9,0.1,0.1,0.9),nrow=2)
mu <- c(1,3)
sigma <- c(0.5,0.5)
ts <- sim_hmm(T = 100, N = 2, Gamma = Gamma, mu = mu, sigma = sigma)
model <- fit_hmm(ts, N = 2)
states <- decode_states(ts, model, N = 2)
plot(ts, type = "l")
points(ts, col = states)


# HMM application ---------------------------------------------------------

ts <- data %>% group_by(day) %>% summarize(x = mean(viewmins)) %>% pull(x) %>% scale(center = FALSE)
model <- fit_hmm(ts, N = 2)
states <- decode_states(ts, model, N = 2)
plot(ts, type = "l")
points(ts, col = states)
