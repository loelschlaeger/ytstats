# Read and save YouTube data ----------------------------------------------

data <- read_yt(
  email = "oelschlaeger.lennart@gmail.com",
  from = "2020-01-01",
  to = "2022-09-01"
)

saveRDS(data, "data/data.rds", compress = "xz")


# Load YouTube data -------------------------------------------------------

data <- readRDS("data/data.rds")
print(data)
summary(data)


# HMM application ---------------------------------------------------------

ts <- data %>% 
  group_by(day) %>% 
  summarize(x = mean(viewmins)) %>% 
  pull(x) %>% 
  scale(center = FALSE)
model <- fit_hmm(ts, N = 2)
states <- decode_states(ts, model, N = 2)
plot(ts, type = "l")
points(ts, col = states)
