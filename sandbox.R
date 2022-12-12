# Read and save YouTube data ----------------------------------------------

data <- read_yt(
  email = "oelschlaeger.lennart@gmail.com",
  from = "2020-01-01",
  to = "2022-09-01"
)

saveRDS(data, "data/data.rds", compress = "xz")

# Load YouTube data -------------------------------------------------------

data <- readRDS("data/data.rds")








