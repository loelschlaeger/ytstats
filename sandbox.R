# Read and save YouTube data ----------------------------------------------

ytdata_example <- read_yt(
  email = "oelschlaeger.lennart@gmail.com",
  from = "2020-01-01",
  to = "2022-09-01"
)

usethis::use_data(ytdata_example, compress = "xz", overwrite = TRUE)

# Load YouTube data -------------------------------------------------------

data <- ytstats::ytdata_example
print(data)
summary(data)
attributes(data)$videos  # access video data
attributes(data)$channel # access channel data

# Plot data ---------------------------------------------------------------

plot(data, "views")
plot(data, "likes")
plot(data, "dislikes")
plot(data, "viewmins", from = "2020-01-01", to = "2020-02-01")

