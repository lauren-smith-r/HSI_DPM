source(here::here("code/packages.R"))
d <- read.csv(here::here("data/total_count.csv"))

count_model <- brm(
  formula = total_count ~ CNR_c + time_point_c + Month_c + (1|routeID),
  data = d, 
  family = "negbinomial",
  cores = 4, chains = 4, 
  iter = 2000, warmup = 1000, 
)
