source(here::here("code/packages.R"))
d <- read.csv(here::here("data/dog_types_centred.csv"))
# Sterilised model ####
d_agg <- aggregate(
  Neutered ~ routeID + survey_no + time_point_c + Month_c + CNR_c,
  data = d, 
  FUN = function(x) c(sum(x), sum(!x))
)

d_agg$N <- rowSums(d_agg[,c("Neutered")])

d_agg <- d_agg %>%
  mutate(Neutered_count = Neutered[,1])

sterilised_model_additive <- brm(
  Neutered_count | trials(N) ~ CNR_c + time_point_c + Month_c + (1|routeID),
  data = d_agg, 
  family = "binomial",
  cores = 4, chains = 4, 
  iter = 2000, warmup = 1000, 
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12
  ),
)
