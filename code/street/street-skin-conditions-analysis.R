source(here::here("code/packages.R"))
d <- read.csv(here::here("data/skin_conditions_centred.csv"))

d_agg <- aggregate(
  SC ~ routeID + survey_no + time_point_c + Month_c + CNR_c,
  data = d, 
  FUN = function(x) c(sum(x), sum(!x))
)
d_agg$N <- rowSums(d_agg[,c("SC")])

d_agg <- d_agg %>%
  mutate(SC_count = SC[,1])

skin_model_additive <- brm(
  SC_count | trials(N) ~ CNR_c + time_point_c + Month_c + (1|routeID),
  data = d_agg, 
  family = "beta_binomial",
  cores = 4, chains = 4, 
  iter = 2000, warmup = 1000, 
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12
  )
)
