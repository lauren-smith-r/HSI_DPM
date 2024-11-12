source(here::here("code/packages.R"))
d <- read.csv(here::here("data/dog_types_centred.csv"))

d <- d %>%
  mutate(Juvenile = case_when(Adult == 1 ~ 0,
                              Adult == 0 ~ 1))

# Adult model ####
d_agg <- aggregate(
  Juvenile ~ routeID + survey_no + time_point_c + Month_c + CNR_c,
  data = d, 
  FUN = function(x) c(sum(x), sum(!x))
)

d_agg$N <- rowSums(d_agg[,c("Juvenile")])

adult_model_additive <- brm(
  Juvenile[,1] | trials(N) ~ CNR_c + time_point_c + Month_c + (1|routeID),
  data = d_agg,
  family = "binomial",
  cores = 4, chains = 4,
  iter = 2000, warmup = 1000,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12
  )
)

