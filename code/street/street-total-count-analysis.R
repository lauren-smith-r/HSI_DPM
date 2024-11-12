source(here::here("02_r.code/packages.R"))
source(here::here("02_r.code/cleaning/street-total-count-cleaning.R"))

### Check for overdispersion ####

d <- d %>%
  filter(!total_count == 0) %>%
  filter(!total_count == 1)
# 
# ggplot(d_2, aes(total_count)) +
#   geom_histogram(bins = 100)
# 
# library(lmerTest)
# library(AER)
# library(MASS)
# model <- glmer(total_count ~ CNR_c * time_point_c + Month_c + (1|routeID),
#                data = d,
#                family = "poisson")
# summary(model)
# performance::check_overdispersion((performance::simulate_residuals(model)))
# 
# nb_model <- glmer.nb(total_count ~ CNR_c * time_point_c + Month_c + (1|routeID),
#                 data = d)
# 
# summary(nb_model)
# 
# library(MASS)
# library(lmtest)
# 
# # likelihood ratio test to compare poisson versus negative binomial
# # https://stats.stackexchange.com/questions/311556/help-interpreting-count-data-glmm-using-lme4-glmer-and-glmer-nb-negative-binom
# lrtest(model, nb_model)

# d <- read.csv(here::here("01_data/post_2016_total_count.csv"))
# priors_ <-  c(
#   set_prior(prior = "normal(0, 7)", class = "Intercept"),
#   set_prior(prior = "normal(0, 1)", class = "b"),
#   set_prior(prior = "normal(0, 1)", class = "sdgp")
# )
# count_model_additive <- brm(
#   formula = total_count ~ CNR + Month + time_point + (1|routeID),
#   data = d, 
#   family = "poisson",
#   cores = 4, chains = 4, 
#   iter = 2000, warmup = 1000, 
#   # prior = priors_,
#   control = list(
#     adapt_delta = 0.99,
#     max_treedepth = 12
#   ),
#   file = here::here("03_outputs/count_model_rf_additive")
# )
# 
# summary(count_model_additive)

count_model_interactive <- brm(
  formula = total_count ~ CNR_c * time_point_c + Month_c + (1|routeID),
  data = d, 
  family = "negbinomial",
  cores = 4, chains = 4, 
  iter = 2000, warmup = 1000, 
  seed = 1234,
  # prior = priors_,
  # control = list(
  #   adapt_delta = 0.99,
  #   max_treedepth = 12
  # ),
  file = here::here("03_outputs/negbinom_count_model_interactive")
)

summary(count_model_interactive)

count_model_additive <- brm(
  formula = total_count ~ CNR_c + time_point_c + Month_c + (1|routeID),
  data = d, 
  family = "negbinomial",
  cores = 4, chains = 4, 
  iter = 2000, warmup = 1000, 
  seed = 1234,
  # prior = priors_,
  # control = list(
  #   adapt_delta = 0.99,
  #   max_treedepth = 12
  # ),
  file = here::here("03_outputs/negbi_count_model_additive.rds")
)

summary(count_model_additive)
# kfold(count_model_additive, count_model_interactive)

ps_count_model <- as.data.frame(as.matrix(count_model_additive))
fwrite(x = ps_count_model, file = here::here("03_outputs/ps_count_model.csv"), row.names = FALSE)

count_model_additive <- readRDS(here::here("03_outputs/negbi_count_model_additive.rds"))

### Attempting emmeans
formula = total_count ~ CNR * time_point + Month + (1|routeID)
library(emmeans)
summary(count_model_additive)
(emm1 <- emmeans(count_model_additive, specs = pairwise ~ CNR_c * time_point_c + Month_c, type = "response"))

test <- emm1$contrasts %>%
  as.data.frame()

conditional_effects(count_model_additive)















