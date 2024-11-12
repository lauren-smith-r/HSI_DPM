source(here::here("code/packages.R"))
d <- read.csv(here::here("data/clinical_data.csv"))
d_female <- read.csv(here::here("data/female_clinical.csv"))

cores <- parallel::detectCores() # determine number of cores
############### Pregnancy ###############
Pregnant_model_gam = brm(
  formula = Pregnant ~ Year + Adult + s(seasonal_trend, bs="cr"),
  data = d_female,
  family = bernoulli("logit"),
  file = here::here("03_outputs/pregnant_model_gam_year"),
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12),
  cores = cores - 1
)

Pregnant_model_gam = readRDS(here::here("03_outputs/pregnant_model_gam_year.rds"))

preg_non_seasonal = brm(
  formula = Pregnant ~ Year + Adult,
  data = d_female,
  family = bernoulli("logit"),
  file = here::here("model comparison/pregnant_non_seasonal_year"),
  cores = cores - 1
)

preg_non_seasonal = readRDS(here::here("model comparison/pregnant_non_seasonal_year.rds"))

# seasonal versus non-seasonal model comparison
model_comparison <- kfold(preg_non_seasonal,
                          Pregnant_model_gam)
beepr::beep()
saveRDS(model_comparison, here::here("model comparison/pregnant_model_comparison_year.rds"))
model_comparison <- readRDS(here::here("model comparison/pregnant_model_comparison.rds"))
print(model_comparison)

############### Distemper ###############

cdv_model_seasonal_additive = brm(
  formula = distemper ~ Year + Adult + Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/cdv_gam_additive"),
  cores = cores - 1
)

cdv_model_seasonal_interactive = brm(
  formula = distemper ~ Year + Adult * Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/cdv_gam_interactive"),
  cores = cores - 1
)

cdv_model_non_seasonal_additive = brm(
  formula = distemper ~ Year + Adult + Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/cdv_nonseasonal_additive"),
  cores = cores - 1
)

cdv_model_non_seasonal_interactive = brm(
  formula = distemper ~ Year + Adult * Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/cdv_nonseasonal_interactive"),
  cores = cores - 1
)

# No model comparison necessary, as only one converged.

############### Rabies ###############

rabies_model_seasonal_additive = brm(
  formula = rabies ~ Year + Adult + Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/rabies_seasonal_additive"),
  cores = cores - 1
)

rabies_model_seasonal_interactive = brm(
  formula = rabies ~ Year + Adult * Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/rabies_seasonal_interactive"),
  cores = cores - 1
)

rabies_model_non_seasonal_additive = brm(
  formula = rabies ~ Year + Adult + Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/rabies_nonseasonal_additive"),
  cores = cores - 1
)

rabies_model_non_seasonal_interactive = brm(
  formula = rabies ~ Year + Adult * Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/rabies_nonseasonal_interactive"),
  cores = cores - 1
)

model_comparison <- kfold(
  # rabies_model_seasonal_additive,
  #                         rabies_model_seasonal_interactive,
  rabies_model_non_seasonal_additive,
  rabies_model_non_seasonal_interactive)
beepr::beep()

saveRDS(model_comparison, here::here("model comparison/rabies_model_comparison.rds"))
model_comparison <- readRDS(here::here("model comparison/rabies_model_comparison.rds"))
print(model_comparison)

############### TVT ###############


venereal_cancer_model_seasonal_additive = brm(
  formula = venereal_cancer ~ Year + Adult + Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/venereal_seasonal_additive"),
  cores = cores - 1
)

venereal_cancer_model_seasonal_interactive = brm(
  formula = venereal_cancer ~ Year + Adult * Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/venereal_seasonal_interactive"),
  cores = cores - 1
)

venereal_cancer_model_non_seasonal_additive = brm(
  formula = venereal_cancer ~ Year + Adult + Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/venereal_nonseasonal_additive"),
  cores = cores - 1
)

venereal_cancer_model_non_seasonal_interactive = brm(
  formula = venereal_cancer ~ Year + Adult * Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/venereal_nonseasonal_interactive"),
  cores = cores - 1
)

model_comparison <- kfold(venereal_cancer_model_seasonal_additive,
                          venereal_cancer_model_seasonal_interactive,
                          venereal_cancer_model_non_seasonal_additive,
                          venereal_cancer_model_non_seasonal_interactive)
# beepr::beep()

saveRDS(model_comparison, here::here("model comparison/venereal_model_comparison.rds"))
model_comparison <- readRDS(here::here("model comparisom/venereal_model_comparison.rds"))
print(model_comparison)

############## Mange ########################

mange_model_seasonal_additive = brm(
  formula = mange ~ Year + Adult + Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/mange_seasonal_additive"),
  cores = cores - 1,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12)
)

mange_model_seasonal_interactive = brm(
  formula = mange ~ Year + Adult * Female + s(seasonal_trend, bs="cr"),
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/mange_seasonal_interactive"),
  cores = cores - 1
)

mange_model_non_seasonal_additive = brm(
  formula = mange ~ Year + Adult + Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/mange_nonseasonal_additive"),
  cores = cores - 1
)

mange_model_non_seasonal_interactive = brm(
  formula = mange ~ Year + Adult * Female,
  data = d,
  family = bernoulli("logit"),
  file = here::here("model comparison/mange_nonseasonal_interactive"),
  cores = cores - 1
)
summary(mange_model_non_seasonal_interactive)

model_comparison <- kfold(mange_model_seasonal_additive,
                          #mange_model_seasonal_interactive,
                          mange_model_non_seasonal_additive,
                          mange_model_non_seasonal_interactive)


saveRDS(model_comparison, here::here("model comparison/mange_model_comparison.rds"))
model_comparison <- readRDS(here::here("model comparison/mange_model_comparison.rds"))
print(model_comparison)

