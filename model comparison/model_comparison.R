require(brms) # bayesian modelling
require(data.table)

set.seed(146)

### Street surveys ####

# count model
count_int <- readRDS(here::here("03_outputs/negbinom_count_model_interactive.rds"))
count_add <- readRDS(here::here("03_outputs/negbi_count_model_additive.rds"))

# sterilised
sterilised_int <- readRDS(here::here("03_outputs/sterilised_model_interactive.rds"))
sterilised_add <- readRDS(here::here("03_outputs/sterilised_model_additive.rds"))

# adult
adult_int <- readRDS(here::here("03_outputs/juv_model_interactive.rds"))
adult_add <- readRDS(here::here("03_outputs/juv_model_additive.rds"))

# skin conditions
skin_int <- readRDS(here::here("03_outputs/skin_model_interactive_betabinomial.rds"))
skin_add <- readRDS(here::here("03_outputs/skin_model_additive_betabinomial.rds"))

# lactating
lactating_int <- readRDS(here::here("03_outputs/lactating_model_interactive.rds"))
lactating_add <- readRDS(here::here("03_outputs/lactating_model_additive.rds"))

model_comparison <- kfold() # add models to compare


