source(here::here("code/packages.R"))

d <- read.csv(here::here("data/clinical.csv"))

# how they were captured
d$get <- as.factor(d$get)
levels(d$get)

caught_by_net <- d %>%
  filter(get %in% c("nn", "net", "net+"))

caught_by_hand <- d %>%
  filter(get %in% c("hand", "Hand", "hand010"))

owners <- d %>%
  filter(get == "owner")

Trapped <- d %>%
  filter(get %in% c("trap", "Trap"))

other_method <- d %>%
  filter(get %in% c("", "9.9"))

nrow(caught_by_net) + nrow(caught_by_hand) + nrow(owners) + nrow(Trapped) + nrow(other_method)

nrow(caught_by_net)

# Sex
na_count <- sum(is.na(d$Female))
na_rows_age <- d %>%
  filter(is.na(Female))
sum(d$Female, na.rm = T)

females <- d %>%
  filter(Female == 1)

males <- d %>%
  filter(Female == 0)

# Age

adults <- d %>%
  filter(Adult == 1)

juveniles <- d %>%
  filter(Adult == 0)

na_rows_age <- d %>%
  filter(is.na(Adult))





