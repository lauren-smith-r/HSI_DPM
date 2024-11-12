source(here::here("code/packages.R"))

### Total counts ####
d_total_counts_raw = read.csv(here::here("data/total_count.csv"))

total_counts_result_df <- d_total_counts_raw %>%
  group_by(time_point, CNR) %>%
  summarise(mean_total_count = round(mean(total_count, na.rm = TRUE), 0),
            sd_total_count = round(sd(total_count, na.rm = TRUE), 0)) %>%
  pivot_wider(names_from = CNR, values_from = c(mean_total_count, sd_total_count),
              names_glue = "{.value}_{CNR}") %>%
  drop_na()

min(total_counts_result_df$mean_total_count_0, na.rm = T)
min(total_counts_result_df$mean_total_count_1, na.rm = T)
max(total_counts_result_df$mean_total_count_0, na.rm = T)
max(total_counts_result_df$mean_total_count_1, na.rm = T)

total_counts_result_df <- total_counts_result_df %>%
  mutate(D_average_count = paste0(mean_total_count_1, " (", sd_total_count_1, ")")) %>%
  mutate(N_average_count = paste0(mean_total_count_0, " (", sd_total_count_0, ")"))

### % sterilised ####

dog_types_raw <- read.csv(here::here("data/dog_types.csv"))

sterilised_result_df <- dog_types_raw %>%
  group_by(time_point, CNR) %>%
  summarise(mean_neutered = round(mean(Neutered == 1) * 100, 0)) %>%
  pivot_wider(names_from = CNR, values_from = mean_neutered,
              names_glue = "{.value}_{CNR}") %>%
  rename(D_percent_sterilised = mean_neutered_1) %>%
  rename(N_percent_sterilised = mean_neutered_0) %>%
  drop_na()

min(sterilised_result_df$D_percent_sterilised, na.rm = T)
min(sterilised_result_df$N_percent_sterilised, na.rm = T)
max(sterilised_result_df$D_percent_sterilised, na.rm = T)
max(sterilised_result_df$N_percent_sterilised, na.rm = T)

### % of juveniles ####

juveniles_result_df <- dog_types_raw %>%
  group_by(time_point, CNR) %>%
  summarise(mean_juv = round(mean(Adult == 0) * 100, 0)) %>%
  pivot_wider(names_from = CNR, values_from = mean_juv,
              names_glue = "{.value}_{CNR}") %>%
  rename(D_percent_juveniles = mean_juv_1) %>%
  rename(N_percent_juveniles = mean_juv_0) %>%
  drop_na

max(juveniles_result_df$D_percent_juveniles)
max(juveniles_result_df$N_percent_juveniles)

### % females sterilised ####

Female_sterilised_result_df <- dog_types_raw %>%
  filter(Female == 1) %>%
  group_by(time_point, CNR) %>%
  summarise(mean_neutered = round(mean(Neutered == 1) * 100, 0)) %>%
  pivot_wider(names_from = CNR, values_from = mean_neutered,
              names_glue = "{.value}_{CNR}") %>%
  rename(D_percent_sterilised = mean_neutered_1) %>%
  rename(N_percent_sterilised = mean_neutered_0) %>%
  drop_na()

### % females lactating ####

Female_lactating_result_df <- dog_types_raw %>%
  filter(Female == 1) %>%
  group_by(time_point, CNR) %>%
  summarise(mean_lactating = round(mean(Lactating == 1) * 100, 0)) %>%
  pivot_wider(names_from = CNR, values_from = mean_lactating,
              names_glue = "{.value}_{CNR}") %>%
  rename(D_percent_lactating = mean_lactating_1) %>%
  rename(N_percent_lactating = mean_lactating_0) %>%
  drop_na()

max(Female_lactating_result_df$D_percent_lactating)
max(Female_lactating_result_df$N_percent_lactating)

### Skin conditions ####

SC_raw_d <- read.csv(here::here("data/skin_conditions.csv"))

SC_result_df <- SC_raw_d %>%
  group_by(time_point, CNR) %>%
  summarise(mean_SC = round(mean(SC == 1) * 100, 0)) %>%
  pivot_wider(names_from = CNR, values_from = mean_SC,
              names_glue = "{.value}_{CNR}") %>%
  rename(D_percent_SC = mean_SC_1) %>%
  rename(N_percent_SC = mean_SC_0)

### Body condition scores ####

bcs_raw <- read.csv(here::here("data/bcs.csv"))

bcs_result_df <- bcs_raw %>%
  group_by(time_point, CNR, bcs) %>%
  summarise(count_bcs = n()) %>%
  group_by(time_point, CNR) %>%
  mutate(total_count = sum(count_bcs)) %>%
  group_by(time_point, CNR, bcs) %>%
  summarise(mean_percentage_bcs = round(sum(count_bcs) / first(total_count) * 100, 0)) %>%
  pivot_wider(names_from = bcs, values_from = mean_percentage_bcs,
              names_glue = "mean_percentage_{bcs}") %>%
  select(time_point, CNR, mean_percentage_C1, mean_percentage_C2, mean_percentage_C3, mean_percentage_C4, mean_percentage_C5) %>%
  mutate_at(vars(starts_with("mean_percentage_C")), ~replace_na(., 0)) 

bcs_result_df <- ungroup(bcs_result_df)

bcs_result_df_direct <- bcs_result_df %>%
  filter(CNR == 1)%>%
  drop_na() %>%
  select(!CNR) %>%
  rename_with(~paste0(., "_direct"), contains("mean_percentage"))

bcs_result_df_not <- bcs_result_df %>%
  filter(CNR == 0) %>%
  drop_na() %>%
  select(!CNR) %>%
  rename_with(~paste0(., "_no_direct"), contains("mean_percentage"))

bcs_result_df <- cbind(bcs_result_df_direct, bcs_result_df_not)

bcs_result_df[,7] <- NULL

bcs_result_df <- bcs_result_df %>%
  select(time_point, 
         mean_percentage_C1_direct, mean_percentage_C1_no_direct, 
         mean_percentage_C2_direct, mean_percentage_C2_no_direct, 
         mean_percentage_C3_direct, mean_percentage_C3_no_direct,
         mean_percentage_C4_direct, mean_percentage_C5_no_direct)

