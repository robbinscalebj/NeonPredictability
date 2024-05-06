# Calculate forecast skill/error metrics
library(tidyverse)
library(here)
library(arrow)
library(neon4cast)
library(scoringRules)

source(here("download_target.R"))




# Retrieve archived forecasts

# if these are not local, they can be written in ~10 minutes(?) with script "download_scores_and_observations"
aq_scores<-read_parquet(file = here("Data/Raw_Scores_and_Obs/aquatic_scores.parquet"))
terr_scores<-read_parquet(file = here("Data/Raw_Scores_and_Obs/terrestrial_scores.parquet"))
pheno_scores<-read_parquet(file = here("Data/Raw_Scores_and_Obs/phenology_scores.parquet"))

scores_df <- bind_rows(aq_scores, terr_scores, pheno_scores)
rm(aq_scores, terr_scores, pheno_scores)



# calculate crps

crps_scores_df <- scores_df|>
  filter(horizon >= 0, horizon<35)|>
  filter(!is.na(median), !is.na(observation))|>
  group_by(horizon, site_id, variable, reference_datetime)|>
  summarize(obs = mean(observation),
            crps = crps_sample(y = obs, dat = median))


crps_scores_df|>write_parquet(here("Data/summarized_crps.parquet"))

# calculate CRPS-based skill scores
 ##download targets
targets <- bind_rows(download_target("aquatics"),
                     download_target("terrestrial_daily"),
                     download_target("pheno"))


target_clim <- targets %>%  
  mutate(doy = yday(datetime)) %>% 
  group_by(doy, site_id, variable) %>% 
  summarise(mean = mean(observation, na.rm = TRUE),
            sd = sd(observation, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(mean = ifelse(is.nan(mean), NA, mean))


targets2 <- targets|>
  filter(year(datetime) == 2023)|>
  mutate(doy = yday(datetime))|>
  right_join(target_clim)|>
  filter(!is.na(mean), !is.na(sd))|>
  #group_by(datetime, site_id, variable)|>
  mutate(clim_crps = crps_norm(y = observation, mean = mean, sd = sd))


crps_skill_scores_df <- crps_scores_df|>
  left_join(targets2)



# calculate normed NSE
scores_df_nse <- bind_rows(read_parquet(file = here("Data/Raw_Scores_and_Obs/aquatic_scores.parquet"))|>filter(horizon >= 0, horizon<35)|>group_by(variable, site_id, reference_datetime, horizon)|>summarize(median = median(median, na.rm = TRUE), mean = mean(mean, na.rm = TRUE)), 
                           read_parquet(file = here("Data/Raw_Scores_and_Obs/phenology_scores.parquet"))|>filter(horizon >= 0, horizon<35)|>group_by(variable, site_id, reference_datetime, horizon)|>summarize(median = median(median, na.rm = TRUE), mean = mean(mean, na.rm = TRUE)),
                           read_parquet(file = here("Data/Raw_Scores_and_Obs/terrestrial_scores.parquet"))|>filter(horizon >= 0, horizon<35)|>group_by(variable, site_id, reference_datetime, horizon)|>summarize(median = median(median, na.rm = TRUE), mean = mean(mean, na.rm = TRUE)))


targets_vec <- c("aquatics", "terrestrial_daily", "phenology")

tar_hist <- map(targets_vec, ~download_target(.))|>
  list_rbind()#|>select(-iso_week)

hist_mean_obs <- tar_hist|>
  filter(datetime < as_date ("2023-01-01"))|>
  group_by(variable, site_id)|>
  summarize(mean_historical_observation = mean(observation, na.rm = TRUE),
            iq_range = quantile(observation,0.75, na.rm = TRUE)-quantile(observation, 0.25, na.rm = TRUE))


forecasts2 <- forecasts|>
  left_join(tar_hist)|>
  left_join(hist_mean_obs)


forecasts2 <- scores_df_nse|>
  rename(datetime = "reference_datetime")|>mutate(datetime = as_date(datetime))|>
  left_join(tar_hist)|>
  left_join(hist_mean_obs)

df2 <- forecasts2|>rename(reference_datetime = "datetime")|>
  # filter(!(variable %in% c("abundance", "amblyomma_americanum", "richness")))|>
  group_by(variable)|>
  mutate(forecast_month = month(reference_datetime),
         forecast_week = week(reference_datetime))|>
  filter(horizon >= 0)|>
  relocate(forecast_week)|>
  mutate(modobs_diff = observation-mean,
         modmean_diff = observation - mean_historical_observation)|>
  group_by(variable, reference_datetime, site_id)|>
  mutate(initial_obs = first(observation))|>
  group_by(variable, horizon, site_id, forecast_week)|>
  summarize(mean_hist_obs = mean(mean_historical_observation), #simplifies summarisation - same value being averaged so nothing changed
            N_non_na_obs = sum(!is.na(modobs_diff)),
            randfor_rmse = sqrt(sum((modobs_diff)^2, na.rm = TRUE)/n()),
            randfor_rmse2 = sqrt(sum((modobs_diff)^2, na.rm = TRUE)/N_non_na_obs), #test with non-NA
            #randfor_rmse_normed = randfor_rmse/mean_hist_obs,
            #randfor_rmse_normed2 = randfor_rmse2/mean_hist_obs,
            #randfor_rmse_normed3 = randfor_rmse2/iq_range, 
            #persistence_rmse = sqrt(sum((initial_obs)^2, na.rm = TRUE))/n(), #don't know if n(0 correctly counting NAs)
            #persistence_rmse_normed = persistence_rmse/mean_hist_obs,
            #forecast_skill = 1 - (randfor_rmse_normed/persistence_rmse_normed),
            R2_num = sum((modobs_diff)^2, na.rm = TRUE),
            R2_denom = sum((modmean_diff)^2, na.rm = TRUE),
            R2 = 1-(R2_num/R2_denom), # this is also NSE 
            NSE_normed = 1/(2-R2)
  )


df2|>write_parquet(here("Data/summarized_weekly_normedNSE.parquet"))

