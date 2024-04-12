library(tidyverse)
library(arrow)

#read data
aq_scores<-read_parquet(file = here("Data/Raw_Scores_and_Obs/aquatic_scores.parquet"))
terr_scores<-read_parquet(file = here("Data/Raw_Scores_and_Obs/terrestrial_scores.parquet"))
pheno_scores<-read_parquet(file = here("Data/Raw_Scores_and_Obs/phenology_scores.parquet"))


obs_df <- bind_rows(aq_scores|>filter(horizon == 0)|>group_by(variable, site_id, reference_datetime)|>summarize(obs = mean(observation, na.rm = TRUE)), 
                    pheno_scores|>filter(horizon == 0)|>group_by(variable, site_id, reference_datetime)|>summarize(obs = mean(observation, na.rm = TRUE)),
                    terr_scores|>filter(horizon == 0)|>group_by(variable, site_id, reference_datetime)|>summarize(obs = mean(observation, na.rm = TRUE)))|>
  mutate(reference_datetime = as_datetime(reference_datetime))

obs_df|>write_csv(here("Data/target_observations_2023.csv"))