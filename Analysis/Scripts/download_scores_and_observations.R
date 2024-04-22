library(tidyverse)
library(here)
library(arrow)
library(neon4cast)

#Download archived forecasts and observations

s3_aquatic <- arrow::s3_bucket(bucket = "neon4cast-scores/parquet/aquatics", endpoint_override= "data.ecoforecast.org")
s3_pheno <- arrow::s3_bucket(bucket = "neon4cast-scores/parquet/phenology", endpoint_override= "data.ecoforecast.org")
s3_terr <- arrow::s3_bucket(bucket = "neon4cast-scores/parquet/terrestrial_daily", endpoint_override= "data.ecoforecast.org")

start_ref_date <- as_date('2023-01-01') # what period do you want the scores for?
end_ref_date <- as_date('2023-12-31')
get_refdates <- as.character(seq(start_ref_date, end_ref_date, by = 'day'))

get_sites <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv", show_col_types = F) |> 
  #dplyr::filter(field_site_subtype == 'Wadeable Stream') |> # Subset to the stream sites
  select(field_site_id) |> #slice_head(n = 5)|>
  pull() # get in a vector

get_variables <- c('temperature', 'oxygen', "chla", "gcc_90", "rcc_90", "le", "nee")

aq_mods <- open_dataset(s3_aquatic)|>
  select(model_id)|>distinct(model_id)|>
  collect()|>
  filter(str_detect(model_id, "tg_"))|>pull(model_id) 

terr_mods <- open_dataset(s3_terr)|>
  select(model_id)|>distinct(model_id)|>
  collect()|>
  filter(str_detect(model_id, "tg_"))|>pull(model_id) 

pheno_mods <- open_dataset(s3_pheno)|>
  select(model_id)|>distinct(model_id)|>
  collect()|>
  filter(str_detect(model_id, "tg_"))|>pull(model_id) 

tg_mods <- c("tg_arima","tg_auto_adam","tg_bag_mlp","tg_bag_mlp_all_sites","tg_ets", "tg_humidity_lm", "tg_humidity_lm_all_sites", "tg_lasso","tg_lasso_all_sites", "tg_precip_lm","tg_precip_lm_all_sites", "tg_randfor","tg_randfor_all_sites","tg_tbats",  "tg_temp_lm","tg_temp_lm_all_sites") 

aq_scores <- open_dataset(s3_aquatic)|>
  filter(reference_datetime %in% get_refdates,
         model_id %in% tg_mods,
         site_id %in% get_sites,
         variable %in% get_variables)|> 
  collect() |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)))

terr_scores <- open_dataset(s3_terr) |>
  filter(reference_datetime %in% get_refdates,
         model_id %in% tg_mods,
         site_id %in% get_sites,
         variable %in% get_variables) |> 
  collect() |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)))

pheno_scores <- open_dataset(s3_pheno) |>
  filter(reference_datetime %in% get_refdates,
         model_id %in% tg_mods,
         site_id %in% get_sites,
         variable %in% get_variables) |> 
  collect() |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)))


# Write scores to file

aq_scores|>write_parquet(sink = here("Data/Raw_Scores_and_Obs/aquatic_scores.parquet"))
terr_scores|>write_parquet(sink = here("Data/Raw_Scores_and_Obs/terrestrial_scores.parquet"))
pheno_scores|>write_parquet(sink = here("Data/Raw_Scores_and_Obs/phenology_scores.parquet"))
