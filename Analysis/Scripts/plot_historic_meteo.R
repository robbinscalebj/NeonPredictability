#summarize historic meteo

meteo_df <- read_csv(here("Data/historical-pre2023_daily-meteo-means.csv"))

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv")|>
  rename(site_id = "field_site_id")|>
  mutate(field_site_type = str_remove(field_site_type,"Core |Gradient "),
         site_category = ifelse(field_site_type == "Terrestrial", field_site_type, str_c(field_site_type, field_site_subtype, sep = "_")),
         .after = 1)



variables <- c('air_temperature',
               "surface_downwelling_longwave_flux_in_air",
               "surface_downwelling_shortwave_flux_in_air",
               "precipitation_flux",
               "air_pressure",
               "relative_humidity",
               "air_temperature",
               "northward_wind",
               "eastward_wind")

# Climate
meteo_sums <- meteo_df|>
  mutate(year = year(datetime))|>
  filter(year > 2020)|> #even coverage of two years
  group_by(site_id)|>
  summarize(across(matches(variables), list(mean = mean, stdev = sd)))|>
  left_join(site_data)


clim.p_repel <- ggplot(data=meteo_sums)+
  geom_text_repel(aes(x = precipitation_flux_mean, y = air_temperature_mean, label = site_id), 
                  max.overlaps = 10)+
  ylab("Mean Daily Air temperature (C)")+
  xlab("Mean Daily Precip Flux")+
  facet_wrap(.~site_category)+
  theme_bw()
clim.p_repel

ggsave(clim.p_repel,file = here("Analysis/Figures/Meteo_Plots/Climate_summaries.pdf"), 
       width = 12, height = 10, units = "in")


# Time series


plots_function <- function(site){
  
  plot <- meteo_df|>
    filter(site_id == site)|>
    ggplot(aes(x = datetime, y = air_temperature))+geom_point()+
    facet_wrap(.~site_id, scales = "free")+
    ylab("Air Temperature (C)")+
    theme_bw()+
    theme(axis.title.x = element_blank())
  

 ggsave(plot, filename = here(paste0("Analysis/Figures/Meteo_Plots/AirTemp_TimeSeries/",site,".pdf")), 
         height = 7, width = 10, units = "in")
}

map(unique(meteo_df$site_id), ~plots_function(site = .))


############









temp_plots <- ggplot(meteo_df, aes(x = datetime, y = air_temperature))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  #scale_x_date(date_labels = "%b")+
  ylab("Air Temperature(C)")+
  theme_bw()+
  plots_theme

ggsave(temp_plots,file = here("Analysis/Figures/Meteo_Plots/AirTemp_ts.pdf"), 
       width = 12, height = 10, units = "in")
