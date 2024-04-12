



obs_df <- read_csv(here("Data/target_observations_2023.csv"))

var_names <- unique(obs_df$variable)

plots_theme <- theme(axis.title.x = element_blank())

# chla
chla_plots <- ggplot(obs_df|>filter(variable == "chla"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  scale_x_datetime(date_labels = "%b")+
  ylab("Chlorophyll (ug/L)")+
  theme_bw()+
  plots_theme

ggsave(chla_plots,file = here("Analysis/Figures/Observation_plots/Chlorophyll_ts.pdf"), 
       width = 12, height = 10, units = "in")

#temp
temp_plots <- ggplot(obs_df|>filter(variable == "temperature"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  theme_bw()+
  scale_x_datetime(date_labels = "%b")+
  ylab("Temperature (C)")+
  plots_theme

ggsave(temp_plots,file = here("Analysis/Figures/Observation_plots/Temperature_ts.pdf"), 
       width = 12, height = 10, units = "in")

#oxygen
oxy_plots <- ggplot(obs_df|>filter(variable == "oxygen"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  theme_bw()+
  scale_x_datetime(date_labels = "%b")+
  ylab("Dissolved Oxygen (mg/L)")+
  plots_theme

ggsave(oxy_plots,file = here("Analysis/Figures/Observation_plots/Oxygen_ts.pdf"), 
       width = 12, height = 10, units = "in")

#nee
nee_plots <- ggplot(obs_df|>filter(variable == "nee"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  theme_bw()+
  scale_x_datetime(date_labels = "%b")+
  ylab("Net Ecosystem Exchange (g C m-2 day-1)")+
  plots_theme

ggsave(nee_plots,file = here("Analysis/Figures/Observation_plots/NEE_ts.pdf"), 
       width = 12, height = 10, units = "in")

#le
le_plots <- ggplot(obs_df|>filter(variable == "le"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  theme_bw()+
  scale_x_datetime(date_labels = "%b")+
  ylab("Latent Heat Flux (W/m2)")+
  plots_theme

ggsave(le_plots,file = here("Analysis/Figures/Observation_plots/LE_ts.pdf"), 
       width = 12, height = 10, units = "in")

#gcc90
gcc_plots <- ggplot(obs_df|>filter(variable == "gcc_90"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  theme_bw()+
  scale_x_datetime(date_labels = "%b")+
  ylab("Green Chromatic Coordinate")+
  plots_theme

ggsave(gcc_plots,file = here("Analysis/Figures/Observation_plots/GCC_ts.pdf"), 
       width = 12, height = 10, units = "in")

#rcc90
rcc_plots <- ggplot(obs_df|>filter(variable == "rcc_90"), aes(x = reference_datetime, y = obs))+geom_point()+
  facet_wrap(.~site_id, scales = "free")+
  theme_bw()+
  scale_x_datetime(date_labels = "%b")+
  ylab("Red Chromatic Coordinate")+
  plots_theme

ggsave(rcc_plots,file = here("Analysis/Figures/Observation_plots/RCC_ts.pdf"), 
       width = 12, height = 10, units = "in")
