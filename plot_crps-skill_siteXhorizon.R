

comb_preds_sh <- read_csv("GAM_crps_skill_summarized.csv")


comb_preds_sh_summarized <- comb_preds_sh|>
  group_by(horizon, variable)|>
  summarize(median_est = median(estimate))|>
  ungroup()|>
  mutate(variable = fct_recode(variable, Temperature = "temperature", Oxygen = "oxygen", RCC90 = "rcc_90", GCC90 = "gcc_90", LE = "le", NEE = "nee", `Chlorophyll a` = "chla"))
levels(comb_preds_sh_summarized$variable)

#across-site variation in CRPS-based skill
ggplot(comb_preds_sh_summarized|>
         mutate(variable = fct_reorder(variable, desc(cv))), aes(x = variable, y = cv))+geom_point()+
  facet_wrap(.~horizon)+
  ylab("Coefficient of Variation")+
  theme(axis.text.x = element_text(angle = 90))

grey_palette <- c("1" = "grey75", "5" = "grey60", "10" = "grey45", "15" = "grey30", "20" = "grey15", "25" = "grey5", "30" = "black")

ggplot(comb_preds_sh_summarized|>mutate(variable = fct_reorder(variable, desc(median_est))),
       aes(x = variable, y = median_est, group = factor(horizon), color = factor(horizon)))+
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  scale_color_manual(values = grey_palette)+
  guides(color = guide_legend(title = "Horizon (days)"))+
  ylab("CRPS Skill (ML vs Climatological)")+
  theme_bw()+theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 16), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 0.6))
