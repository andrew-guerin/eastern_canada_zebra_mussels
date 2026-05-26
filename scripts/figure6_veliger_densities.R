# this script generates Figure 6 from the Weise et al (2026) article - spatial variation in veliger density in 2023

library(tidyverse)
library(readxl)
library(janitor)

veldat <- read_xlsx("data/source_data/2023_veliger_densities.xlsx") %>%
  clean_names() %>%
  mutate(date=as.Date(date),
         dm=as.factor(case_when(date=="2023-06-08" ~ "08-Jun",
                                date=="2023-06-21" ~ "21-Jun",
                                date=="2023-07-13" ~ "13-Jul",
                                date=="2023-08-07" ~ "07-Aug",
                                date=="2023-09-11" ~ "09-Sep",
                                date=="2023-10-17" ~ "17-Oct",
                                date=="2023-07-25" ~ "25-Jul",
                                date=="2023-09-06" ~ "06-Sep")),
         dm=fct_relevel(dm,
                        "08-Jun",
                        "21-Jun",
                        "13-Jul",
                        "25-Jul",
                        "07-Aug",
                        "06-Sep",
                        "09-Sep",
                        "17-Oct"),
         location=as.factor(case_when(site=="Marina Dégelis" ~ "Lake Témiscouata\n(Dégelis Marina)",
                                      site=="Barrage Témis" ~ "Lake Témiscouata\n(Dégelis Dam)",
                                      site=="Pont Dégelis" ~ "Madawaska River\n(Dégelis Bridge)",
                                      site=="Parc République" ~ "Madawaska River\n(République Park)",
                                      site=="Marina Edmundston" ~ "Edmundston\n(Marina)",
                                      site=="Barrage Edmundston" ~ "Edmundston\n(Dam)")), 
         location=fct_relevel(location,
                              "Lake Témiscouata\n(Dégelis Marina)",
                              "Lake Témiscouata\n(Dégelis Dam)",
                              "Madawaska River\n(Dégelis Bridge)",
                              "Madawaska River\n(République Park)",
                              "Edmundston\n(Marina)",
                              "Edmundston\n(Dam)")) %>%
  rename(abundance=average_abundance_vel_m3) %>%
  mutate(abundance=abundance*5,
         sd=sd*5) %>%
  dplyr::select(site,location,date,abundance,sd,dm)
  
veliger_plot <- 
  ggplot(data=veldat,aes(x=as.factor(dm),y=abundance)) +
  geom_col() +
  geom_errorbar(aes(ymin=abundance-sd,ymax=abundance+sd),width=0.5) +
  facet_grid(cols=vars(location),scales="free_x",space="free") +
  #scale_y_continuous(trans=scales::pseudo_log_trans()) +
  ylab(expression(paste("Veligers per ",m^3))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        axis.title.x = element_blank(),
        strip.text = element_text(size=7),
        strip.background = element_rect(fill="white",colour=NA),
        panel.grid = element_blank())

ggsave(
  filename = "figures/figure6_veligers.png",
  plot = veliger_plot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  

