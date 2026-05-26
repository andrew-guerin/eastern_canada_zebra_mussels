# this script generates Figure 4 from the Weise et al (2026) article
# density of zebra mussels on stems of specific plant species

library(tidyverse)
library(readxl)
library(ggpubr)

stem_data <- read_xlsx("data/source_data/plant_data.xlsx", sheet="zm_cm_stem_site") %>%
  mutate(year = as.factor(year))

# generate plots comparing years for species which were sampled at the same site(s) in 2022 and 2023
# first, gather data for Elodea canadensis, sampled from sites GB, RT, and TE in both years

elodat <- stem_data %>%  filter(species == "Elodea canadensis") 

site_names <- c(
  `GB` = "Site GB",
  `RT` = "Site RT",
  `TE` = "Site TE")

#boxplot comparing each year at each site
elobox <-   
  ggplot(data=elodat,aes(x=year,y=zm_cm_stem,group=year,fill=year)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("grey45","grey75")) +
  xlab("Year") +
  ylab("Zebra mussels per cm") +
  ylim(0,3.6) +
  facet_wrap(~site,nrow=1,labeller = as_labeller(site_names)) +  
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        strip.background = element_blank())

# second, Potamogeton richardsonii, sampled from site RT in both years
prichdat <- stem_data %>%  filter(species == "Potamogeton richardsonii" & site %in% c("RT")) 

#boxplot comparing each year at each site
prichbox <-   
  ggplot(data=prichdat,aes(x=year,y=zm_cm_stem,group=year,fill=year)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("grey45","grey75")) +
  xlab("Year") +
  ylab("Zebra mussels per cm") +
  ylim(0,3.6) +
  facet_wrap(~site,nrow=1,labeller = as_labeller(site_names)) +  
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank())

# third, Potamogeton robbinsii, sampled from sites GB and TE in both years
probbdat <- stem_data %>%  filter(species == "Potamogeton robbinsii" & site %in% c("GB","TE")) 

#boxplot comparing each year at each site
probbbox <-   
  ggplot(data=probbdat,aes(x=year,y=zm_cm_stem,group=year,fill=year)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("grey45","grey75")) +
  xlab("Year") +
  ylab("Zebra mussels per cm") +
  ylim(0,3.6) +
  facet_wrap(~site,nrow=1,labeller = as_labeller(site_names)) +  
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank())

# fourth, Myriophyllum spicatum, sampled from site TE in both years
mspicdat <- stem_data %>%  filter(species == "Myriophyllum spicatum" & site %in% c("TE")) 

#boxplot comparing each year at each site
mspicbox <-   
  ggplot(data=mspicdat,aes(x=year,y=zm_cm_stem,group=year,fill=year)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("grey45","grey75")) +
  xlab("Year") +
  ylab("Zebra mussels per cm") +
  ylim(0,3.6) +
  facet_wrap(~site,nrow=1,labeller = as_labeller(site_names)) +  
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank())

yearsiteplot <- ggarrange(elobox,probbbox,prichbox,mspicbox,
                          nrow=1, 
                          labels=c("a","b","c","d"),
                          widths=c(3,2,1.1,1.1))

ggsave(
  filename = "figures/figure4_plant_densities_yearsite_species.png",
  plot = yearsiteplot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 24,
  height = 12,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white"
)  

