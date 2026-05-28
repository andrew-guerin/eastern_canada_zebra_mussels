
library(tidyverse)
library(readxl)

#`%ni%` <- Negate(`%in%`)

# get site locations and zebra mussel data
#order_custom <- c("A","B","C","D","H","I","J","M","N","L","K","O","P","E","F","G")

#locations <- read_xlsx("source_data/monitorage_2022.xlsx", sheet="sites") %>% 
#  dplyr::select(1) %>%
#  mutate(site_new=order_custom) %>%
#  arrange(site_new)


sites <- read_xlsx("data/source_data/temi_sitelist.xlsx", sheet="benthic") %>%
  dplyr::select(1,2) %>%
  rename(site=site_orig)

data22 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2022") %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre),
            density=total/0.25) %>%
  mutate(year=2022) %>%
  dplyr::select(year,site,quadrat,density)
  
data23 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="counts_2023") %>%
  rename(site=site,
         quadrat=quadrat) %>%
  rowwise() %>%
  mutate(adults=as.numeric(nb_gros) / 0.25,
         juvs=as.numeric(nb_5mm) / 0.25, 
         density=juvs + adults,
         year=2023) %>%
  dplyr::select(year,site,quadrat,density)

quadlist24 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2024") %>%
  dplyr::select(site,quadrat,taille_quadrat) %>%
  distinct()
  
data24 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2024") %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre)) %>%
  ungroup() %>%
  left_join(quadlist24) %>%
  mutate(density= total / taille_quadrat,
         year=2024) %>%
  dplyr::select(year,site,quadrat,density)

data_all <- bind_rows(data22,
                      data23,
                      data24) %>%
  left_join(sites) %>%
  mutate(year=as.factor(year),
         lg2dens = log2(density + 1))
  
abundance_plot <- 
  ggplot(data=data_all,aes(x=year,y=lg2dens)) + 
  geom_boxplot(aes(fill=year)) +
  scale_fill_manual("Year", values=c("white","grey","black")) +
  facet_wrap(~site_id, nrow=2, strip.position = "bottom") +
  xlab("Site") + 
  ylab(bquote("Log-transformed density, mussels per m"^2)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text=element_text(size=10))

ggsave(
  filename = "figures/S1_temi_zm_temporal.png",
  plot = abundance_plot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 19,
  height = 15,
  units = "cm",
  dpi = 400,
  limitsize = TRUE,
  bg = "white")  

