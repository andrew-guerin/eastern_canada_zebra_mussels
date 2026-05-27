
#this script generates Figure 1 from the Weise et al 2026 article (maps of study area)

library(tidyverse)
library(sf)
library(rnaturalearth)
library(readxl)
library(ggpubr)

crs1 <- 4326 # WGS84 lat-long
sf_use_s2(FALSE)

# load basemap data 

# lake and river data obtained from https://www.donneesquebec.ca/recherche/dataset/grhq
lakes_rivers <- st_read("data/geodata/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% st_transform(crs=crs1)

temi <- lakes_rivers %>% filter(TOPONYME == "Lac Témiscouata")

madawaska <- lakes_rivers %>% filter(TOPONYME == "Rivière Madawaska")

saintjohn <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf") %>% filter(name=="Saint John")

# country outlines
can <- ne_countries(country = "canada", returnclass = "sf", scale = "medium")
usa <- ne_countries(country = "united states of america", returnclass = "sf", scale = "medium")

n_amer <- rbind(can,usa)

# provincial and state outlines
provlims <- ne_states(country="canada", returnclass = "sf") %>%
  filter(name %in% c("Québec",
                     "New Brunswick",
                     "Prince Edward Island",
                     "Nova Scotia")) 

statlims <- ne_states(country="united states of america", returnclass = "sf") %>% 
  filter(name %in% c("Maine",
                     "New Hampshire"
                     ))

terrs <- rbind(provlims,statlims)

#Saint John River watershed
# use level 5 hydrobasins data from https://www.hydrosheds.org/products/hydrobasins
bvsj <- read_sf("data/geodata/hybas/hybas_na_lev05_v1c.shp") %>% filter(HYBAS_ID == 7050038320)

# sampling site locations
benthic_sites <- read_xlsx("data/source_data/temi_sitelist.xlsx", sheet="benthic") %>%
  mutate(site_id = case_when(site_id == "P" ~ "Dégelis Marina (P)",
                             TRUE ~ site_id),
         site_type = "benthic") %>%
  dplyr::select(2:5)

plant_sites <- read_xlsx("data/source_data/temi_sitelist.xlsx", sheet="plant") %>%
  mutate(site_type = "plant") %>%
  dplyr::select(2:5)

veliger_sites <- read_xlsx("data/source_data/temi_sitelist.xlsx", sheet="veliger") %>%
  mutate(site_type = "veliger") %>%
  filter(site_id %in% c("Dégelis Dam", "Dégelis Bridge")) 

sampling_sites <- bind_rows(benthic_sites,plant_sites,veliger_sites) %>%
  mutate(latitude_adj = case_when(site_id %in% c("Dégelis Bridge",
                                                 "Dégelis Dam",
                                                 "Dégelis Marina (P)",
                                                 "J") ~ latitude,
                                  TRUE ~ latitude + 0.01),
         longitude_adj = case_when(site_id == "Dégelis Bridge" ~ longitude - 0.07,
                                   site_id == "Dégelis Dam" ~ longitude + 0.065,
                                   site_id == "Dégelis Marina (P)" ~ longitude - 0.085,
                                   site_id == "J" ~ longitude + 0.01,
                                   TRUE ~ longitude),
         site_type = case_when(site_id == "Dégelis Marina (P)" ~ "benthic and veliger",
                               TRUE ~ site_type),
         site_type = fct_relevel(site_type,
                                 "benthic",
                                 "veliger",
                                 "benthic and veliger",
                                 "plant"))

site_positions <- sampling_sites %>% st_as_sf(coords=c("longitude","latitude"),crs=crs1) 
label_positions <- sampling_sites %>% st_as_sf(coords=c("longitude_adj","latitude_adj"),crs=crs1) 

# generate component maps

wide_area <-    
  ggplot() +
  geom_sf(data=n_amer,
          aes(fill=admin)) +
  geom_sf(data=bvsj, linetype=2,fill=NA) +
  scale_fill_manual(values = c("grey75","grey50")) +
  geom_rect(aes(xmin=-71,xmax=-64,ymin=45,ymax=49.4),
            fill=NA,
            colour="black",
            linewidth=0.5) +  
  geom_text(aes(x=-70,y=52.5), label="Canada", size=4) +
  geom_text(aes(x=-73.25,y=44), label="USA", size=4) +
  coord_sf(xlim=c(-80,-53),ylim=c(44,62)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size=7)) 

zoom1 <-    
  ggplot() +
  geom_sf(data=terrs,
          aes(fill=admin)) +
  geom_sf(data=bvsj, linetype=2,fill=NA) +
  scale_fill_manual(values = c("grey75","grey50")) +
  geom_rect(aes(xmin=-68.95,xmax=-68.52,ymin=47.54,ymax=47.825),
            fill=NA,
            colour="black",
            linewidth=0.25) +  
  geom_sf(data=temi,
        fill="dodgerblue",
        colour="dodgerblue",
        linewidth=0.25) +
  geom_sf(data=saintjohn, linewidth=0.5, colour="blue") +
  geom_sf(data=madawaska, linewidth=0.25, colour="blue") +
  geom_text(aes(x=-66.75,y=48.5), label="QC", size=7) +
  geom_text(aes(x=-66.5,y=46.75), label="NB", size=7) +
  geom_text(aes(x=-69,y=46.25), label="ME", size=7) +
  coord_sf(xlim=c(-71,-64),ylim=c(45,49.4),
           expand=FALSE) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size=7))
 
zoom2 <-    
  ggplot() +
  geom_sf(data=lakes_rivers,
          fill="lightblue",
          colour="grey30",
          linewidth=0.1) +
  geom_sf(data=temi,
          fill="dodgerblue",
          colour="grey20",
          linewidth=0.25) +
  geom_sf(data=site_positions,
          size = 3,
          aes(shape=site_type)) +
  geom_sf_text(data=label_positions,
               aes(label=site_id),
               size = 5) +
  scale_shape_manual("Sampling",
                     values = c(16,17,18,15),
                     labels = c("Benthic samples",
                                "Veliger tows",
                                "Benthic samples and veliger tows",
                                "Plant stems")) +
  coord_sf(xlim=c(-68.95,-68.52),ylim=c(47.54,47.825),
           expand=FALSE) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "inside",
        legend.justification = c(0.98,0.97),
        legend.background = element_rect(fill="white",colour="black"),
        panel.grid = element_blank(),
        axis.text = element_text(size=8))
  
study_map <- ggarrange(
                       ggarrange(wide_area,zoom1,
                                 ncol=1,nrow=2,
                                 heights=c(1,1),
                                 labels=c("a","b"),
                                 font.label=list(size=20)),
                       zoom2,
                       ncol=2,nrow=1,
                       widths=c(1,2.1),
                       labels=c("","c"),
                       font.label=list(size=20))

ggsave(
  filename = "figures/figure1_study_map.png",
  plot = study_map,
  device = "png",
  path = NULL,
  scale = 1,
  width = 21,
  height = 13,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  
  


