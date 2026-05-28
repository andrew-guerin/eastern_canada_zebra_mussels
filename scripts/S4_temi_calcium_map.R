# this script generates Supplemental Figure S4 for the Weise et al (2026) article - 2024 calcium measurements for Lake Témiscouata

library(tidyverse)
library(readxl)
library(sf)

crs1 <- 4326 # WGS84 lat-long
#crs2 <- "ESRI:102008" # North America Albers Equal Areas projection - we need to use this to keep the pie charts circular

# load basemap data, obtained from https://www.donneesquebec.ca/recherche/dataset/grhq
temi <- st_read("data/geodata/quebec_lakes_rivers/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Lac Témiscouata") %>% st_transform(crs=crs1)

# load calcium data
temicalc <- read_xlsx("data/source_data/temi_calcium_2024.xlsx") %>%
  mutate(avcalc = round((calcium_rep1 + calcium_rep2) / 2, digits=1),
         latitude=as.numeric(latitude),
         longitude=as.numeric(longitude),
         lat_adj = case_when(site==1 ~ latitude + 0,
                             site==2 ~ latitude + 0,
                             site==3 ~ latitude + 0,
                             site==4 ~ latitude + 0,
                             site==5 ~ latitude - 0.005,
                             site==6 ~ latitude + 0.0075,
                             site==7 ~ latitude - 0.0025,
                             site==8 ~ latitude - 0.003,
                             site==9 ~ latitude + 0.001,
                             site==10 ~ latitude - 0.0075,
                             site==11 ~ latitude + 0.005,
                             site==12 ~ latitude - 0.001,
                             site==13 ~ latitude + 0),
         long_adj = case_when(site==1 ~ longitude - 0.02,
                              site==2 ~ longitude + 0.0275,
                              site==3 ~ longitude - 0.02,
                              site==4 ~ longitude - 0.02,
                              site==5 ~ longitude - 0.015,
                              site==6 ~ longitude + 0.015,
                              site==7 ~ longitude - 0.0175,
                              site==8 ~ longitude - 0.0175,
                              site==9 ~ longitude + 0.02,
                              site==10 ~ longitude + 0.01,
                              site==11 ~ longitude + 0.0175,
                              site==12 ~ longitude - 0.0175,
                              site==13 ~ longitude + 0.02)) 

temicalc_marks <- temicalc %>% st_as_sf(coords=c("longitude","latitude"), crs=crs1) 
temicalc_labs <- temicalc %>% st_as_sf(coords=c("long_adj","lat_adj"), crs=crs1) 

# generate and save plot
temi_plot <-    
  ggplot() +
  geom_sf(data=temi,
          fill="lightblue",
          colour="grey25",
          linewidth=0.25) +
  geom_sf(data=temicalc_marks,
          colour="black",
          size=3) +
  geom_sf_text(data=temicalc_labs,
               aes(label=avcalc),
               size=6,
               fontface="bold") +
  coord_sf(xlim=c(-68.94,-68.63)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        #legend.title = element_text(size=12,face="bold"),
        #legend.position = c(0.855,0.62),
        #legend.text = element_text(size=10),
        panel.grid = element_blank()
  ) 

ggsave(
  filename = "figures/S4_temi_calcium.png",
  plot = temi_plot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 19,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  




