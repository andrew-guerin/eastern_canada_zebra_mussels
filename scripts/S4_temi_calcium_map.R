# this script generates Supplemental Figure S4 for the Weise et al (2026) article - 2024 calcium measurements for Lake Témiscouata

library(tidyverse)
library(readxl)
library(sf)

crs1 <- 4326 # WGS84 lat-long
crs2 <- "ESRI:102008" # North America Albers Equal Areas projection - we need to use this to keep the pie charts circular

# load basemap data, obtained from https://www.donneesquebec.ca/recherche/dataset/grhq
temi <- st_read("data/geodata/quebec_lakes_rivers/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Lac Témiscouata") %>% st_transform(crs=crs1)

# define the map limits in lat-long and convert these to positions in the equal areas projection
limits <- rbind(c(-68.92,47.61),
                c(-68.92,47.77),
                c(-68.6,47.61),
                c(-68.6,47.77)) %>%
  as.data.frame() %>%
  st_as_sf(coords=c("V1","V2"),crs=crs1) %>%
  st_transform(crs=crs2) %>%
  st_coordinates() %>%
  as.data.frame()

xright <- max(limits$X)
xleft <- min(limits$X)
ybottom <- min(limits$Y)
ytop <- max(limits$Y)

# load calcium data

temicalc <- read_xlsx("source_data/2024_temiscouata_calcium.xlsx",sheet="Calcium_Donnees Témis") %>%
  slice_head(n=20) %>%
  row_to_names(row_number = 6) %>%
  clean_names() %>%
  slice_tail(n=13) %>%
  dplyr::select(1:3,9,10) %>%
  rename(lab1 = 4,
         lab2 = 5) %>%
  mutate(avcalc = round((as.numeric(lab1) + as.numeric(lab2)) / 2,digits=1)) %>%
  st_as_sf(coords=c("longitude","latitude"),crs=crs1) %>%
  st_transform(crs=crs2)

# add some old data

# generate and save plot
temi_plot <-    
  ggplot() +
  geom_sf(data=temi,
          fill="lightblue",
          colour="grey25",
          linewidth=0.25) +
  geom_sf(data=temicalc,
          colour="black",
          size=3) +
  geom_sf_text(data=temicalc,
               aes(label=avcalc),
               size=6,
               fontface="bold",
               nudge_x=c(-1250,
                        2000,
                        -1500,
                        -1500,
                        -1000,
                        1250,
                        -1250,
                        -1250,
                        1250,
                        1500,
                        1500,
                        -1250,
                        1500),
               nudge_y=c(0,
                         0,
                         0,
                         0,
                         -500,
                         750,
                         -500,
                         -500,
                         500,
                         -500,
                         500,
                         -500,
                         0)
               ) +
  coord_sf(xlim=c(xleft,xright),ylim=c(ybottom,ytop)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        #legend.title = element_text(size=12,face="bold"),
        #legend.position = c(0.855,0.62),
        #legend.text = element_text(size=10),
        panel.grid = element_line(colour="grey90")
  ) 

ggsave(
  filename = "figures/temiscouata_calcium_2024.png",
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




