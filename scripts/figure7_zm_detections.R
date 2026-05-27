# this script generates Figure 7 from the Weise et al (2026) article - ZM detections from 2022-2024
# the figure displays all sites investigated using a) eDNA sampling, b) net sampling for veligers, c) settlement plates, and d) visual surveys
# Sites are marked positive (at least one detection in at least one year) or negative (not detected)

library(tidyverse)
library(rnaturalearth)
library(sf)
library(readxl)
library(ggpubr)

#library(terra)
#library(raster)

crs1 <- 4326 # WGS84 lat-long
sf_use_s2(FALSE)

## prepare basemap

# gather administrative outlines
provlims <- ne_states(country="canada", returnclass = "sf") %>%
  filter(name %in% c("Québec",
                     "New Brunswick",
                     "Prince Edward Island",
                     "Nova Scotia")) 

statlims <- ne_states(country="united states of america", returnclass = "sf") %>% 
  filter(name %in% c("Maine"))

terrs = rbind(provlims,statlims)

# load catchment outlines (bassins versants) for Saint John, Matapédia-Restigouche, and Miramichi rivers, using data from https://www.hydrosheds.org/products/hydrobasins 
bvsj <- read_sf("data/geodata/hybas/hybas_na_lev05_v1c.shp") %>% filter(HYBAS_ID == 7050038320)
bvmr <- read_sf("data/geodata/hybas/hybas_na_lev05_v1c.shp") %>% filter(HYBAS_ID == 7050035470)
bvmi <- read_sf("data/geodata/hybas/hybas_na_lev05_v1c.shp") %>% filter(HYBAS_ID == 7050035860)

# load river data
# Saint John River from rnaturalearth package 
rsj <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf") %>% filter(name=="Saint John")

# Madawaska and Matapédia rivers using data from https://www.donneesquebec.ca/recherche/dataset/grhq
madawaska <- st_read("data/geodata/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Rivière Madawaska")
matapedia <- st_read("data/geodata/GRHQ_01AI_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Rivière Matapédia")

# Some New Brunswick Rivers using data from https://www.gnb.ca/en/campaign/geonb/data-catalogue/hydrographic-network.html
tobique <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Tobique River")
meduxnekeag <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Meduxnekeag River")
kennebecasis <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Kennebecasis River")
hammond <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Hammond River")
petitcodiac <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Petitcodiac River")
restigouche <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(grepl("Ristigouche",NAME1) | grepl("Restigouche",NAME1)) %>% st_transform(crs=crs1) %>% filter(WATERDEFINITION==6)
miramichi <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(grepl("Miramichi",NAME1))

# load lake data
# uses Hydrolakes data from https://www.hydrosheds.org/products/hydrolakes 

# Set x and y limits for the plot, to use to filter lakes
# this is not essential but will speed up subsequent operations because we only need to work with data for the region, rather than the global dataset
# set the crs to match other layers
box_coords <- tibble(x = c(-71,-64), y = c(45,49.4)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(bvsj))

# get the bounding box of the two x & y coordinates, make sfc
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

# read in lakes data, filter using the bounding box and restrict to a minimum size
# this step is quite slow...
select_lakes <- read_sf("data/geodata/hydrolakes/HydroLAKES_polys_v10.gdb") %>% 
  st_intersection(bounding_box) %>% 
  filter(Lake_area > 0.39)

# for the plot, get just lakes within the featured river catchments
plotlakes_bvsj <- select_lakes %>% st_intersection(bvsj)
plotlakes_bvmr <- select_lakes %>% st_intersection(bvmr)
plotlakes_bvmi <- select_lakes %>% st_intersection(bvmi)
#plotlakes_temi <- select_lakes %>% filter(Hylak_id == 8487)
#plotlakes_other <- select_lakes %>% 
#  filter(Hylak_id %in% c(8304,
#                         8342,
#                         103312,
#                         #103487,
#                         104031,
#                         103699,
#                         103378))

# quick plot to check that we have everything we need for the basemap
ggplot() + 
  geom_sf(data=terrs,fill="grey95") + 
  geom_sf(data=bvsj, linetype=3,fill=NA,linewidth=1,colour="black") +
  geom_sf(data=bvmr, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=bvmi, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=plotlakes_bvsj,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmr,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmi,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=tobique, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=hammond, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=rsj, linewidth=1, colour="blue") +
  coord_sf(xlim=c(-70.5,-64.9),ylim=c(45,48.75),expand=FALSE) +
  theme_bw() +
  theme(panel.grid=element_blank())

## build subplots
# Load eDNA data and create subplot a)

edna_data <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx",sheet="edna_2023_2024") %>%
  mutate(result = case_when(yrs_detected %in% c("2023","2024","both") ~ "detection",
                            yrs_detected == "none" ~ "no detection")) %>%
  st_as_sf(coords=c("long","lat"),crs=crs1)

ednaplot <- 
  ggplot() + 
  geom_sf(data=terrs,fill="grey95") + 
  geom_sf(data=bvsj, linetype=3,fill=NA,linewidth=1,colour="black") +
  geom_sf(data=bvmr, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=bvmi, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=plotlakes_bvsj,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmr,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmi,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=rsj, linewidth=1, colour="blue") +
  geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=tobique, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=hammond, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=edna_data,aes(fill=result),
          shape=21,
          size=3,
          stroke=1) +
  scale_fill_manual("eDNA",
                    labels=c("detected","not detected"),
                    values=c("black","white")) +
  coord_sf(xlim=c(-70.5,-64.9),ylim=c(45,48.75),expand=FALSE) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position = "inside",
        legend.justification = c(0.98,0.98),
        legend.text = element_text(size=18),
        legend.title = element_text(size=14),
        legend.key.spacing.y = unit(0.25,"cm"),
        legend.background = element_rect(colour="black")) +
  guides(fill = guide_legend(order=1))

# Load veliger data and create subplot b)

#veligers
vel_data <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx", sheet="veligers_2023_2024") %>%
  mutate(detected=as.factor(detected) %>% fct_relevel("yes","no")) %>%
  st_as_sf(coords=c("long","lat"),crs=4326) 

velplot <- 
  ggplot() + 
  geom_sf(data=terrs,fill="grey95") + 
  geom_sf(data=bvsj, linetype=3,fill=NA,linewidth=1,colour="black") +
  geom_sf(data=bvmr, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=bvmi, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=plotlakes_bvsj,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmr,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmi,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=rsj, linewidth=1, colour="blue") +
  geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=tobique, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=hammond, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=vel_data,
          aes(fill=detected),
          shape=25,size=4,
          stroke=0.5) +
  scale_fill_manual("Veligers",
                    values=c("black","white"),
                    labels=c("detected","not detected")) +
  coord_sf(xlim=c(-70.5,-64.9),ylim=c(45,48.75),expand=FALSE) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        legend.position = "inside",
        legend.justification = c(0.98,0.98),
        legend.text = element_text(size=18),
        legend.title = element_text(size=14),
        legend.key.spacing.y = unit(0.25,"cm"),
        legend.background = element_rect(colour="black")) +
  guides(fill = guide_legend(order=1,override.aes = list(colour="black",stroke=1)))

# Load collector data and build subplot c)
coll_data <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx", sheet="collectors_2023_2024") %>%
  mutate(result = as.factor(result)) %>%
  st_as_sf(coords=c("long","lat"),crs=4326)

collplot <- 
  ggplot() + 
  geom_sf(data=terrs,fill="grey95") + 
  geom_sf(data=bvsj, linetype=3,fill=NA,linewidth=1,colour="black") +
  geom_sf(data=bvmr, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=bvmi, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=plotlakes_bvsj,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmr,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmi,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=rsj, linewidth=1, colour="blue") +
  geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=tobique, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=hammond, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=coll_data, 
          aes(fill=result),
          shape=22,size=5,
          stroke=0.5) +
  scale_fill_manual("Juveniles",
                    labels=c("detected","not detected"),
                    values=c("black","white")) +
  coord_sf(xlim=c(-70.5,-64.9),ylim=c(45,48.75),expand=FALSE) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position = "inside",
        legend.justification = c(0.98,0.98),
        legend.text = element_text(size=18),
        legend.title = element_text(size=14),
        legend.key.spacing.y = unit(0.25,"cm"),
        legend.background = element_rect(colour="black")) +
  guides(fill = guide_legend(order=1,override.aes = list(colour="black",stroke=1)))

# Load adult (visual survey etc) data and build subplot d)
adult_data <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx", sheet="visual_2023_2024") %>%
  mutate(detected=as.factor(detected) %>% fct_relevel("yes","no")) %>%
  st_as_sf(coords=c("long","lat"),crs=4326)   

visplot <- 
  ggplot() + 
  geom_sf(data=terrs,fill="grey95") + 
  geom_sf(data=bvsj, linetype=3,fill=NA,linewidth=1,colour="black") +
  geom_sf(data=bvmr, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=bvmi, linetype=3,fill=NA,linewidth=0.75) +
  geom_sf(data=plotlakes_bvsj,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmr,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=plotlakes_bvmi,fill="deepskyblue",colour="deepskyblue") +
  geom_sf(data=rsj, linewidth=1, colour="blue") +
  geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=tobique, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=hammond, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") + 
  geom_sf(data=adult_data,
          shape="\u2726",
          colour="white",
          size=7,
          stroke=1) +
  geom_sf(data=adult_data,
          aes(shape=detected),
          colour="black",
          size=7,
          stroke=1) +
  scale_shape_manual("Adults",
                     values=c("\u2726","\u2727"),
                     labels=c("detected","not detected")) +
  coord_sf(xlim=c(-70.5,-64.9),ylim=c(45,48.75),expand=FALSE) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        legend.position = "inside",
        legend.justification = c(0.98,0.98),
        legend.text = element_text(size=18),
        legend.title = element_text(size=14),
        legend.key.spacing.y = unit(0.25,"cm"),
        legend.background = element_rect(colour="black")) 

# build the complete 2x2 plot

combined_plot <- ggarrange(ednaplot,velplot,collplot,visplot,
                           labels=c("a","b","c","d"),
                           font.label = list(size=38),
                           vjust = 1.75,
                           hjust = -2.5)

ggsave(
  filename = "figures/figure7_zmdetections.png",
  plot = combined_plot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 40,
  height = 38,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  



