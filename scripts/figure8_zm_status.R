# This script generates Figure 8 from the Weise et al (2026) article
# Shows zebra mussel status underlaid with freshwater calcium concentration data
# Calcium data from Guerin et al 2024. High-resolution freshwater dissolved calcium and pH data layers for Canada and the United States. Sci Data 11(1): 370. doi:10.1038/s41597-024-03165-8.

library(tidyverse)
library(rnaturalearth)
library(sf)
library(readxl)
library(lwgeom)
library(raster)
library(ggnewscale)

crs1 <- 4326 # WGS84 lat-long
sf_use_s2(FALSE)

# prepare basemap

# administrative outlines
provlims <- ne_states(country="canada", returnclass = "sf") %>%
  filter(name %in% c("Québec",
                     "New Brunswick",
                     "Prince Edward Island",
                     "Nova Scotia")) 

statlims <- ne_states(country="united states of america", returnclass = "sf") %>% 
  filter(name %in% c("Maine"))

terrs = rbind(provlims,statlims)

# load Saint John River
rsj <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf") %>% filter(name=="Saint John")

# need to split St John into presumed infested and non-infested segments for plotting

latcut1 <- st_linestring(matrix(c(-68.35, 47, -68.35, 48), nrow = 2, byrow = T), dim = "XY") %>% st_sfc(crs = 4326) %>% st_as_sf()
latcut2 <- st_linestring(matrix(c(-66.63, 45.5, -66.63, 46.5), nrow = 2, byrow = T), dim = "XY") %>% st_sfc(crs = 4326) %>% st_as_sf()

# first segment (upstream of Edmunston, no ZM detections)
rsj1 <- st_split(rsj,latcut1) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_head(n=1)

# second section, Edmunston to Fredericton, ZM present (throughout?)
rsj2 <- st_split(rsj,latcut1) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_tail(n=1) %>% 
  st_split(latcut2) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_head(n=1)

# final segment, Fredericton to ocean - currently presumed uninfested
rsj3 <- st_split(rsj,latcut1) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_tail(n=1) %>% 
  st_split(latcut2) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_tail(n=1)

# Other sampled river systems
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

# create some location labels
loclabs <- data.frame(site = c("Edmundston",
                               "Mactaquac",
                               "Beechwood",
                               "Nackawic",
                               "Grand Falls"),
                      long = c(-68.32430,-66.86883,-67.66979,-67.23224,-67.74073),
                      lat = c(47.36317,45.95482,46.54210,45.99569,47.05151),
                      long_lab = c(-67.5,-66.75,-68.75,-68.5,-68.75),
                      lat_lab = c(47.65,46.5,46.54210,45.99569,47))

loclabs_marks <- loclabs %>% st_as_sf(coords=c("long","lat"),crs=crs1)
loclabs_label <- loclabs %>% st_as_sf(coords=c("long_lab","lat_lab"),crs=crs1) 

#pointers
pointers_start <- loclabs %>% 
  dplyr::select(long,lat) %>%
  rowid_to_column() %>% 
  st_as_sf(coords=c("long","lat"), crs=crs1) 

pointers_end <- loclabs %>% 
  dplyr::select(long_lab,lat_lab) %>%
  rowid_to_column() %>% 
  st_as_sf(coords=c("long_lab","lat_lab"), crs=crs1) 

pointers <- bind_rows(pointers_start,pointers_end) %>%
  group_by(rowid) %>% 
  summarize() %>%
  st_cast("LINESTRING") 

# Gather data for lakes

edna_summary <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx",sheet="edna_2023_2024") %>%
  filter(type == "lake") %>%
  mutate(edna = case_when(yrs_detected %in% c("2023","2024","both") ~ "detection",
                          yrs_detected == "none" ~ "no detection")) %>%
  rename(waterbody = location) %>%
  dplyr::select(waterbody,edna,lat,long)

vel_summary <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx",sheet="veligers_2023_2024") %>%
  rename(lat_v = lat,
         long_v = long) %>%
  mutate(waterbody = case_when(location == "barrage_temis" ~ "Témiscouata",
                               TRUE ~ location),
         veligers = case_when(detected == "yes" ~ "detection",
                              detected == "no" ~ "no detection")) %>%
  filter(who == "melccfp" | waterbody == "Témiscouata") %>%
  dplyr::select(waterbody,veligers)

coll_summary <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx",sheet="collectors_waterbody_2023_2024") %>%
  rename(lat_c = lat,
         long_c = long,
         settle = result) %>%
  dplyr::select(waterbody,settle,lat_c,long_c)

adult_summary <- read_xlsx("data/source_data/zm_watershed_sampling.xlsx",sheet="visual_2023_2024") %>%
  filter(type == "lake") %>%
  rename(waterbody = where,
         lat_a = lat,
         long_a = long) %>%
  mutate(adults = case_when(detected == "yes" ~ "detection",
                            detected == "no" ~ "no detection")) %>%
  dplyr::select(waterbody,adults)

# create detection summary

detection_summary <- edna_summary %>%
  full_join(vel_summary, by="waterbody") %>%
  full_join(coll_summary, by="waterbody") %>%
  full_join(adult_summary, by="waterbody") %>%
  mutate(lat = case_when(is.na(lat) ~ lat_c,TRUE ~ lat),
         long = case_when(is.na(long) ~ long_c,TRUE ~ long)) %>%
  mutate(status = case_when(adults == "detection" | veligers == "detection" ~ "A",
                            edna == "detection" ~ "B",
                            TRUE ~ "C")) %>%
  dplyr::select(waterbody,edna,veligers,settle,adults,status,lat,long) %>%
  st_as_sf(coords=c("long","lat"),crs=4326)

# Get calcium data for background

# load interpolated calcium raster from Guerin et al 2024. 
calcrast <- raster("data/geodata/calcium_raster/calcium-KR-97648-median-10km-ZN.tif")

#create template raster with desired properties (5 minute resolution)
rast_temp <- raster(xmn=-180,xmx=-45,ymn=20,ymx=89, resolution=0.0833, crs=crs1)

#reproject raw rasters into latitude-longitude
#you may get some warning messages: "Point outside of projection domain (GDAL error 1)" but there is no indication that this has caused any problems in the resulting rasters
calc_latlong <- calcrast %>% projectRaster(crs = crs1)

#resample to get raster with equal x and y resolutions (5 minutes) using template
calc_latlong_5m <- raster::resample(calc_latlong,rast_temp,method="bilinear")

#mask the rasters
calc_latlong_5m_masked <- mask(calc_latlong_5m, terrs)

#prep calcium data for plotting
calcinterdat <- as.data.frame(rasterToPoints(calc_latlong_5m_masked)) %>%
  rename(calcium=3) %>%
  mutate(catmed = cut(calcium, c(0,14.999,19.999,23.999,500), labels=c("< 15 (negligible)",
                                                                         "15-19 (low to moderate)",
                                                                         "20-24 (moderate to high)",
                                                                         "> 24 (high)"))) 

detection_plot_calcium <- 
  ggplot() + 
  geom_tile(data = calcinterdat, aes(x=x,
                                     y=y,
                                     fill=catmed,
                                     colour=catmed
  )) +
  scale_fill_manual("Calcium, mg/L (invasion risk)", values=c("green2","orange2","red2","red4")) +
  scale_colour_manual("Calcium, mg/L (invasion risk)", values=c("green2","orange2","red2","red4")) +
  geom_sf(data=terrs,
          fill=NA,
          colour="grey50") + 
  geom_sf(data=rsj1, linewidth=1, colour="white") +
  geom_sf(data=rsj2, linewidth=1, colour="black") +
  geom_sf(data=rsj3, linewidth=1, colour="white") +
  geom_sf(data=madawaska, linewidth=0.5, colour="black") +
  geom_sf(data=matapedia, linewidth=0.5, colour="white") +
  geom_sf(data=tobique, linewidth=0.5, colour="white") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="white") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="white") +
  geom_sf(data=hammond, linewidth=0.5, colour="white") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="white") +
  geom_sf(data=restigouche, linewidth=0.5, colour="white") + 
  geom_sf(data=miramichi, linewidth=0.5, colour="white") + 
  geom_sf(data=rsj1, linewidth=1, colour="white") +
  geom_sf(data=rsj2, linewidth=1, colour="black") +
  geom_sf(data=rsj3, linewidth=1, colour="white") +
  geom_sf(data=pointers, linewidth=1.25, colour="yellow") +
  geom_sf(data=loclabs_marks, shape=21, colour="black", fill="yellow") +
  geom_sf_label(data=loclabs_label,size=4,aes(label=site)) +
  new_scale_fill() +  
  geom_sf(data=detection_summary,
          aes(fill=status),
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status=="B"),
          fill="grey50",
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status=="A"),
          fill="black",
          shape=23,
          size=3,
          stroke=0.25) + 
  scale_fill_manual("Zebra mussel status (lakes)",
                    values=c("black","grey50","white"),
                    labels=c("Established",
                             "Detected",
                             "Not detected")) +
  #coord_sf(xlim=c(-70.5,-64.9),ylim=c(45.2,48.7),expand=FALSE) +
  coord_sf(xlim=c(-70.6,-59.5),ylim=c(43.25,49.35),expand=FALSE) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        #legend.justification = c(0.02,0.02),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        #legend.background = element_rect(colour="black"),
        legend.direction = "vertical"
        ) +
  guides(fill = guide_legend(override.aes = list(size=5)))

ggsave(
  filename = "figures/figure8_zmstatus.png",
  plot = detection_plot_calcium,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 20,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  

