
library(tidyverse)
library(rnaturalearth)
library(sf)
library(lwgeom)
library(terra)
library(raster)
#library(janitor)
library(readxl)
library(ggnewscale)

crs1 <- 4326 # WGS84 lat-long
sf_use_s2(FALSE)

# prepare background map
# administrative outlines
provlims <- ne_states(country="canada", returnclass = "sf") %>%
  filter(name %in% c("Québec",
                     "New Brunswick",
                     "Prince Edward Island",
                     "Nova Scotia")) 

statlims <- ne_states(country="united states of america", returnclass = "sf") %>% 
  filter(name %in% c("Maine"))

terrs = rbind(provlims,statlims)

#location labels
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

# load St John catchment outline
bvsj <- read_sf("geodata/hybas/hybas_na_lev05_v1c.shp") %>% filter(HYBAS_ID == 7050038320)

# load St. John and Madawaska rivers
rivers<-ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf")
#lakes<-ne_download(scale = 10, type = "lakes", category = "physical", load = T, returnclass = "sf")

# load some rivers
rsj <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf") %>% filter(name=="Saint John")
# need to split St John into infested and non-infested segments

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

# final segment, Fredericton to ocean - presumed uninfested
rsj3 <- st_split(rsj,latcut1) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_tail(n=1) %>% 
  st_split(latcut2) %>% 
  st_collection_extract("LINESTRING") %>% 
  slice_tail(n=1)

madawaska <- st_read("geodata/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Rivière Madawaska")
matapedia <- st_read("geodata/GRHQ_01AI_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Rivière Matapédia")

tobique <- st_read("geodata/nbrivers.gdb") %>% filter(NAME1 == "Tobique River")
meduxnekeag <- st_read("geodata/nbrivers.gdb") %>% filter(NAME1 == "Meduxnekeag River")
kennebecasis <- st_read("geodata/nbrivers.gdb") %>% filter(NAME1 == "Kennebecasis River")
hammond <- st_read("geodata/nbrivers.gdb") %>% filter(NAME1 == "Hammond River")
petitcodiac <- st_read("geodata/nbrivers.gdb") %>% filter(NAME1 == "Petitcodiac River")
restigouche <- st_read("geodata/nbrivers.gdb") %>% filter(grepl("Ristigouche",NAME1) | grepl("Restigouche",NAME1)) %>% st_transform(crs=crs1) %>% filter(WATERDEFINITION==6)
miramichi <- st_read("geodata/nbrivers.gdb") %>% filter(grepl("Miramichi",NAME1))

# Set x and y limits for the plot, to use to filter lakes,
# set the crs to match other layers
#box_coords <- tibble(x = c(-71,-64), y = c(45,49.4)) %>% 
#  st_as_sf(coords = c("x", "y")) %>% 
#  st_set_crs(st_crs(bvsj))

#get the bounding box of the two x & y coordintates, make sfc
#bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

#read in lakes data, filter using the bounding box and restrict to a minimum size 
#select_lakes <- read_sf("geodata/hydrolakes/HydroLAKES_polys_v10.gdb") %>% 
#  st_intersection(bounding_box) %>% 
#  filter(Lake_area > 0.39)

#select_lakes2 <- select_lakes %>% filter(Shape_Area > 0.0001)

#get lakes within catchment of Saint John River, along with Témiscouata and a few others elsewhere in BSL
#plotlakes_bvsj <- select_lakes %>% st_intersection(bvsj)
#plotlakes_temi <- select_lakes %>% filter(Hylak_id == 8487)
#plotlakes_other <- select_lakes %>% 
#  filter(Hylak_id %in% c(8304,
#                         8342,
#                         103312,
                         #103487,
#                         104031,
#                         103699,
#                         103378))

# Detection summary plot
# first, gather data for lakes

edna_summary <- read_xlsx("source_data/2025_data_version/edna_2324.xlsx",sheet="combined") %>%
  filter(type == "lake") %>%
  rename(waterbody = location,
         lat_e = lat,
         long_e = long) %>%
  mutate(edna = case_when(yrs_detected == "both" ~ 2,
                          yrs_detected == "2023" ~ 1,
                          yrs_detected == "2024" ~ 1,
                          yrs_detected == "none" ~ 0)) %>%
  dplyr::select(waterbody,edna,lat_e,long_e)

coll_summary <- read_xlsx("source_data/2025_data_version/collectors_2324.xlsx", sheet="waterbodies") %>%
  rename(lat_c = lat,
         long_c = long) %>%
  mutate(settle = case_when(result == "no detection" ~ 0,
                            result == "detection" ~ 1)) %>%
  dplyr::select(waterbody,settle,lat_c,long_c)

vel_summary <- read_xlsx("source_data/2025_data_version/veligers.xlsx") %>%
  rename(lat_v = lat,
         long_v = long) %>%
  mutate(waterbody = case_when(location == "barrage_temis" ~ "Témiscouata",
                               TRUE ~ location),
         veligers = case_when(waterbody == "Témiscouata" ~ 2,
                              detected == "yes" ~ 1,
                              detected == "no" ~ 0)) %>%
  filter(who == "melccfp" | waterbody == "Témiscouata") %>%
  dplyr::select(waterbody,veligers,lat_v,long_v)

adult_summary <- read_xlsx("source_data/2025_data_version/adults_visual.xlsx") %>%
  filter(type == "lake") %>%
  rename(waterbody = where,
         lat_a = lat,
         long_a = long) %>%
  mutate(adults = case_when(detected == "no" ~ 0,
                            detected == "yes" ~ 1)) %>%
  dplyr::select(waterbody,adults,lat_a,long_a)

#create detection summary

detection_summary <- edna_summary %>%
  full_join(coll_summary, by="waterbody") %>%
  full_join(vel_summary, by="waterbody") %>%
  full_join(adult_summary, by="waterbody") %>%
  mutate(ES = case_when(!is.na(edna) ~ 1, TRUE ~ 0),
         NS = case_when(!is.na(veligers) ~ 1, TRUE ~ 0),
         CS = case_when(!is.na(settle) ~ 1, TRUE ~ 0),
         VS = case_when(!is.na(adults) ~ 1, TRUE ~ 0),
         n_methods = ES + NS + CS + VS) %>%
  mutate(lat = case_when(is.na(lat_e) ~ lat_c,TRUE ~ lat_e),
         long = case_when(is.na(long_e) ~ long_c,TRUE ~ long_e)) %>%
  mutate(status = case_when(adults == 1 ~ "A",
                            edna + veligers >= 2 ~ "B",
                            edna == 1 | veligers == 1 ~ "C",
                            TRUE ~ "D")) %>%
  mutate(status2 = case_when(adults >= 1 | veligers >= 1 ~ "A",
                            edna >= 1 ~ "B",
                            TRUE ~ "C")) %>%
  dplyr::select(waterbody,edna,veligers,settle,adults,status,status2,n_methods,lat,long) %>%
  st_as_sf(coords=c("long","lat"),crs=4326)

#detection_plot <- 
#  ggplot() + 
#  geom_sf(data=terrs,
#          fill="palegreen",
#          colour="grey50") + 
#  geom_sf(data=rsj1, linewidth=1, colour="white") +
#  geom_sf(data=rsj2, linewidth=1, colour="black") +
#  geom_sf(data=rsj3, linewidth=1, colour="white") +
#  geom_sf(data=madawaska, linewidth=0.5, colour="black") +
#  geom_sf(data=matapedia, linewidth=0.5, colour="white") +
#  geom_sf(data=tobique, linewidth=0.5, colour="white") +
#  geom_sf(data=meduxnekeag, linewidth=0.5, colour="white") +
#  geom_sf(data=kennebecasis, linewidth=0.5, colour="white") +
#  geom_sf(data=hammond, linewidth=0.5, colour="white") +
#  geom_sf(data=petitcodiac, linewidth=0.5, colour="white") +
#  geom_sf(data=restigouche, linewidth=0.5, colour="white") + 
#  geom_sf(data=miramichi, linewidth=0.5, colour="white") + 
#  geom_sf(data=detection_summary,
#          aes(fill=status),
#          shape=23,
#          size=4,
#          stroke=0.5) + 
#  geom_sf(data=filter(detection_summary, status=="C"),
#          fill="grey75",
#          shape=23,
#          size=4,
#          stroke=0.5) + 
#  geom_sf(data=filter(detection_summary, status=="B"),
#          fill="grey35",
#          shape=23,
#          size=4,
#          stroke=0.5) + 
#  geom_sf(data=filter(detection_summary, status=="A"),
#          fill="black",
#          shape=23,
#          size=4,
#          stroke=0.5) + 
#  scale_fill_manual("Zebra mussel status",
#                    values=c("black","grey35","grey75", "white"),
#                    labels=c("Established",
#                             "Probably established",
#                             "Detected",
#                             "Undetected")) +
#  #coord_sf(xlim=c(-70.5,-64.9),ylim=c(45.2,48.7),expand=FALSE) +
#  coord_sf(xlim=c(-70.6,-59.5),ylim=c(43.25,49.35),expand=FALSE) +
#  theme_bw() +
#  theme(panel.grid=element_blank(),
#        legend.position = "inside",
#        legend.justification = c(0.98,0.97),
#        legend.text = element_text(size=12),
#        legend.title = element_text(size=14),
#        legend.background = element_rect(colour="black")) 

#ggsave(
#  filename = "figures/detection_summary.png",
#  plot = detection_plot,
#  device = "png",
#  path = NULL,
#  scale = 1,
#  width = 20,
#  height = 16,
#  units = "cm",
#  dpi = 300,
#  limitsize = TRUE,
#  bg = "white")  


#calcium version
#load rasters, reproject to lat-long and mask 
calcrast <- raster("geodata/rasters/calcium-KR-97648-median-10km-ZN.tif")

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
  mutate(catmed = cut(calcium, c(0,7.999,11.999,19.999,24.999,500), labels=c("< 8 (very low)",
                                                                             "8-12 (low)",
                                                                             "12-20 (moderate)",
                                                                             "20-25 (high)",
                                                                             "> 25 (very high)"))) %>%
  mutate(catmedTR = cut(calcium, c(0,14.999,19.999,23.999,500), labels=c("< 15 (negligible)",
                                                                         "15-19 (low to moderate)",
                                                                         "20-24 (moderate to high)",
                                                                         "> 24 (high)"))) %>%
  mutate(catmedAW = cut(calcium, c(0,11.999,19.999,24.999,500), labels=c("< 12 (negligible)",
                                                                         "12-19 (low to moderate)",
                                                                         "20-25 (moderate to high)",
                                                                         "> 25 (very high)"))) 

detection_plot_calcium <- 
  ggplot() + 
  geom_tile(data = calcinterdat, aes(x=x,
                                     y=y,
                                     fill=catmedAW,
                                     colour=catmedAW
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
          aes(fill=status2),
          shape=23,
          size=3,
          stroke=0.25) + 
  #geom_sf(data=filter(detection_summary, status=="C"),
  #        fill="grey75",
  #        shape=23,
  #        size=3,
  #        stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="B"),
          fill="grey50",
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="A"),
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
  filename = "figures/detection_summary_calcium.png",
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

detection_plot_calcium_TR <- 
  ggplot() + 
  geom_tile(data = calcinterdat, aes(x=x,
                                     y=y,
                                     fill=catmedTR,
                                     colour=catmedTR
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
          aes(fill=status2),
          shape=23,
          size=3,
          stroke=0.25) + 
  #geom_sf(data=filter(detection_summary, status=="C"),
  #        fill="grey75",
  #        shape=23,
  #        size=3,
  #        stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="B"),
          fill="grey50",
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="A"),
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
  filename = "figures/detection_summary_calcium_alt.png",
  plot = detection_plot_calcium_TR,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 20,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  


# establishment risk (based on Wilcox calcium model)

#load raster, reproject to lat-long and mask 
estrast <- raster("geodata/zmra_layers/ZM_Calcium_Establishment_Potential.tif")

#create template raster with desired properties (5 minute resolution)
rast_temp <- raster(xmn=-180,xmx=-45,ymn=20,ymx=89, resolution=0.0833, crs=crs1)

#reproject raw rasters into latitude-longitude
#you may get some warning messages: "Point outside of projection domain (GDAL error 1)" but there is no indication that this has caused any problems in the resulting rasters
est_latlong <- estrast %>% projectRaster(crs = crs1)

#resample to get raster with equal x and y resolutions (5 minutes) using template
est_latlong_5m <- raster::resample(est_latlong,rast_temp,method="bilinear")

#mask the rasters
est_latlong_5m_masked <- mask(est_latlong_5m, terrs) 

#prep raster data for plotting
estadat <- as.data.frame(rasterToPoints(est_latlong_5m_masked)) %>% rename(estap=3) 

detection_plot_establishment <- 
  ggplot() + 
  geom_tile(data = estadat, aes(x=x,
                                y=y,
                                fill=estap,
                                #colour=estap
  )) +
  scale_fill_viridis_c("Risk of Establishment") +
  #scale_colour_viridis_c("Calcium, mg/L (invasion risk)") +
  geom_sf(data=terrs,
          fill=NA,
          colour="grey50") + 
  guides(fill = guide_legend(order=1)) +
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
  geom_sf(data=pointers, linewidth=1.25, colour="red") +
  geom_sf(data=loclabs_marks, shape=21, colour="black", fill="red") +
  geom_sf_label(data=loclabs_label,size=4,aes(label=site)) +
  new_scale_fill() +  
  geom_sf(data=detection_summary,
          aes(fill=status2),
          shape=23,
          size=3,
          stroke=0.25) + 
  #geom_sf(data=filter(detection_summary, status=="C"),
  #        fill="grey75",
  #        shape=23,
  #        size=3,
  #        stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="B"),
          fill="grey50",
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="A"),
          fill="black",
          shape=23,
          size=3,
          stroke=0.25) + 
  #geom_sf(data=detection_summary,
  #        aes(fill=status),
  #        shape=23,
  #        size=2,
  #        stroke=0.25) + 
  #geom_sf(data=filter(detection_summary, status=="C"),
  #        fill="grey75",
  #        shape=23,
  #        size=2,
  #        stroke=0.25) + 
  #geom_sf(data=filter(detection_summary, status=="B"),
  #        fill="grey35",
  #        shape=23,
  #        size=2,
  #        stroke=0.25) + 
  #geom_sf(data=filter(detection_summary, status=="A"),
  #        fill="black",
  #        shape=23,
  #        size=2,
  #        stroke=0.25) + 
  #scale_fill_manual("Zebra mussel status (lakes)",
  #                  values=c("black","grey35","grey75", "white"),
  #                  labels=c("Established",
  #                           "Probably established",
  #                           "Detected",
  #                           "Undetected")) +
  scale_fill_manual("Zebra mussel status (lakes)",
                    values=c("black","grey50","white"),
                    labels=c("Established",
                             "Detected",
                             "Not detected")) +
  coord_sf(xlim=c(-70.6,-59.5),ylim=c(43.25,49.35),expand=FALSE) +
  #coord_sf(xlim=c(-70.5,-64.9),ylim=c(45.2,48.7),expand=FALSE)  +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        #legend.justification = c(0.98,0.98),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        #legend.background = element_rect(colour="black"),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.spacing.y = unit(0.01,"cm")
        ) +
  guides(fill = guide_legend(override.aes = list(size=5)))

ggsave(
  filename = "figures/detection_summary_establishment_risk.png",
  plot = detection_plot_establishment,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 18,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  

# establishment risk (based on Wilcox MaxEnt model)

#load raster, reproject to lat-long and mask 
estrast_me <- raster("geodata/zmra_layers/ZM_MaxEnt_Establishment_Potential.tif")

#create template raster with desired properties (5 minute resolution)
rast_temp <- raster(xmn=-180,xmx=-45,ymn=20,ymx=89, resolution=0.0833, crs=crs1)

#reproject raw rasters into latitude-longitude
#you may get some warning messages: "Point outside of projection domain (GDAL error 1)" but there is no indication that this has caused any problems in the resulting rasters
est_latlong_me <- estrast_me %>% projectRaster(crs = crs1)

#resample to get raster with equal x and y resolutions (5 minutes) using template
est_latlong_me_5m <- raster::resample(est_latlong_me,rast_temp,method="bilinear")

#mask the rasters
est_latlong_me_5m_masked <- mask(est_latlong_me_5m, terrs) 

#prep raster data for plotting
estadat_me <- as.data.frame(rasterToPoints(est_latlong_me_5m_masked)) %>% rename(estap=3) 

detection_plot_establishment_maxent <- 
  ggplot() + 
  geom_tile(data = estadat_me, aes(x=x,
                                   y=y,
                                   fill=estap,
                                   #colour=estap
  )) +
  scale_fill_viridis_c("Risk of Establishment") +
  #scale_colour_viridis_c("Calcium, mg/L (invasion risk)") +
  geom_sf(data=terrs,
          fill=NA,
          colour="grey50") + 
  guides(fill = guide_legend(order=1)) +
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
  new_scale_fill() +  
  geom_sf(data=detection_summary,
          aes(fill=status2),
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="B"),
          fill="grey50",
          shape=23,
          size=3,
          stroke=0.25) + 
  geom_sf(data=filter(detection_summary, status2=="A"),
          fill="black",
          shape=23,
          size=3,
          stroke=0.25) + 
  scale_fill_manual("Zebra mussel status (lakes)",
                    values=c("black","grey50","white"),
                    labels=c("Established",
                             "Detected",
                             "Not detected")) +
  coord_sf(xlim=c(-70.6,-59.5),ylim=c(43.25,49.35),expand=FALSE) +
  #coord_sf(xlim=c(-70.5,-64.9),ylim=c(45.2,48.7),expand=FALSE)  +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        #legend.justification = c(0.98,0.98),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        #legend.background = element_rect(colour="black"),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.spacing.y = unit(0.01,"cm")
  ) +
  guides(fill = guide_legend(override.aes = list(size=5)))

ggsave(
  filename = "figures/detection_summary_establishment_risk_maxent.png",
  plot = detection_plot_establishment_maxent,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 18,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  
