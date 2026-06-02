# This script generates Figure 9 for the Weise et al (2026) article
# Shows links between Témiscouata and other locations, which were named by surveyed boaters
# It also generates the list of sites, and the distances from Témiscouata to those sites, for Supplementary Table T6

library(tidyverse)
library(readxl)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(ggpubr)
library(writexl)

'%ni%' = Negate('%in%')

crs1 <- 4326 # WGS84 lat-long
sf_use_s2(FALSE)

## prepare basemap

# administrative boundaries

canada <- readRDS("data/geodata/gadm/gadm41_CAN_1_pk.rds") %>% unwrap() %>% st_as_sf(crs=4326) %>% dplyr::select(NAME_1)

quebec <- canada %>% filter(NAME_1 == "Québec") 
newbrun <- canada %>% filter(NAME_1 == "New Brunswick")

ontario <- canada %>% filter(NAME_1 == "Ontario")
novascot <- canada %>% filter(NAME_1 == "Nova Scotia")
princeed <- canada %>% filter(NAME_1 == "Prince Edward Island")

usa <- readRDS("data/geodata/gadm/gadm41_USA_1_pk.rds") %>% unwrap() %>% st_as_sf(crs=4326) %>% dplyr::select(NAME_1)

maine <- usa %>% filter(NAME_1 == "Maine")
newhamp <- usa %>% filter(NAME_1 == "New Hampshire")
newyork <- usa %>% filter(NAME_1 == "New York")
vermont <- usa %>% filter(NAME_1 == "Vermont")

otherterrs <- bind_rows(maine,vermont,newhamp,newyork,ontario,novascot,princeed)

# lakes
# uses Hydrolakes data from https://www.hydrosheds.org/products/hydrolakes 

# Set x and y limits for the plot, to use to filter lakes,
# set the crs to match other layers
lakebox1_coords <- tibble(x = c(-77.5,-63), y = c(43.5,51)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(crs1))

#get the bounding box of the two x & y coordinates, make sfc
lakebox1 <- st_bbox(lakebox1_coords) %>% st_as_sfc()

lakes1 <- read_sf("data/geodata/hydrolakes/HydroLAKES_polys_v10.gdb") %>% 
  st_intersection(lakebox1) %>% 
  filter(Lake_area > 1)

# set the crs to match other layers
lakebox2_coords <- tibble(x = c(-69.75,-65), y = c(46.8,48.7)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(crs1))

#get the bounding box of the two x & y coordinates, make sfc
lakebox2 <- st_bbox(lakebox2_coords) %>% st_as_sfc()

lakes2 <- read_sf("data/geodata/hydrolakes/HydroLAKES_polys_v10.gdb") %>% 
  st_intersection(lakebox2) %>% 
  filter(Lake_area > 0.2)

# Rivers
# Saint John and St. Lawrence directly via rnaturalearth package

rsj <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf") %>% filter(name=="Saint John")
stlaw <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", load = T, returnclass = "sf") %>% filter(name=="Saint Lawrence") 

# Madawaska and Matapédia rivers using data from https://www.donneesquebec.ca/recherche/dataset/grhq
madawaska <- st_read("data/geodata/quebec_lakes_rivers/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Rivière Madawaska")
matapedia <- st_read("data/geodata/quebec_lakes_rivers/GRHQ_01AI_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Rivière Matapédia")

# Some New Brunswick Rivers using data from https://www.gnb.ca/en/campaign/geonb/data-catalogue/hydrographic-network.html
tobique <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Tobique River")
meduxnekeag <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Meduxnekeag River")
kennebecasis <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Kennebecasis River")
hammond <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Hammond River")
petitcodiac <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(NAME1 == "Petitcodiac River")
restigouche <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(grepl("Ristigouche",NAME1) | grepl("Restigouche",NAME1)) %>% st_transform(crs=crs1) %>% filter(WATERDEFINITION==6)
miramichi <- st_read("data/geodata/nb_watercourse.gdb") %>% filter(grepl("Miramichi",NAME1))

# load survey data
survey_data <- read_xlsx("data/source_data/temi_surveydata.xlsx", sheet="destinations") 
  
# load the list of destination locations
positions <- 
  read_xlsx("data/source_data/temi_surveydata.xlsx", sheet="positions") %>% 
  mutate(type=case_when(waterbody=="Témiscouata"~"Témiscouata", TRUE~type),
         type=fct_relevel(as.factor(type),"Témiscouata","lake","river","brackish","marine"))

# gather a list of destinations that are marine, or that are territories / areas rather than specific locations - these will not be considered further
nonspec_or_marine <- 
  read_xlsx("data/source_data/temi_surveydata.xlsx", sheet="positions") %>% 
  filter(specificity == "area" | type == "marine") %>% 
  dplyr::select(waterbody)

# temiscouata links
# links identified at Témiscouata
temi_outlinks <- survey_data %>%
  filter(waterbody=="Témiscouata") %>%
  rename(lakeA = waterbody,
         lakeZ = destinations) %>%
  dplyr::select(lakeA,lakeZ) %>% 
  mutate(lakeB = str_split_i(lakeZ,"; ",1),
         lakeC = str_split_i(lakeZ,"; ",2),
         lakeD = str_split_i(lakeZ,"; ",3),
         lakeE = str_split_i(lakeZ,"; ",4),
         lakeF = str_split_i(lakeZ,"; ",5),
         lakeG = str_split_i(lakeZ,"; ",6)) %>% #ensure that last column (lakeG here) is always empty. Add another column if not
  pivot_longer(cols = c(3:8),values_to = "lakeB") %>%
  filter(!is.na(lakeB)) %>%
  dplyr::select(lakeA,lakeB) %>%
  rename(lake1=lakeA,lake2=lakeB) 

# links identified at other sites
temi_inlinks <- survey_data %>%
  filter(grepl("Témiscouata",destinations)) %>%
  rename(lake2 = waterbody,
         lake1 = destinations) %>%
  dplyr::select(lake1,lake2) 

# combine to get a list of all links
temilinks_all <- bind_rows(temi_outlinks,temi_inlinks) %>%
  filter(lake2 %ni% nonspec_or_marine$waterbody) %>%
  group_by(lake1,lake2) %>%
  summarise(n=n()) %>%
  left_join(positions %>% rename(lake1=waterbody) %>% dplyr::select(lake1,lat,long)) %>%
  rename(lat1 = lat,long1 = long) %>%
  left_join(positions %>% rename(lake2=waterbody) %>% dplyr::select(lake2,lat,long)) %>%
  rename(lat2 = lat,long2 = long) %>%
  mutate(ncnt = as.factor(case_when(n == 1 ~ "1",
                                      n > 1 & n < 5 ~ "2 to 4",
                                      n > 4 ~ "5 or more")))

# positions to be used for plotting 
positions_inregion <- positions %>% 
  filter(region %in% c("Bas-Saint-Laurent",
                       "Gaspésie-Îles-de-la-Madeleine",
                       "New Brunswick")) %>%
  filter(waterbody == "Témiscouata" |
         waterbody %in% temilinks_all$lake2) %>%
  st_as_sf(crs=4326,coords=c("long","lat")) 

positions_outregion <- positions %>% 
  filter(waterbody == "Témiscouata" |
         waterbody %in% temilinks_all$lake2) %>%
  filter(waterbody == "Témiscouata" |
         region %ni% c("Bas-Saint-Laurent",
                       "Gaspésie-Îles-de-la-Madeleine",
                       "New Brunswick")) %>%
  st_as_sf(crs=4326,coords=c("long","lat")) 

# prepare links for plotting
# convert lake links to sf

links_temi_ids <- temilinks_all %>% ungroup() %>% rowid_to_column() %>% dplyr::select(1:4,9) 

links_temi_start <- temilinks_all %>% 
  ungroup() %>%
  rowid_to_column() %>% 
  dplyr::select(1,5,6) %>%
  mutate(type="start") %>%
  st_as_sf(coords=c("long1","lat1"), crs=4326) 

links_temi_end <- temilinks_all %>% 
  ungroup() %>%
  rowid_to_column() %>% 
  dplyr::select(1,7,8) %>%
  mutate(type="end") %>%
  st_as_sf(coords=c("long2","lat2"), crs=4326) 

links_temi <- bind_rows(links_temi_start,links_temi_end) %>%
  group_by(rowid) %>% 
  summarize() %>%
  st_cast("LINESTRING") %>% 
  left_join(links_temi_ids) 

links_outregion <- links_temi %>% filter(lake2 %in% positions_outregion$waterbody)
links_inregion <- links_temi %>% filter(lake2 %in% positions_inregion$waterbody)

# time to build the figures!

outregion_plot <- 
  ggplot() +
  geom_rect(aes(xmin=-75,xmax=-71.2,ymin=43.6,ymax=50.3),fill="deepskyblue",colour="NA") +
  geom_sf(data = otherterrs, 
          inherit.aes = FALSE,
          fill="grey60",
          colour="grey35",
          linewidth=0.25) +
  geom_sf(data = quebec,  
          inherit.aes = FALSE,
          fill="grey95",
          colour="grey35",
          linewidth=0.25) +
  geom_sf(data = newbrun,  
          inherit.aes = FALSE,
          fill="grey95",
          colour="grey35",
          linewidth=0.25) +
  geom_sf(data=rsj, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=tobique, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=meduxnekeag, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=kennebecasis, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=hammond, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=petitcodiac, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=stlaw, linewidth=0.5, colour="deepskyblue") +
  geom_sf(data=lakes1,
          fill="deepskyblue",
          colour="black", 
          linewidth=0.01) +
  geom_sf(data = links_outregion,
          inherit.aes = FALSE,
          aes(linetype=ncnt)) +
  geom_sf(data=positions_outregion,
          inherit.aes = FALSE,
          aes(shape=type,fill=type,size=type)) +
  scale_shape_manual(name="Site type",
                     labels=c("Témiscouata","lake","river","brackish"),
                     values=c(23,21,25,24)) +
  scale_fill_manual(name="Site type",
                    labels=c("Témiscouata","lake","river","brackish"),
                    values=c("yellow","green","violet","grey")) +
  scale_size_manual(name="Site type",
                    labels=c("Témiscouata","lake","river","brackish"),
                    values=c(4,3,3,3)) +
  scale_linetype_manual(name = "Number of surveys",
                        values=c(3,2,1)) +
  geom_rect(aes(xmin=-69.75,xmax=-65,ymin=46.8,ymax=48.7),fill="NA",linetype=2,colour="grey35") +
  annotation_scale(location="br") +
  coord_sf(xlim=c(-77.5,-63),ylim=c(43.5,51),expand=FALSE) +  
  theme(panel.grid=element_blank(),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(colour="black",fill=NA),
        axis.title = element_blank(),
        axis.text = element_text(size=8),
        legend.box.background = element_rect(colour="black"),
        legend.title = element_text(size=10,face="bold"),
        legend.text = element_text(size=10),
        legend.key.size = unit(0.35, "cm"),
        legend.key = element_rect(color = NA, fill = NA),
        legend.justification.inside = c(0.01,0.98),
        legend.location = "plot",
        legend.background = element_blank()) +
  guides(shape = guide_legend(override.aes = list(size = 2), position = "inside"),
         fill = guide_legend(override.aes = list(size = 2), position = "inside"),
         linetype = guide_legend(override.aes = list(size = 2), position = "inside"))

inregion_plot <- 
  ggplot() +
    geom_sf(data = otherterrs, 
            inherit.aes = FALSE,
            fill="grey60",
            colour="grey35",
            linewidth=0.25) +
    geom_sf(data = quebec,  
            inherit.aes = FALSE,
            fill="grey95",
            colour="grey35",
            linewidth=0.25) +
    geom_sf(data = newbrun,  
            inherit.aes = FALSE,
            fill="grey95",
            colour="grey35",
            linewidth=0.25) +
    geom_sf(data=lakes2,
            inherit.aes = FALSE,
            fill="deepskyblue",
            colour="grey35",
            linewidth=0.025) +
    geom_sf(data=madawaska, linewidth=0.5, colour="deepskyblue") +
    geom_sf(data=matapedia, linewidth=0.5, colour="deepskyblue") +
    geom_sf(data=miramichi, linewidth=0.5, colour="deepskyblue") +
    geom_sf(data=restigouche, linewidth=0.5, colour="deepskyblue") +
    geom_sf(data=rsj, linewidth=0.5, colour="deepskyblue") +
    geom_sf(data = links_inregion,
            inherit.aes = FALSE,
            linewidth=0.75,
            colour="black",
            aes(linetype=ncnt)) +
    geom_sf(data=positions_inregion,
            inherit.aes = FALSE,
            aes(fill=type,shape=type,size=type)) +
    scale_shape_manual(name="Site type",
                       labels=c("Témiscouata","lake","river","brackish"),
                       values=c(23,21,25,24)) +
    scale_size_manual(name="Site type",
                      labels=c("Témiscouata","lake","river","brackish"),
                      values=c(4,3,3,3)) +
    scale_fill_manual(name="Site type",
                      labels=c("Témiscouata","lake","river","brackish"),
                      values=c("yellow","green","violet","grey")) +
    scale_linetype_manual(name = "Number of surveys",
                          values=c(3,2,1)) +
    annotation_scale(location="br") +
    coord_sf(xlim=c(-69.75,-65.),ylim=c(46.8,48.7),expand=FALSE) +  
    theme(panel.grid=element_blank(),
          panel.background = element_rect(fill="white"),
          panel.border = element_rect(colour="black",fill=NA),
          axis.title = element_blank(),
          axis.text = element_text(size=8),
          legend.title = element_text(size=16,face="bold"),
          legend.text = element_text(size=16),
          legend.key.size = unit(0.5, "cm"),
          legend.key = element_rect(color = NA, fill = NA),
          legend.justification.inside = c(0.975,0.95),
          legend.location = "plot",
          legend.box.background = element_rect(colour = "black"),
          legend.background = element_blank(),
          plot.margin = margin(-1,1,0,1,"cm")
          ) +
    guides(shape = guide_legend(override.aes = list(size = 4), position = "inside"),
           fill = guide_legend(override.aes = list(size = 4), position = "inside"),
           linetype = guide_legend(override.aes = list(size = 4,linewidth=0.4), position = "inside"))
  

combined_linksmap <- ggarrange(outregion_plot,inregion_plot,
                               ncol=1,nrow=2,
                               labels=c("a","b"),
                               font.label=list(size=30))

ggsave(
  filename = "figures/figure9_linksmap.png", 
  plot = combined_linksmap,
  device = "png",
  path = NULL,
  scale = 1,
  width = 22,
  height = 29,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  

## Assemble link list for Supplementary Table T6
# calculate distances between pairs of lakes

lakenames_col <- positions %>% 
  filter(waterbody %ni% nonspec_or_marine$waterbody) %>%
  dplyr::select(waterbody)

lakenames_row <- lakenames_col %>% t() %>% as.data.frame()

distances_matrix <- positions %>%
  filter(waterbody %ni% nonspec_or_marine$waterbody) %>%
  st_as_sf(coords = c("long","lat"), crs=4326) %>%
  st_distance() %>% 
  t() %>% 
  as_tibble() 

colnames(distances_matrix) <- lakenames_row[1,]

distances <- lakenames_col %>% 
  bind_cols(distances_matrix) %>%
  pivot_longer(!waterbody, names_to = "lakeB", values_to = "distance") %>%
  mutate(distance = round(as.numeric(distance / 1000))) %>%
  filter(distance > 0) %>%
  rowwise() %>%
  mutate(lake1 = min(waterbody,lakeB),
         lake2 = max(waterbody,lakeB)) %>%
  dplyr::select(-waterbody,-lakeB) %>%
  distinct() %>%
  filter(lake1=="Témiscouata" | lake2=="Témiscouata") %>%
  mutate(destination=case_when(lake1=="Témiscouata" ~ lake2,
                               lake2=="Témiscouata" ~ lake1))

distances_dests <- distances %>% 
  dplyr::select(destination,distance) %>% 
  left_join(positions %>% 
              rename(destination=waterbody) %>%
              dplyr::select(destination,type,lat,long))

temilinks_external <- links_outregion %>% 
  rename(destination=lake2) %>%
  left_join(distances_dests) %>%
  as.data.frame() %>%
  dplyr::select(destination,type,lat,long,distance,n) 
  
temilinks_regional <- links_inregion %>% 
  rename(destination=lake2) %>%
  left_join(distances_dests) %>%
  as.data.frame() %>%
  dplyr::select(destination,type,lat,long,distance,n) 

write_xlsx(temilinks_external,"data/temiscouata_external_links.xlsx")
write_xlsx(temilinks_regional,"data/temiscouata_regional_links.xlsx")
