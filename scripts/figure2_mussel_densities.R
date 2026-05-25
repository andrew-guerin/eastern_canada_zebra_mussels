
library(tidyverse)
library(readxl)

library(sf)
library(terra)
library(ggspatial)

'%ni%' <- Negate('%in%') 
crs1 <- 4326 # WGS84 lat-long

# load basemap data, obtained from https://www.donneesquebec.ca/recherche/dataset/grhq
temi <- st_read("data/geodata/GRHQ_01AE_GRP.gdb",layer="RH_S") %>% filter(TOPONYME == "Lac Témiscouata") 

# get site locations
locations <- read_xlsx("data/source_data/sitelist.xlsx", sheet="benthic") %>% 
  mutate(latitude_lett = case_when(site_id == "A" ~ latitude - 0.001, # these will be the positions of the letter labels on the plot 
                                   site_id == "B" ~ latitude,
                                   site_id == "C" ~ latitude - 0.001,
                                   site_id == "D" ~ latitude,
                                   site_id == "E" ~ latitude,
                                   site_id == "F" ~ latitude,
                                   site_id == "G" ~ latitude,
                                   site_id == "H" ~ latitude - 0.006,
                                   site_id == "I" ~ latitude,
                                   site_id == "J" ~ latitude,
                                   site_id == "K" ~ latitude + 0.006,
                                   site_id == "L" ~ latitude + 0.0025,
                                   site_id == "M" ~ latitude + 0.006,
                                   site_id == "N" ~ latitude - 0.005,
                                   site_id == "O" ~ latitude,
                                   site_id == "P" ~ latitude),
         longitude_lett = case_when(site_id == "A" ~ longitude + 0.0075,
                                    site_id == "B" ~ longitude - 0.0075,
                                    site_id == "C" ~ longitude + 0.0075,
                                    site_id == "D" ~ longitude - 0.0075,
                                    site_id == "E" ~ longitude + 0.0075,
                                    site_id == "F" ~ longitude + 0.0075,
                                    site_id == "G" ~ longitude + 0.0075,
                                    site_id == "H" ~ longitude,
                                    site_id == "I" ~ longitude + 0.0075,
                                    site_id == "J" ~ longitude - 0.0075,
                                    site_id == "K" ~ longitude,
                                    site_id == "L" ~ longitude + 0.0075,
                                    site_id == "M" ~ longitude,
                                    site_id == "N" ~ longitude + 0.0025,
                                    site_id == "O" ~ longitude - 0.0075,
                                    site_id == "P" ~ longitude + 0.0075))

locations_sf <- locations %>% st_as_sf(coords=c("longitude","latitude"),crs=crs1) 
locations_lett_sf <- locations %>% st_as_sf(coords=c("longitude_lett","latitude_lett"),crs=crs1) 

#import the data 

data22 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2022") %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre),
            density=total/0.25) %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(n22=n(),
            mndens22 = mean(density))

data23 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="counts_2023") %>%
  dplyr::select(2:5) %>%
  rowwise() %>%
  mutate(adults=as.numeric(nb_gros) / 0.25,
         juvs=as.numeric(nb_5mm) / 0.25, 
         total=juvs + adults) %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(n23=n(),
            mndens23 = mean(total))

quadlist24 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="lengths_2024") %>% 
  dplyr::select(site,quadrat,taille_quadrat) %>%
  distinct()

data24 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="lengths_2024") %>%
  dplyr::select(site,quadrat,nombre) %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre)) %>%
  ungroup() %>%
  left_join(quadlist24) %>%
  mutate(density= total / taille_quadrat) %>%
  group_by(site) %>%
  summarise(n24=n(),
            mndens24 = mean(density))

data_all <- locations %>%
  left_join(data22) %>%
  left_join(data23) %>%
  left_join(data24)

data_long <- data22 %>% 
  mutate(year="2022") %>%
  rename(n=n22,
         mndens=mndens22,
         sddens=sddens22,
         mddens=mddens22) %>%
  bind_rows(data23 %>% 
              mutate(year="2023") %>%
              rename(n=n23,
                     mndens=mndens23,
                     sddens=sddens23,
                     mddens=mddens23)) %>%
  bind_rows(data24 %>% 
              mutate(year="2024") %>%
              rename(n=n24,
                     mndens=mndens24,
                     sddens=sddens24,
                     mddens=mddens24)) %>%
  mutate(year=as.factor(year),
         col=case_when(year=="2022" ~ "white",
                       year=="2023" ~ "grey",
                       year=="2024" ~ "black")) %>%
  left_join(locations) %>% 
  dplyr::select(-latitude_adj,-longitude_adj) %>%
  #arrange(-mndens) %>%
  mutate(latitude_adj = case_when(year==2022 & site_new == "A" ~ latitude,
                                  year==2022 & site_new == "B" ~ latitude,
                                  year==2022 & site_new == "C" ~ latitude,
                                  year==2022 & site_new == "D" ~ latitude,
                                  year==2022 & site_new == "E" ~ latitude,
                                  year==2022 & site_new == "F" ~ latitude,
                                  year==2022 & site_new == "G" ~ latitude,
                                  year==2022 & site_new == "H" ~ latitude,
                                  year==2022 & site_new == "I" ~ latitude,
                                  year==2022 & site_new == "J" ~ latitude,
                                  year==2022 & site_new == "K" ~ latitude,
                                  year==2022 & site_new == "L" ~ latitude,
                                  year==2022 & site_new == "M" ~ latitude - 0.01,
                                  year==2022 & site_new == "N" ~ latitude,
                                  year==2022 & site_new == "O" ~ latitude,
                                  year==2022 & site_new == "P" ~ latitude,
                                  year==2023 & site_new == "A" ~ latitude,
                                  year==2023 & site_new == "B" ~ latitude,
                                  #year==2023 & site_new == "C" ~ latitude,
                                  year==2023 & site_new == "D" ~ latitude,
                                  #year==2023 & site_new == "E" ~ latitude,
                                  year==2023 & site_new == "F" ~ latitude,
                                  year==2023 & site_new == "G" ~ latitude,
                                  year==2023 & site_new == "H" ~ latitude,
                                  year==2023 & site_new == "I" ~ latitude,
                                  year==2023 & site_new == "J" ~ latitude,
                                  year==2023 & site_new == "K" ~ latitude,
                                  year==2023 & site_new == "L" ~ latitude,
                                  #year==2023 & site_new == "M" ~ latitude,
                                  year==2023 & site_new == "N" ~ latitude,
                                  #year==2023 & site_new == "O" ~ latitude,
                                  year==2023 & site_new == "P" ~ latitude,
                                  year==2024 & site_new == "A" ~ latitude,
                                  year==2024 & site_new == "B" ~ latitude,
                                  #year==2024 & site_new == "C" ~ latitude,
                                  year==2024 & site_new == "D" ~ latitude,
                                  #year==2024 & site_new == "E" ~ latitude,
                                  year==2024 & site_new == "F" ~ latitude,
                                  year==2024 & site_new == "G" ~ latitude,
                                  year==2024 & site_new == "H" ~ latitude,
                                  year==2024 & site_new == "I" ~ latitude,
                                  year==2024 & site_new == "J" ~ latitude,
                                  #year==2024 & site_new == "K" ~ latitude,
                                  year==2024 & site_new == "L" ~ latitude,
                                  #year==2024 & site_new == "M" ~ latitude,
                                  year==2024 & site_new == "N" ~ latitude,
                                  year==2024 & site_new == "O" ~ latitude,
                                  year==2024 & site_new == "P" ~ latitude),
         longitude_adj = case_when(year==2022 & site_new == "A" ~ longitude - 0.015,
                                   year==2022 & site_new == "B" ~ longitude + 0.0175,
                                   year==2022 & site_new == "C" ~ longitude - 0.015,
                                   year==2022 & site_new == "D" ~ longitude + 0.0175,
                                   year==2022 & site_new == "E" ~ longitude - 0.015,
                                   year==2022 & site_new == "F" ~ longitude - 0.015,
                                   year==2022 & site_new == "G" ~ longitude - 0.015,
                                   year==2022 & site_new == "H" ~ longitude + 0.0175,
                                   year==2022 & site_new == "I" ~ longitude - 0.015,
                                   year==2022 & site_new == "J" ~ longitude + 0.0175,
                                   year==2022 & site_new == "K" ~ longitude - 0.015,
                                   year==2022 & site_new == "L" ~ longitude - 0.015,
                                   year==2022 & site_new == "M" ~ longitude,
                                   year==2022 & site_new == "N" ~ longitude + 0.02,
                                   year==2022 & site_new == "O" ~ longitude + 0.0175,
                                   year==2022 & site_new == "P" ~ longitude - 0.015,
                                   year==2023 & site_new == "A" ~ longitude - 0.02,
                                   year==2023 & site_new == "B" ~ longitude + 0.024,
                                   #year==2023 & site_new == "C" ~ longitude + 0,
                                   year==2023 & site_new == "D" ~ longitude + 0.025,
                                   #year==2023 & site_new == "E" ~ longitude + 0,
                                   year==2023 & site_new == "F" ~ longitude - 0.024,
                                   year==2023 & site_new == "G" ~ longitude - 0.02,
                                   year==2023 & site_new == "H" ~ longitude + 0.022,
                                   year==2023 & site_new == "I" ~ longitude - 0.023,
                                   year==2023 & site_new == "J" ~ longitude + 0.0275,
                                   year==2023 & site_new == "K" ~ longitude - 0.02,
                                   year==2023 & site_new == "L" ~ longitude - 0.024,
                                   #year==2023 & site_new == "M" ~ longitude + 0,
                                   year==2023 & site_new == "N" ~ longitude + 0.0265,
                                   #year==2023 & site_new == "O" ~ longitude + 0.02,
                                   year==2023 & site_new == "P" ~ longitude - 0.021,
                                   year==2024 & site_new == "A" ~ longitude - 0.0265,
                                   year==2024 & site_new == "B" ~ longitude + 0.033,
                                   #year==2024 & site_new == "C" ~ longitude + 0,
                                   year==2024 & site_new == "D" ~ longitude + 0.039,
                                   #year==2024 & site_new == "E" ~ longitude + 0,
                                   year==2024 & site_new == "F" ~ longitude - 0.0365,
                                   year==2024 & site_new == "G" ~ longitude - 0.0265,
                                   year==2024 & site_new == "H" ~ longitude + 0.036,
                                   year==2024 & site_new == "I" ~ longitude - 0.043,
                                   year==2024 & site_new == "J" ~ longitude + 0.0565,
                                   #year==2024 & site_new == "K" ~ longitude + 0,
                                   year==2024 & site_new == "L" ~ longitude - 0.039,
                                   #year==2024 & site_new == "M" ~ longitude + 0,
                                   year==2024 & site_new == "N" ~ longitude + 0.037,
                                   year==2024 & site_new == "O" ~ longitude + 0.028,
                                   year==2024 & site_new == "P" ~ longitude - 0.030)) 

data_long_sf <- data_long %>% st_as_sf(coords=c("longitude_adj","latitude_adj"),crs=crs1) %>% filter(site_new != "C")
data_c <- data_long %>% filter(site_new == "C") %>% st_as_sf(coords=c("longitude_adj","latitude_adj"),crs=crs1) 

# these next lines create pointers to run from the adjusted pie chart positions to the locations of the sites
pointers_start <- locations %>% 
  dplyr::select(longitude,latitude) %>%
  rowid_to_column() %>% 
  st_as_sf(coords=c("longitude","latitude"), crs=crs1) 

pointers_end <- data_long %>% 
  filter(year==2022) %>%
  arrange(site_new) %>%
  dplyr::select(longitude_adj,latitude_adj) %>%
  rowid_to_column() %>% 
  st_as_sf(coords=c("longitude_adj","latitude_adj"), crs=crs1) 

pointers <- bind_rows(pointers_start,pointers_end) %>%
  group_by(rowid) %>% 
  summarize() %>%
  st_cast("LINESTRING") 

temi_bubble <-    
  ggplot() +
  geom_sf(data=temi,
          fill="lightblue",
          colour="grey25",
          linewidth=0.25) +
  geom_sf(data=pointers) +    
  geom_sf(data=locations_sf,
          colour="black") +  
  geom_sf_text(data=locations_sf2,
               colour="black",
               fontface="bold",
               aes(label=site_new)) +  
  geom_sf(data=data_long_sf,
          aes(size=mndens,
              fill=year),
              shape=21) +
  geom_sf_text(data=data_c,
               colour="black",
               size=4,
               aes(label="X")) +  
  scale_fill_manual("Year",values=c("white","grey","black")) +
  scale_size(bquote("Mean density,\nmussels per m"^2),
             range=c(1,25),
             breaks=c(1,100,1000,10000,30000)
             #transform="sqrt"
  ) +  
  annotation_scale() +  
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_text(size=12),
        legend.position = c(0.84,0.7),
        legend.text = element_text(size=10),
        panel.grid = element_blank()) +
  guides(size = guide_legend(order=1),
         fill = guide_legend(override.aes = list(size = 5),
                             order=2))

ggsave(
  filename = "figures/temiscouata_abundances_bubble2.png",
  plot = temi_bubble,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 19,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  


