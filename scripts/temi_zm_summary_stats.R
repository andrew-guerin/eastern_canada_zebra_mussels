# this script calculates summary statistics used to populate Table 2 for the Weise et al (2026) article

library(tidyverse)
library(readxl)

## Calculate average and maximum mussel densities recorded at each site in each year 

locations <- read_xlsx("data/source_data/temi_sitelist.xlsx", sheet="benthic") %>% rename(site = site_orig) %>% dplyr::select(site,site_id)

data22 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2022") %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre),
            density=total/0.25) %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(n=n(),
            mndens = mean(density) %>% signif(digits=3),
            mxdens = max(density))

data23 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="counts_2023") %>%
  dplyr::select(2:5) %>%
  rowwise() %>%
  mutate(adults=as.numeric(nb_gros) / 0.25,
         juvs=as.numeric(nb_5mm) / 0.25, 
         total=juvs + adults) %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(n=n(),
            mndens = mean(total) %>% signif(digits=3),
            mxdens = max(total))

quadlist24 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2024") %>% 
  dplyr::select(site,quadrat,taille_quadrat) %>%
  distinct()

data24 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2024") %>%
  dplyr::select(site,quadrat,nombre) %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre)) %>%
  ungroup() %>%
  left_join(quadlist24) %>%
  mutate(density= total / taille_quadrat) %>%
  group_by(site) %>%
  summarise(n=n(),
            mndens = mean(density) %>% signif(digits=3),
            mxdens = max(density))

data_combined <- data22 %>% mutate(year="2022") %>%
  bind_rows(data23 %>% mutate(year="2023")) %>%
  bind_rows(data24 %>% mutate(year="2024")) %>%
  pivot_wider(names_from = year,
              values_from = c(n,mndens,mxdens)) %>%
  left_join(locations)

# calculate lake-wide statistics

lakewide22 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2022") %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre),
            density=total/0.25) %>%
  ungroup() %>%
  summarise(n=n(),
            mndens = mean(density) %>% signif(digits=3),
            mxdens = max(density))

lakewide23 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="counts_2023") %>%
  dplyr::select(2:5) %>%
  rowwise() %>%
  mutate(adults=as.numeric(nb_gros) / 0.25,
         juvs=as.numeric(nb_5mm) / 0.25, 
         total=juvs + adults) %>%
  ungroup() %>%
  summarise(n=n(),
            mndens = mean(total) %>% signif(digits=3),
            mxdens = max(total))

lakewide24 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2024") %>%
  dplyr::select(site,quadrat,nombre) %>%
  group_by(site,quadrat) %>%
  summarise(total=sum(nombre)) %>%
  ungroup() %>%
  left_join(quadlist24) %>%
  mutate(density= total / taille_quadrat) %>%
  summarise(n=n(),
            mndens = mean(density) %>% signif(digits=3),
            mxdens = max(density))

## Calculate maximum sizes

big22 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2022") %>%
  left_join(locations) %>%
  group_by(site_id) %>%
  summarise(biggest22=max(as.numeric(longueur),na.rm=TRUE) %>% round(digits=1)) %>%
  dplyr::select(site_id,biggest22)

big23 <- read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="lengths_2023") %>%
  filter(!(site %in% c(4,8,10,15))) %>%
  left_join(locations) %>%
  group_by(site_id) %>%
  summarise(biggest23=max(as.numeric(longueur),na.rm=TRUE) %>% round(digits=1)) %>%
  dplyr::select(site_id,biggest23)

big24 <-  read_xlsx("data/source_data/temi_mussel_data.xlsx", sheet="data_2024") %>%
  group_by(site) %>%
  left_join(locations) %>%
  group_by(site_id) %>%
  summarise(biggest24=max(as.numeric(longueur),na.rm=TRUE) %>% round(digits=1)) %>%
  dplyr::select(site_id,biggest24)

biggest_mussels <- big22 %>%
  left_join(big23) %>%
  left_join(big24)

# the lake-wide maximum length is simply the largest mussel reported at any site in any given year

