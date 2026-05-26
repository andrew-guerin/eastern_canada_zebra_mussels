# This script generates Figure 3 for the Weise et al (2026) article
# This shows the size distributions of zebra mussels at a selection of sites in Lake Témiscouata
# It also generates Supplementary Figure 3, which includes all sites

library(tidyverse)
library(readxl) #chargement des fichiers au format xlsx
library(moments) #permet de caracteriser les courbes
library(PearsonDS) #simulation de la distribution des mz qui n'ont pas ete mesurees
library(ggpubr)

'%ni%'<-Negate('%in%') #fonction qui exclut les termes mentionnes

#Chargement des donnees

locations <- read_xlsx("data/source_data/sitelist.xlsx", sheet="benthic") %>% 
  rename(site = site_orig) %>%
  dplyr::select(site,site_id)

#2022 data
mz_2022 <- read_excel("data/source_data/temi_mussel_data.xlsx", sheet="data_2022",
                    col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric"), na = "NA") %>% 
  left_join(locations,by="site") %>%
  dplyr::select(-site) %>%
  unite("strate_id", c("site_id", "quadrat", "strate"), sep="-", remove = F)

#2023 data
mz_2023 <- read_excel("data/source_data/temi_mussel_data.xlsx", sheet="lengths_2023",
                    col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric"), na = "NA") %>% 
  left_join(locations,by="site") %>%
  dplyr::select(-site) %>%
  unite("strate_id", c("site_id", "quadrat", "strate"), sep="-", remove = F)

#2024 data
mz_2024 <- read_excel("data/source_data/temi_mussel_data.xlsx", sheet="data_2024",
                    col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric"), na = "NA") %>% 
  left_join(locations,by="site") %>%
  dplyr::select(-site) %>%
  unite("strate_id", c("site_id", "quadrat", "strate"), sep="-", remove = F)

#Manipulations des donnees
# l'augmentation drastique des densites a necessite des modifications aui protocole a chaque annee
# chacun des jeu de donnees ont ete manipule differemment vu la difference dans la facon de faire le decompte.
# Voir l'article (Weise et al 2026) pour les détails

# 2022
# all (most) mussels measured

hist_data_2022 <- mz_2022 %>% 
  group_by(site_id) %>% 
  mutate(pond=(nombre/taille_quadrat)/length(unique(quadrat))) #standardise a la fois par m2 et en moyenne pour le site

# quick peek at the data
ggplot(data=hist_data_2022, aes(x=longueur, weights=pond))+
  geom_histogram()+
  facet_wrap(~site_id, scales="free_y")

#2023####
# En 2023, les moules de moins de 5mm n'ont pas ete mesurees, seulement comptees, excepte pour un sous-echantillon
# qui a ete utilise pour simuler des donnees de longueurs pour le reste des petites moules. 
# De facon a ce que les histogrammes refletent les nombres reels.

# simulation of lengths for small mussels based on subsample
sim_5_2023 <- mz_2023 %>% 
  filter(longueur < 5) %>% 
  summarise(mean=mean(longueur, na.rm=T), 
            variance=var(longueur, na.rm=T),
            skewness=skewness(longueur, na.rm=T), 
            kurtosis=kurtosis(longueur, na.rm=T)) %>% #caracterisation de la courbe des longueurs
  as.vector() %>% 
  unlist() %>% 
  rpearson(n=mz_2023 %>% 
             filter(strate %in% "moins_5mm", nombre>=1) %>% summarise(sum(nombre)) %>% unlist(), 
           moments = .) #simulation des donnees

# developpement des comptes et remplacement des valeurs nulles de longueur par celles simulees
sim_5_long_2023 <- mz_2023 %>%
  filter(strate %in% "moins_5mm") %>% 
  uncount(nombre) %>% 
  mutate(longueur=ifelse(is.na(longueur), sim_5_2023, longueur)) %>% 
  mutate(nombre=1, .before = strate) 

# data for the measured mussels
mes_2023 <- mz_2023 %>% filter(!is.na(longueur) | nombre == 0) # pour garder les quadrats vides

hist_data_2023 <- rbind(mes_2023, sim_5_long_2023) %>% #fusion des longueurs mesurees et simulees
  group_by(site_id) %>% 
  mutate(pond=(nombre/taille_quadrat)/length(unique(quadrat))) #standardise a la fois par m2 et en moyenne pour le site

# quick peek
ggplot(data=hist_data_2023, aes(x=longueur, weights=pond))+
  geom_histogram()+
  facet_wrap(~site_id, scales="free_y")

# 2024
# Mussel counting protocol in 2024 was more complicated given the very large numbers sampled
# Briefly, samples were passed through a seive stack and a subsample of mussels from each fraction were measured
# see the manuscript for full details

#Creer des courbes pour chaque strate et les caracteriser
nmes_2024 <- mz_2024 %>% 
  group_by(site_id, quadrat, strate) %>% 
  filter(nombre>1, strate %ni% 0.50)

mes_sim_2024 <- mz_2024 %>% 
  filter(nombre<=1, strate_id %in% nmes_2024$strate_id) %>%
  group_by(strate_id) %>% 
  nest() %>% 
  left_join(nmes_2024, by="strate_id") %>% 
  mutate(descr=map(data, function(x) {
    x %>% 
      summarise(mean=mean(longueur), variance=var(longueur),skewness=skewness(longueur), kurtosis=kurtosis(longueur)) %>% 
      as.vector() %>% 
      unlist()
  })) %>% 
  mutate(sim_data=map((strate_id), function(x) { 
    rpearson(nombre, moments = descr[[1]])
  })) 

#Simuler des donnees pour les moules comptees
sim_data_long_2024 <- mes_sim_2024 %>% 
  unnest(cols=c(sim_data)) %>% 
  dplyr::select(annee, site_id, quadrat, longueur=sim_data, nombre, strate, taille_quadrat) %>% 
  mutate(nombre=1)

mes_data_long_2024 <- mz_2024 %>% filter(!is.na(longueur) | nombre == 0) # pour garder les quadrats vides

#donnees tamis 0.5
sim_05_2024<-mz_2024 %>% 
  filter(strate %in% 0.5) %>% 
  summarise(mean=mean(longueur, na.rm=T), 
            variance=var(longueur, na.rm=T),
            skewness=skewness(longueur, na.rm=T), 
            kurtosis=kurtosis(longueur, na.rm=T)) %>% 
  as.vector() %>% 
  unlist() %>% 
  rpearson(n=mz_2024 %>% 
             filter(strate %in% 0.5, nombre>1) %>% summarise(sum(nombre)), 
           moments = .)

sim_05_long_2024 <- mz_2024 %>%
  filter(strate %in% 0.5) %>% 
  uncount(nombre) %>% 
  mutate(longueur=ifelse(is.na(longueur), sim_05_2024, longueur)) %>% 
  mutate(nombre=1, .before = strate)

hist_data_2024 <- rbind(mes_data_long_2024, sim_data_long_2024, sim_05_long_2024) %>% # combine measured and simulated length data
  group_by(site_id) %>% 
  mutate(pond=(nombre/taille_quadrat)/length(unique(quadrat))) %>%  #standardise a la fois par m2 et en moyenne pour le site
  mutate(strate=as.character(strate))

# peek at the data
ggplot(data=hist_data_2024, aes(x=longueur, weights=pond))+
  geom_histogram()+
  facet_wrap(~site_id, scales="free_y")

# Combinaison des trois annees

# Plotting by Site 
# Define all site letters
site_letters <- LETTERS[1:16]  # A through P

# Create lists to store plots - one for the main text figure, one for the supplemental figure
plot_list <- list()

# Loop through each site and create a plot
# First for the main text figure
for(site in site_letters) {
  # Filter data for this site
  site_data <- filter(comb_hist_data, site_id==site)
  
  # Calculate total density for each year at this site
  year_totals <- site_data %>%
    group_by(annee, site_id) %>%
    summarise(total = sum(pond, na.rm = TRUE), .groups = 'drop')
  library(matrixStats)
  year_site_median<-site_data %>% 
    group_by(annee) %>% 
    summarise(median=weightedMedian(longueur, w = pond, na.rm = T))
  
  plot_list[[site]] <- 
    ggplot(data=site_data, 
           aes(x=longueur,
               weights=pond,
               fill=annee,
               colour=annee))+
    geom_histogram(binwidth=0.5, 
                   position="identity")+
    facet_grid(rows=vars(annee), 
               cols=vars(site_id),
               scales="free_y",
               drop = FALSE)+ 
    scale_fill_manual(values=c("white","grey40","black")) +  
    scale_colour_manual(values=c("black","grey40","black")) +  
    scale_x_continuous(name="Length (mm) ", 
                       limits=c(0,40), 
                       expand = c(0, 0),
                       minor_breaks = c(5,15,25,35)) + 
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_line(colour="grey85"),
          panel.grid.minor.x = element_line(colour="grey85",
                                            linetype=2),
          axis.line.x = element_line(colour="black"),
          axis.title.y = element_blank(),
          strip.background.x = element_rect(colour="black", fill=NA),
          strip.background.y = element_blank(),
          strip.text.y = element_blank(),
          legend.position = "none")
}

# to get the bar colours right, Site O has to be built separately 

siteOplot <- 
  ggplot(data=filter(comb_hist_data, site_id=="O"), 
         aes(x=longueur,
             weights=pond,
             fill=annee,
             colour=annee))+
  geom_histogram(binwidth=0.5, 
                 position="identity")+
  facet_grid(rows=vars(annee), 
             cols=vars(site_id),
             scales="free_y",
             drop = FALSE)+ 
  scale_fill_manual(values=c("white","black")) +  
  scale_colour_manual(values=c("black","black")) +  
  scale_x_continuous(name="Length (mm) ", 
                     limits=c(0,40), 
                     expand = c(0, 0),
                     minor_breaks = c(5,15,25,35)) + 
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour="grey85"),
        panel.grid.minor.x = element_line(colour="grey85",
                                          linetype=2),
        axis.line.x = element_line(colour="black"),
        axis.title.y = element_blank(),
        strip.background.x = element_rect(colour="black", fill=NA),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none")


# generate Figure 3
# note that since the size distribution data for 2023 and 2024 are partially simulated, they will come out slightly differently each time
# these plots will therefore not be totally identical to those presented in the manuscript
# this is more noticeable for 2024, where a greater proportion of the length data is simulated

msfig <- ggarrange(plot_list$B, plot_list$D, plot_list$J, plot_list$P,nrow=1)

ggsave(
    filename = "figures/figure3_mussel_lengths.png",
    plot = msfig,
    device = "png",
    path = NULL,
    scale = 1,
    width = 28,
    height = 20,
    units = "cm",
    dpi = 300,
    limitsize = TRUE,
    bg = "white")  
  
# now generate the three parts of the supplemental figure, which includes all sites

part1 <- ggarrange(plot_list$A, plot_list$B, plot_list$D, plot_list$E, plot_list$F,nrow=1)
part2 <- ggarrange(plot_list$G, plot_list$H, plot_list$I, plot_list$J, plot_list$K,nrow=1)
part3 <- ggarrange(plot_list$L, plot_list$M, plot_list$N, siteOplot, plot_list$P,nrow=1)

ggsave(
  filename = "figures/supp_figure_S3_part1.png",
  plot = part1,
  device = "png",
  path = NULL,
  scale = 1,
  width = 28,
  height = 20,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  

ggsave(
  filename = "figures/supp_figure_S3_part2.png",
  plot = part2,
  device = "png",
  path = NULL,
  scale = 1,
  width = 28,
  height = 20,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  

ggsave(
  filename = "figures/supp_figure_S3_part3.png",
  plot = part3,
  device = "png",
  path = NULL,
  scale = 1,
  width = 28,
  height = 20,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  