# this script generates Supplemental Figure S1 - size distributions of zebra mussels on plant stems in October 2022 and 2023

library(tidyverse)
library(readxl)

#mussel length data
stem_size_data <- read_xlsx("data/source_data/plant_data.xlsx", sheet="mussel_lengths") %>%
  mutate(size_mm = round(as.numeric(size_mm),digits=2),
         year = as.factor(year),
         monyr = as.factor(str_to_title(paste(month,year, sep=" "))),
         monyr = fct_relevel(monyr,
                             'Oct 2022',
                             'Oct 2023')) 

sizedist_plot <-   
  ggplot(data=stem_size_data,
         aes(x=size_mm)) +
  geom_histogram(binwidth = 0.25) +
  geom_vline(
      data = . %>%
        group_by(monyr) %>%
        summarise(line = median(size_mm)),
      mapping = aes(xintercept = line),
      linetype=2
    ) +
  facet_wrap(~monyr) + 
  xlab("Length, mm") +
  theme(panel.background = element_blank(),
        #panel.border = element_rect(colour="black",fill=NA),
        panel.grid.major.x = element_line(colour="grey85"),
        axis.line.x = element_line(colour="black"),
        axis.title.y = element_blank(),
        strip.background.x = element_rect(colour="black", fill=NA),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none")

ggsave(
  filename = "figures/S1_mussel_sizes_plants.png",
  plot = sizedist_plot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 10,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  
