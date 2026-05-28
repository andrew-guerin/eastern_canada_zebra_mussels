library(tidyverse)
library(readxl)

tempdat <- read_xlsx("data/source_data/temperature_madawaska.xlsx") %>%
  rename(id = 1,
         date_time = 2,
         temp = 3) %>%
  mutate(date=date(date_time),
         year_mnth=paste(year(date_time),month(date_time), sep="-")) 

temp_day <- tempdat %>%
  group_by(date) %>%
  summarise(n=n(),
            mntemp=mean(temp))

tplot <- 
  ggplot(data=temp_day) +
  geom_point(aes(x=date,y=mntemp)) +
  geom_line(aes(x=date,y=mntemp)) +
  geom_smooth(aes(x=date,y=mntemp),
              method="loess", span=0.25) +
  geom_hline(yintercept = 12) + 
  scale_x_date(date_breaks = "month") +
  ylab("Daily mean temperature (°C)") +
  xlab("Date") +
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=270,
                                   vjust=0.25))

ggsave(
  filename = "figures/S5_madawaska_temperature.png",
  plot = tplot,
  device = "png",
  path = NULL,
  scale = 1,
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = "white")  
