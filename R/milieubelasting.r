# MPFF logo

options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(readxl)
library(patchwork)
library(cowplot)
library(magick)
library(treemapify)

source("R/my utils.r")

mypath <- "C:/TEMP"

data <- 
  readxl::read_xlsx(path=file.path(mypath, "Milieubelasting Voedingsmiddelen.xlsx")) %>% 
  lowcase() %>% 
  group_by(productgroep) %>% 
  mutate(meankgco2eq = mean(kgco2eq, na.rm=TRUE)) 

hull <- 
  data %>%
  group_by(productgroep) %>% 
  slice(chull(kgco2eq, kgneq))

data %>% 
  ggplot(aes(x=kgco2eq, y=kgneq)) +
  theme_minimal() +
  geom_point(aes(colour=productgroep)) +
  geom_polygon(data=hull, aes(fill=productgroep), alpha=.5)


data %>% 
  ggplot(aes(x=productgroep, y=kgco2eq)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_boxplot(aes(fill=productgroep), alpha=.5, width=1) +
  labs(x="")



