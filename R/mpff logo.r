# MPFF logo

options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(readxl)
library(patchwork)
library(cowplot)
library(magick)
library(treemapify)

source("R/my utils.r")

mypath <- file.path(get_dropbox(), "MPFF")

data <- readxl::read_xlsx(path=file.path(mypath, "logo plot.xlsx"))

img <- 
  image_read(file.path(mypath, "mackerel3.jpg")) %>% 
  image_resize("1190x345") %>%
  image_colorize(35, "white")

img2 <- 
  image_read(file.path(mypath, "ganzen.jpg")) %>% 
  image_resize("2000x2000") %>%
  image_colorize(35, "white")

p <-
  data %>% 
  ggplot(aes(x=nr, y=value)) +
  # theme_tufte() +
  theme_minimal() +
  theme(axis.title=element_blank()) +
  theme(panel.grid=element_blank()) +
  # theme(axis.line = element_line(colour="black")) +
  theme(axis.text = element_blank()) +
  geom_bar(stat="identity", position=position_dodge2(width=0.8), fill="#00467D", alpha=0.75) +
  # geom_errorbar(aes(ymin=min, ymax=max)) +
  # geom_errorbar(aes(ymin=value, ymax=max), width=.2, position=position_dodge(.9)) +
  # geom_errorbar(aes(ymin=min, ymax=value), width=.2, position=position_dodge(.9), colour="white") +
  # geom_linerange(aes(ymin=min, ymax=value), size=2, colour="white") +
  # geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=min), size=2, colour="white") +
  geom_linerange(aes(ymin=min, ymax=value), size=2, colour="white") +
  geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=min), size=2, colour="white") +
  
  geom_linerange(aes(ymin=value, ymax=max), colour="#00ACDD", size=2) +
  geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=max), size=2, colour="#00ACDD")


cowplot::ggdraw() + 
  draw_image(img, .056, .01, .89, .90, valign=0.1) + 
  draw_plot(p)   

cowplot::ggdraw() + 
  draw_image(img2, .056, .04, 0.90, .99, valign=0.1) + 
  draw_plot(p)   



data %>% 
  ggplot(aes(area = value, fill = nr)) +
  geom_treemap()

