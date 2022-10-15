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

data <- readxl::read_xlsx(path=file.path(mypath,"logo", "logo plot.xlsx"))

img <- 
  image_read(file.path(mypath, "images", "mackerel3.jpg")) %>% 
  image_resize("1190x345") %>%
  image_colorize(35, "white")

img2 <- 
  image_read(file.path(mypath, "images", "ganzen.jpg")) %>% 
  image_resize("2000x2000") %>%
  image_colorize(35, "white")

img3 <- 
  image_read(file.path(mypath, "images",  "plant noot ingekort 20220917_151556 (Groot).png")) %>% 
  # image_rotate(degrees=90) %>% 
  # image_resize("1190x345") %>%
  image_colorize(70, "black")

cowplot::ggdraw() + cowplot::draw_image(img3, x=.056, y=.04, width=0.90, height=.99, valign=0.1)  
  

# cowplot::ggdraw() + draw_image(img3, .056, .04, 0.90, .99, valign=0.1)  
  
p <-
  data %>% 
  ggplot(aes(x=nr, y=value)) +
  # theme_tufte() +
  theme_minimal() +
  theme(axis.title=element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(axis.text = element_blank()) +
  geom_bar(stat="identity", position=position_dodge2(width=0.8), fill="#00467D", alpha=0.75) +
  geom_linerange(aes(ymin=min, ymax=value), size=2, colour="white") +
  geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=min), size=2, colour="white") +
  
  geom_linerange(aes(ymin=value, ymax=max), colour="#00ACDD", size=2) +
  geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=max), size=2, colour="#00ACDD")

p2 <-
  data %>%
  ggplot(aes(x=nr, y=value)) +
  # theme_tufte() +
  theme_minimal() +
  theme(axis.title=element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(axis.text = element_blank()) +
  
  geom_bar(stat="identity", position=position_dodge2(width=0.8), fill="#2C321A", alpha=0.75) +
  geom_linerange(aes(ymin=min, ymax=value), size=2, colour="white") +
  geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=min), size=2, colour="white") +

  geom_linerange(aes(ymin=value, ymax=max), colour="#8EA451", size=2) +
  geom_linerange(aes(xmin=nr-0.2, xmax=nr+0.2, y=max), size=2, colour="#8EA451")

cowplot::ggdraw() +
  draw_image(img, .056, .01, .89, .90, valign=0.1) +
  draw_plot(p)

cowplot::ggdraw() +
  draw_image(img2, .056, .04, 0.90, .99, valign=0.1) +
  draw_plot(p)



cowplot::ggdraw() + 
  cowplot::draw_image(img3, x=.056, y=.04, width=0.90, height=.99, valign=0.1) + 
  # draw_plot(p, x=0.22, width=0.55) 
  cowplot::draw_plot(p2, x=0.1, width=0.85, height=0.9)

cowplot::ggdraw() + 
  cowplot::draw_plot(p2, x=0.1, y=0.05, width=0.85, height=0.9) +
  cowplot::draw_image(img3, x=.056, y=.04, width=0.90, height=.99, valign=0.1) 

