# cv.r 

options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(readxl)
library(patchwork)
library(cowplot)
library(magick)

source("R/my utils.r")

mypath <- file.path(get_dropbox(), "MPFF")

data <- 
  readxl::read_xlsx(path=file.path("C:/TEMP", "Pastoors 20221031 career timeline.xlsx"), sheet="data")

yrs <- seq(min(data$start), max(data$end), 5)

data %>% 
  ggplot(aes(y=value)) +
  theme_publication() +
  theme(axis.text.y=element_blank()) + 
  theme(legend.position="none") +
  ggalt::geom_dumbbell(aes(x = start, xend = end+1, colour=Job),
                       size=10, size_x = 0, size_xend = 0, alpha=0.6) +
  # scale_colour_manual(values=myAreaColors) +
  geom_text(aes(x=start, label=Job), hjust=0, vjust=0.5) +
  geom_rect(aes(xmin=start, xmax=end+1, ymin=value, ymax=value+0.1, fill=Job), hjust=0, vjust=0.5) +
  # geom_text(aes(x=yrs, y=0, label=yrs), hjust=0.5, vjust=1) +
  # geom_hline(yintercept=0, size=1) +
  # geom_segment(aes(x=yrs, xend=yrs, y=0, yend=0.05), size=1) +
  # scale_x_continuous(position="centre") +
  labs(x="", y="") 
