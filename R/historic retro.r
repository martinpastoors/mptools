# ===========================================================================
# R-script for ICES assessment summary 1982-2016
# Martin Pastoors

# Original:09/07/2014
# updated: 08/09/2014
# updated: 10/03/2015
# updated: 05/04/2016
# 10/04/2016 removed forecast years to separate file
# ===========================================================================

# Reset lists
rm(list=ls())

require(gdata)          # install.packages("gdata")
require(graphics)       # install.packages("graphics" )
require(ggplot2)
require(lattice)
library(dplyr)
library(tidyr)
# library(directlabels)   # install.packages("directlabels")

# Set working directory
setwd("C:/DATA/GIT/mptools/")

# Read and convert data
rby             <- read.csv("C:/DATA/ICES Assessment Summary database 20160330.csv",
                            stringsAsFactors=F)
rby$F           <- as.numeric(rby$F)
rby$Recruitment <- as.numeric(rby$Recruitment)
rby$SSB         <- as.numeric(rby$SSB)
rby$Landings    <- as.numeric(rby$Landings)
rby$Catches     <- as.numeric(rby$Catches)

# Store the forecast years
rbyF <-
  rby %>% 
  tbl_df() %>% 
  select(AssYear, FishStock, Year, SSB, Recruitment, F, Landings, Catches) %>% 
  filter(Year >= AssYear) %>% 
  mutate(ForecastYear = Year - AssYear)
# write.csv(rbyF,"rbyF.csv")

# Store the assessments
rbyA <-
  rby %>% 
  tbl_df() %>% 
  select(AssYear, FishStock, Year, SSB, Recruitment, F, Landings, Catches) %>% 
  filter(Year < AssYear)
# write.csv(rbyA,"rbyA.csv")

# Plot stock data over different assessment years ---------------------
d <-
  rbyA %>% 
  tbl_df() %>% 
#  filter(FishStock == "cod-347d",
  filter(FishStock == "her-47d3",
         Year      >  1990, 
         AssYear   >= 2011,
         Year      <= AssYear) %>% 
  select(AssYear, Year, FishStock, SSB, F, Recruitment) %>% 
  gather(key=variable, value, SSB:Recruitment)

# dataset with last year
d.last <- d %>% filter(AssYear == max(AssYear))

# dataset from other years
d <- d %>% filter(AssYear != max(AssYear))

d %>% 
  filter(variable %in% c("SSB","F","Recruitment") ) %>% 
  
  # strange the factor does not work in ordering the graphs??
  mutate(variable = factor(variable, levels = c("SSB", "F", "Recruitment"))) %>% 
  
  ggplot(aes(Year,value, group=AssYear)) +
  theme_bw() +
  geom_line() +
  geom_line(data = d.last, col = "red", lwd = 1) +
  facet_grid(variable ~ ., scales = "free", switch = "y") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank()) +
  labs(x = NULL, y = NULL)
  


