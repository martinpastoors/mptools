# trackR codes to read tcx files

library(tidyverse)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

lt <- 
  readxl::read_xlsx("C:/TEMP/trends_broedvogels_1990-2019_sovon.xlsx",
                     sheet="Landelijke trends 1990-2019",
                     col_names=TRUE,
                     skip=2) %>% 
  lowcase() %>% 
  dplyr::select(!(contains("trend"))) %>% 
  pivot_longer(names_to="year", values_to="index", "1990":"2019") %>% 
  mutate(year = as.integer(year)) %>% 
  filter(index != 0)

lt %>% 
  ggplot(aes(x=year, y=index)) +
  theme_publication( ) +
  theme(legend.position="none") +
  theme(panel.spacing = unit(0, "lines")) +
  geom_line(aes(colour=soort)) +
  geom_point(aes(colour=soort)) +
  expand_limits(y=0) +
  facet_wrap(~soort, scales="free_y")




pt <- 
  readxl::read_xlsx("C:/TEMP/trends_broedvogels_1990-2019_sovon.xlsx",
                    sheet="Provinciale trends 1990-2019",
                    col_names=TRUE,
                    skip=2) %>% 
  lowcase() %>% 
  dplyr::select(!(contains("trend"))) %>% 
  pivot_longer(names_to="year", values_to="index", "1990":"2019") %>% 
  mutate(year = as.integer(year)) %>% 
  filter(index != 0)

pt %>% 
  filter(soort == "Roek") %>% 
  filter(provincie %in% c("Drenthe","Friesland","Groningen")) %>% 
  ggplot(aes(x=year, y=index, group=provincie)) +
  theme_publication( ) +
  geom_line(aes(colour=provincie)) +
  expand_limits(y=0) +
  facet_wrap(~provincie)

pt %>% 
  filter(soort == "Huiszwaluw") %>% 
  filter(provincie %in% c("Drenthe","Friesland","Groningen")) %>% 
  ggplot(aes(x=year, y=index, group=provincie)) +
  theme_publication( ) +
  geom_line(aes(colour=provincie)) +
  expand_limits(y=0) +
  facet_wrap(~provincie)

pt %>% 
  filter(soort == "Koolmees") %>% 
  filter(provincie %in% c("Drenthe","Friesland","Groningen")) %>% 
  ggplot(aes(x=year, y=index, group=provincie)) +
  theme_publication( ) +
  geom_line(aes(colour=provincie)) +
  expand_limits(y=0) +
  facet_wrap(~provincie)

pt %>% 
  filter(provincie %in% c("Drenthe")) %>% 
  ggplot(aes(x=year, y=index, group=provincie)) +
  theme_publication( ) +
  theme(legend.position="none") +
  theme(panel.spacing = unit(0, "lines")) +
  geom_line(aes(colour=soort)) +
  geom_point(aes(colour=soort)) +
  expand_limits(y=0) +
  facet_wrap(~soort, scales="free_y")
