# ====================================================================================
# auto.r
# 
# read auto data 
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)
library(googledrive)   # install.packages("googledrive")
library(ggrepel)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# drive_find()
drive_download(file="Auto verbruik.xlsx", overwrite=TRUE)

t <-
  readxl::read_excel("C:/Users/MartinPastoors/Dropbox/Martin en Pina/auto verbruik.xlsx") %>% 
  lowcase() %>% 
  drop_na(datum) %>% 
  drop_na(kmafgelegd) %>% 
  filter(auto != "Corsa 2021 blauw") %>% 
  dplyr::select(c(1:5,15)) %>% 
  
  # bind google drive document
  bind_rows(
    readxl::read_excel("Auto verbruik.xlsx") %>% 
    lowcase() %>% 
    drop_na(datum) %>% 
    drop_na(kmafgelegd) %>% 
    dplyr::select(c(1:5,11))    
  ) %>% 
  mutate(
    yday  = lubridate::yday(datum),
    month = lubridate::month(datum),
    year  = lubridate::year(datum),
    decade= 10*floor(year/10)
  ) %>% 
  arrange(datum) %>% 
  
  # quick fix for missing liters or euros
  mutate(
    litersgetankt = ifelse(is.na(litersgetankt), lag(litersgetankt) * kmafgelegd/lag(kmafgelegd), litersgetankt),
    eurogetankt   = ifelse(is.na(eurogetankt), lag(eurogetankt) * kmafgelegd/lag(kmafgelegd), eurogetankt)
  ) %>% 
  
  group_by(year) %>% 
  mutate(
    km   = cumsum(kmafgelegd),
    euro =cumsum(eurogetankt),
    liters=cumsum(litersgetankt)
  ) %>% 
  filter(year >= 2003) %>% 
  filter(year %notin% 2014:2018)  # missing Corsa Eco data

# per month  
m <-
  t %>% 
  group_by(decade, year, month) %>% 
  summarise(
    km = sum(kmafgelegd, na.rm=TRUE),
    euro= sum(eurogetankt, na.rm=TRUE),
    liters=sum(litersgetankt, na.rm=TRUE)
  )

tt <-
  t %>% 
  group_by(year) %>% 
  slice_tail(n=1)

# plot km
t %>% 
  ggplot(aes(x=yday, y=km, group=year)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=as.character(year))) +
  geom_point(data=tt,
             aes(colour=as.character(year))) +
  scale_x_continuous(limits=c(0,400), breaks=seq(0,350,50)) +
  ggrepel::geom_text_repel(data=tt, 
                           aes(label=year, colour=as.character(year)),
                           hjust=0, size=3, segment.size=0.25, segment.linetype="dashed", nudge_x=5, direction="y",
                           min.segment.length = 0) +
  labs(title="afgelegde kilometers") +
  facet_wrap(~decade)

# plot euro
t %>% 
  ggplot(aes(x=yday, y=euro, group=year)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=as.character(year))) +
  geom_point(data=tt,
             aes(colour=as.character(year))) +
  scale_x_continuous(limits=c(0,400), breaks=seq(0,350,50)) +
  ggrepel::geom_text_repel(data=tt, 
                           aes(label=year, colour=as.character(year)),
                           hjust=0, size=3, segment.size=0.25, segment.linetype="dashed", nudge_x=5, direction="y",
                           min.segment.length = 0) +
  facet_wrap(~decade)

# plot liters
t %>% 
  ggplot(aes(x=yday, y=liters, group=year)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=as.character(year))) +
  geom_point(data=tt,
             aes(colour=as.character(year))) +
  scale_x_continuous(limits=c(0,400), breaks=seq(0,350,50)) +
  ggrepel::geom_text_repel(data=tt, 
                           aes(label=year, colour=as.character(year)),
                           hjust=0, size=3, segment.size=0.25, segment.linetype="dashed", nudge_x=5, direction="y",
                           min.segment.length = 0) +
  facet_wrap(~decade)

# plot km per month
m %>% 
  ggplot(aes(x=month, y=km, group=year)) +
  theme_publication() +
  theme(legend.position="none") +
  # geom_line(aes(colour=as.character(year))) +
  geom_bar(aes(fill=as.character(year)), stat="identity") +
  labs(title="afgelegde kilometers") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  facet_wrap(~year)

skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()


