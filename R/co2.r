# ====================================================================================
# co2.r
# 
# read co2 data 
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)
# library(googledrive)   # install.packages("googledrive")
library(ggrepel)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

dropboxdir <- file.path(get_dropbox(), "Martin en Pina")

owid <-
  readxl::read_excel(file.path(dropboxdir, "ourworldindata annual-co2-emissions-per-country.xlsx"),
                     sheet="data") %>% 
  lowcase() %>% 
  filter(year >= 1950) %>% 
  mutate(source="OWID") %>% 
  rename(emission = annualco2emissions)

er <-
  readxl::read_excel(file.path(dropboxdir, "ER_DataExport-2023-04-16-063340.xlsx"),
                     sheet="data") %>% 
  lowcase()  %>% 
  mutate(source="ER") %>% 
  rename(emission = emissie, year=jaar)

clo <-
  readxl::read_excel(file.path(dropboxdir, "PBL 2022 Compendium voor de Leefomgeving - Verschillen tussen CO2-emissietotalen verklaard, 1990-2021.xlsx"),
                     sheet="data") %>% 
  lowcase()  %>% 
  mutate(emission = 1000000 * emission)

t <-
  bind_rows(
    owid %>% 
      filter(code=="NLD"),
    er %>% 
      filter(stof=="Koolstofdioxide") %>% 
      group_by(source, year) %>% 
      summarise(emission = sum(emission)/1000),
    clo
  ) %>% 
  mutate(emission = emission / 1000000)

tt <-
  t %>% 
  filter(year == 1990) %>% 
  dplyr::select(emission1990=emission, source) %>% 
  right_join(t, by=c("source")) %>% 
  mutate(rel_emission = emission / emission1990 -1)

ttt <-
  tt %>% 
  filter(year == 2021)

tttt <-
  t %>% 
  filter(source %in% c("OWID","B. IPCC-emissies")) %>% 
  mutate(source=ifelse(source=="B. IPCC-emissies","IPCC", source)) %>% 
  dplyr::select(-country, -code) %>% 
  pivot_wider(names_from = source, values_from = emission) %>% 
  lowcase() %>% 
  drop_na(ipcc) %>% 
  mutate(diff=ipcc/owid-1)



distinct(ttt, source)

# plot NL
t %>% 
  ggplot(aes(x=year, y=emission, group=source)) +
  theme_publication() +
  geom_line(aes(colour=source)) +
  geom_point(aes(colour=source)) +
  labs(title="CO2 emissions NL") +
  labs(y="Emission (megaton)") +
  scale_x_continuous(breaks=seq(1950,2021,10)) +
  geom_vline(xintercept=1990, linetype="dashed") +
  expand_limits(y=0)

# plot NL relative
tt %>% 
  ggplot(aes(x=year, y=rel_emission, group=source)) +
  theme_publication() +
  geom_line(aes(colour=source)) +
  geom_point(aes(colour=source)) +
  ggrepel::geom_text_repel(data=ttt, aes(colour=source, label=scales::percent(rel_emission, accuracy=1))) +
  labs(title="CO2 emissions NL") +
  labs(y="Emission relative to 1990") +
  scale_x_continuous(breaks=seq(1950,2021,10)) +
  geom_vline(xintercept=1990, linetype="dashed") +
  expand_limits(y=0)

# plot world
largest10 <- owid %>% drop_na(code) %>% filter(year == 2020) %>% arrange(desc(emission)) %>% slice_head(n=11)

owid %>% 
  filter(country %in% c("European Union (28)", as.character(largest10$country))) %>% 
  mutate(emission = emission/1000) %>% 
  ggplot(aes(x=year, y=emission, group=country)) +
  theme_publication() +
  geom_line(aes(colour=country)) +
  geom_point(aes(colour=country)) +
  labs(title="CO2 emissions world + top 10 countries") +
  labs(y="Emission (megaton)") +
  scale_x_continuous(breaks=seq(1950,2021,10)) +
  geom_vline(xintercept=1990, linetype="dashed") +
  expand_limits(y=0)


distinct(owid, country) %>% filter(grepl("Europe", country))
owid %>% filter(country == "European Union (27)") %>% View()

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

t %>% 
  filter(year >= 2019) %>% 
  ggplot(aes(x=yday, y=liters, group=year)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=as.character(year))) +
  geom_point(data=filter(tt, year >= 2019),
             aes(colour=as.character(year))) +
  scale_x_continuous(limits=c(0,400), breaks=seq(0,350,50)) +
  ggrepel::geom_text_repel(data=filter(tt, year >= 2019), 
                           aes(label=year, colour=as.character(year)),
                           hjust=0, size=3, segment.size=0.25, segment.linetype="dashed", nudge_x=5, direction="y",
                           min.segment.length = 0) 

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


