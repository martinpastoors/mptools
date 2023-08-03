# ====================================================================================
# aahunze.r
# 
# read Aa en Hunze data 
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)
library(ggrepel)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

t <-
  readxl::read_excel("C:/TEMP/Data Hunze selectie boven detectielimiet .xlsx") %>% 
  lowcase() %>% 
  mutate(datum= as.Date(datum)) %>% 
  mutate(data = as.numeric(numeriekewaarde))

# plot
t %>% 
  filter(parameteromschrijving %notin% c("Biochemisch zuurstofverbruik over 5 dagen","Destructie uitgevoerd","Doorzicht","Extinctie",
                                         "Helderheid","Geur","Kleur","onbekend","Onopgeloste stoffen","Troebelheid","Zuurgraad",
                                         "zuurstof", "Geleidendheid", "Temperatuur")) %>% 
  drop_na(parameteromschrijving) %>% 
  
  ggplot(aes(x=datum, y=data, group=beschrijving)) +
  theme_publication() +
  theme(plot.margin = margin(1,1,1,1, "mm")) +
  theme(text=element_text(size=9)) +
  # theme(legend.position="none") +
  geom_point(aes(colour=beschrijving), size=0.3) +
  geom_smooth(aes(colour=beschrijving), method="lm", se=FALSE, linewidth=0.2) +
  # scale_x_continuous(limits=c(0,400), breaks=seq(0,350,50)) +
  # ggrepel::geom_text_repel(data=tt, 
  #                          aes(label=year, colour=as.character(year)),
  #                          hjust=0, size=3, segment.size=0.25, segment.linetype="dashed", nudge_x=5, direction="y",
  #                          min.segment.length = 0) +
  # labs(title="afgelegde kilometers") +
  expand_limits(y=0) +
  scale_y_continuous(breaks=scales::pretty_breaks(n=3)) +
  scale_x_date(date_labels =  "%Y", breaks=scales::pretty_breaks(n=4))  + 
  labs(x="",y="") +
  facet_wrap(~parameteromschrijving, scales="free_y")


skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()


