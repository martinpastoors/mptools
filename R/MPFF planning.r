# ====================================================================================
# MPFF planning.r
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")
w <- data.frame(week = seq(1,52,1))

t <-
  readxl::read_excel("C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/MPFF Planning 2023.xlsx") %>% 
  lowcase() %>% 
  mutate(
    weekstart = lubridate::week(start),
    weekend   = lubridate::week(end),
    year      = lubridate::year(start),
    euroweek  = euro/weeks
  ) %>% 
  cross_join(w) %>% 
  filter(week >= weekstart, week <= weekend)

t1 <-
  t %>% 
  group_by(year, project, status) %>%
  distinct(year, project, status, week) %>% 
  filter(week == max(week)) %>% 
  ungroup() %>% 
  arrange(year, week)

t2 <-
  t1 %>% 
  distinct(year, week)

tt <-
  t %>% 
  filter(paste(year,week) %in% paste(t2$year, t2$week)) %>% 
  arrange(year, week) %>% 
  left_join(t1, by=c("year","project","week")) %>% 
  mutate(label = ifelse(is.na(status.y), "", project))

t %>% 
  ggplot(aes(x=week, y=hoursweek)) +
  theme_bw() +
  geom_bar(aes(fill=project, alpha=status), stat="identity") +
  scale_alpha_manual(values = c("completed" = 0.6, "ongoing" = 1.0, "planned" = 0.4)) + 
  # geom_text(data=tt, aes(label=label), hjust=1, vjust=0.5, position="stack") +
  guides(alpha="none") +
  facet_wrap(~year, ncol=1)

t %>% 
  ggplot(aes(x=week, y=euroweek)) +
  theme_bw() +
  geom_bar(aes(fill=project, alpha=status), stat="identity") +
  scale_alpha_manual(values = c("completed" = 0.6, "ongoing" = 1.0, "planned" = 0.4)) + 
  # geom_text(data=tt, aes(label=label), hjust=1, vjust=0.5, position="stack") +
  guides(alpha="none") +
  facet_wrap(~year, ncol=1)

# hours and € per week
t %>% 
  filter(year==2023) %>% 
  ungroup() %>% 
  dplyr::select(project, status, year, week, euroweek, hoursweek) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", c("euroweek","hoursweek")) %>% 
  mutate(variable = factor(variable, levels=c("hoursweek","euroweek"))) %>% 
  
  ggplot(aes(x=week, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=project, alpha=status), stat="identity") +
  scale_alpha_manual(values = c("completed" = 0.6, "ongoing" = 1.0, "planned" = 0.4)) + 
  # geom_text(data=tt, aes(label=label), hjust=1, vjust=0.5, position="stack") +
  guides(alpha="none") +
  facet_grid(variable~year, scales="free_y")

skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()



