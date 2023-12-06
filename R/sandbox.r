# ====================================================================================
# sandbox.r
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)
library(ggrepel)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# PISA 2022
t <-
  readxl::read_excel("C:/TEMP/OECD 2023 PISA.xlsx", range="B12:F204") %>% 
  lowcase() %>% 
  mutate(across(c(score,se), as.numeric)) %>% 
  mutate(across(c(year), as.integer)) 

t <-
  bind_rows(
    t,
    t %>% 
      filter(eu=="yes") %>% 
      group_by(year) %>% 
      summarise(score=mean(score, na.rm=TRUE)) %>% 
      mutate(country="EU")
  )

t %>% 
  ggplot(aes(x=year, y=score, group=country)) +
  theme_publication() +
  geom_line(colour="gray") +
  geom_line(data=t %>% filter(country=="Netherlands"), colour="red", size=1) +
  geom_line(data=t %>% filter(country=="Ireland"), colour="green", size=1) +
  geom_line(data=t %>% filter(country=="EU"), colour="blue", size=1)
  # geom_label_repel(data=t %>% filter(year==2022), aes(label=country), size=3, hjust=0, nudge_x=0.5,  direction="y", colour="gray") +
  # scale_x_continuous(expand=c(0, 10))  

# stikstof schier
t <-
  readxl::read_excel("C:/TEMP/CLO NH3 schiermonnikoog-3-maanden.xlsx") %>% 
  lowcase() 

t %>% 
  mutate(periode=jaar + (kwartaal-1)/4) %>% 
  ggplot(aes(x=periode, y=value)) +
  theme_publication() +
  geom_line(aes(colour=locatienaam)) +
  scale_x_continuous(breaks=seq(2011,2022,1)) +
  facet_wrap(~locatienaam)

t %>% 
  mutate(periode=jaar + (kwartaal-1)/4) %>% 
  ggplot(aes(x=periode, y=value)) +
  theme_publication() +
  geom_line(aes(colour=locatienaam)) +
  scale_x_continuous(breaks=seq(2011,2022,1)) +
  facet_wrap(~kwartaal)


t %>% 
  mutate(periode=jaar + (kwartaal-1)/4) %>% 
  ggplot(aes(x=periode, y=value)) +
  theme_publication() +
  geom_line(aes(colour=locatienaam)) +
  scale_x_continuous(breaks=seq(2011,2022,1)) +
  facet_grid(locatienaam~kwartaal)

t %>% 
  group_by(jaar) %>%
  summarise(
    mean = mean(value),
    sd   = sd(value),
    se   = sd(value)/sqrt(n()))  %>% 
  
  ggplot(aes(x=jaar, y=mean)) +
  theme_publication() +
  geom_line() +
  geom_ribbon(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), alpha=0.3) +
  expand_limits(y=0) +
  scale_x_continuous(breaks=seq(2011,2022,1)) 

t <-
  readxl::read_excel("C:/TEMP/predicted length.xlsx") %>% 
  lowcase() %>% 
  tidyr::pivot_longer(names_to = "weight", values_to = "length", l250:l550)

skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()

t %>% 
  ggplot(aes(x=month, y=length)) +
  theme_publication() +
  geom_line(aes(colour=country)) +
  scale_x_continuous(breaks=1:12) +
  facet_wrap(~weight)


# Parallel processing

library(doFuture)

plan(multisession, workers=12)

registerDoFuture()

foreach(i = 1:12) %dopar% {
  
  print(sum(rnorm(99)))
}
