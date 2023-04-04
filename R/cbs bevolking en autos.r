# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# Downloaden van tabeloverzicht
toc <- cbs_get_toc(); toc %>% filter(grepl("bevolking", tolower(Title))) %>% View()
tabel    <- "83474NED"
tabel    <- "37296ned"

bevolking    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns()  %>% 
  mutate(Perioden_label = as.integer(as.character(Perioden_label))) %>% 
  drop_na(Perioden_label) %>% 
  lowcase() %>% 
  dplyr::select(c(2,3)) %>% 
  setNames(c("jaar","bevolking"))

toc %>% filter(grepl("motorvoertuigen", tolower(Title))) %>% View()
tabel    <- "82472NED"

voertuigen    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns()  %>% 
  mutate(Perioden_label = as.integer(as.character(Perioden_label))) %>% 
  drop_na(Perioden_label) %>% 
  lowcase() %>% 
  dplyr::select(c(2,3,4)) %>% 
  setNames(c("jaar","voertuigen","autos"))

comb <-
  bevolking %>% 
  left_join(voertuigen, by="jaar")

p1 <-
  comb %>% 
  mutate(bevolking = as.integer(bevolking / 1000)) %>% 
  pivot_longer(names_to = "variable", values_to = "data", c("bevolking", "voertuigen","autos")) %>% 
  filter(variable %in% c("bevolking","autos")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_publication() +
  geom_point(aes(colour=variable)) +
  labs(y="*1000", title="bevolking en aantal auto's")

p2 <-
  comb %>% 
  mutate(bevolking = as.integer(bevolking / 1000)) %>% 
  mutate(decade = cut(jaar,breaks=seq(1950,2030,10),
                      dig.lab=10,
                      ordered_result = TRUE) ) %>% 
  ggplot(aes(x=bevolking, y=autos)) +
  theme_publication() +
  geom_point(aes(colour=decade)) +
  scale_colour_brewer(palette = "Set1") +
  labs(y="autos", title="bevolking en aantal auto's")

library(patchwork)
print( p1 + p2   + plot_layout(widths = c(25, 25)) )


  # cbs_add_date_column() 

metadata <- 
  cbs_get_meta(tabel)


data <-
  dwnld %>% 
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="Key", values_to="data", 3:ncol(.)) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("Key"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
  lowcase()




