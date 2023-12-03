# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# toc <- cbs_get_toc()

# ------------------------------------------------------------------------------
# 71904ned landbouw vanaf 1851
# ------------------------------------------------------------------------------

tabel <- 
  "71904ned" # landbouw vanaf 1851
dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() %>% 
  cbs_add_date_column() 
metadata <- 
  cbs_get_meta(tabel)
properties <- 
  metadata$DataProperties
topicgroups <- 
  properties %>% 
  filter(Type == "TopicGroup") %>% 
  dplyr::select(ID, ParentID, Title_tg=Title, Description_tg=Description)
metadata_df <-
  properties %>% 
  filter(Type == "Topic") %>% 
  dplyr::select(Key, ID, Position, ParentID, Variable=Title, Description, Unit) %>% 
  # level1
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, ParentID=ParentID.y, Title_1=Title_tg, Description_1 = Description_tg) %>% 
  # level2
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, ParentID=ParentID.y, Title_2=Title_tg, Description_2 = Description_tg) %>% 
  # level3
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, ParentID=ParentID.y, Title_3=Title_tg, Description_3 = Description_tg) %>% 
  # level4
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, -ParentID.y, Title_4=Title_tg, Description_4 = Description_tg) %>% 
  
  tidyr::pivot_longer(names_to = "tempvar", values_to = "data", Title_1:Description_4) %>% 
  drop_na(data) %>% 
  tidyr::separate(tempvar, into=c("text","id"), sep="_") %>% 
  mutate(id = as.integer(id) ) %>% 
  
  group_by(Key, text) %>% 
  mutate(id = abs(id - max(id, na.rm=TRUE))) %>% 
  
  tidyr::unite("tempvar", text:id, sep="_") %>% 
  
  pivot_wider(names_from = tempvar, values_from = data) %>% 
  rename(hoofdcategorie = Title_0, hoofdcategorie_desc = Description_0) %>% 
  rename(unittype       = Title_1, unittype_desc       = Description_1) %>% 
  {if("Title2" %in% names(.)) rename(subcategorie   = Title_2, subcategorie_desc   = Description_2) else .} %>% 
  {if("Title3" %in% names(.)) rename(subsubcategorie= Title_3) else .}
  
data <-
  dwnld %>% 
  ungroup() %>% 
  pivot_longer(names_to="Key", values_to="data", 5:204) %>% 
  left_join(metadata_df, by=c("Key"="Key")) %>% 
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  drop_na(data) %>% 
  lowcase() %>% 
  mutate(tabel = tabel) %>% 
  mutate(title = metadata$TableInfos$Title) %>% 
  mutate(summary = metadata$TableInfos$Summary) %>% 
  mutate(modified = metadata$TableInfos$Modified) %>% 
  dplyr::select(-perioden, -periodendate, -periodenfreq, -periodenlabel, -key, -id, -position) %>% 
  dplyr::select(tabel, title, summary, variable, jaar, everything())

cbs_jaardata <- data
if (tabel %in% cbs_jaardata$tabel) {
  cbs_jaardata <-
    cbs_jaardata %>%
    filter(tabel %notin% tabel) %>% 
    bind_rows(data)
} else {
  cbs_jaardata <-
    cbs_jaardata %>%
    bind_rows(data)
}

