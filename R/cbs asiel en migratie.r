# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
toc %>% filter(grepl("asiel", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80059ned"
dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns()  %>% 
  mutate(Perioden_label = as.integer(as.character(Perioden_label)))
  # cbs_add_date_column() 

metadata <- 
  cbs_get_meta(tabel)

save(dwnld, metadata, file=paste0("cbs",tabel,".RData"))
load(file=paste0("cbs",tabel,".RData"))
# load(file=paste0("cbs","85217NED",".RData"))
load(file=paste0("cbs","03759ned",".RData"))


# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)
# unique(metadata$DataProperties$Type)

properties <- metadata$DataProperties

topicgroups <- 
  properties %>% 
  filter(Type == "TopicGroup") %>% 
  dplyr::select(ID, ParentID, Title_tg=Title, Description_tg=Description)

metadata_df <-
  properties %>% 
  filter(Type == "Topic") %>% 
  dplyr::select(Key, ID, Position, ParentID, Variable=Title, Description, Unit)  %>% 
  
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
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="Key", values_to="data", 3:ncol(.)) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("Key"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
  lowcase()



dwnld %>% 
  lowcase() %>% 
  filter(nationaliteitlabel=="Totaal") %>% 
  dplyr::select(periodenlabel, totaalasielverzoekenennareizigers1) %>% 
  pivot_longer(names_to = "variable", values_to = "data", totaalasielverzoekenennareizigers1) %>% 
  ggplot(aes(x=periodenlabel, y=data)) + 
  geom_bar(stat="identity") +
  facet_wrap(~variable)


