# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
# View(toc)

toc %>% filter(grepl("landbouw", tolower(Title))) %>% View()
# toc %>% filter(grepl("personenauto", tolower(Title))) %>% View()
# toc %>% filter(grepl("broedvogel", tolower(Title))) %>% View()
# toc %>% filter(grepl("bodem", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80781ned"
# tabel    <- "84498NED" # broedvogel index
tabel <- "71904ned" # landbouw vanaf 1851

dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() %>% 
  cbs_add_date_column() 
metadata <- 
  cbs_get_meta(tabel)

save(dwnld, metadata, file="cbs80781ned.RData")
# load(file="cbs80781ned.RData")

# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)

properties <- metadata$DataProperties

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
  


# dataset samenstellen
data <-
  dwnld %>% 
  
  # add naam van gemeente
  # left_join(metadata$RegioS, by=c("RegioS"="Key")) %>% 
  # rename(naam = RegioS_label) %>% 
  
  # filter op namenn
  # filter(naam %in% my_names) %>% 
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="Key", values_to="data", 5:204) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("Key"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
  lowcase()


unique(data$unittype)
unique(data$variable)
data %>% filter(grepl("bedrij", tolower(variable))) %>% View()
data %>% filter(grepl("melk", tolower(variable))) %>% View()
data %>% filter(grepl("vlees", tolower(variable))) %>% View()

# alle dieren
data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" | 
         variable == "Melk afgeleverd aan fabrieken"|
         variable == "Totale melkproductie"|
         variable == "Bedrijven met rundvee") %>% 
  # filter(tolower(variable) == "totale melkproductie", !is.na(data)) %>% 
  # filter(tolower(variable) == "kalfsvlees", !is.na(data)) %>% 
  mutate(hoofdcategorie = ifelse(variable == "Totale melkproductie", "Productie1", hoofdcategorie)) %>% 
  mutate(hoofdcategorie = ifelse(variable == "Melk afgeleverd aan fabrieken", "Productie2", hoofdcategorie)) %>% 
  
  filter(jaar >= 1950) %>% 
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~hoofdcategorie, scales = "free_y")

data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" | 
           variable == "Melk afgeleverd aan fabrieken"|
           variable == "Bedrijven met rundvee") %>% 
  # filter(tolower(variable) == "totale melkproductie", !is.na(data)) %>% 
  # filter(tolower(variable) == "kalfsvlees", !is.na(data)) %>% 

  filter(jaar >= 1950) %>% 
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~hoofdcategorie, scales = "free_y")

# overzicht bedrijven, rundvee en melk geleverd aan fabrieken
data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" | 
           variable == "Melk afgeleverd aan fabrieken"|
           variable == "Bedrijven met rundvee") %>% 
  mutate(hoofdcategorie = paste(hoofdcategorie, unit)) %>% 
  mutate(hoofdcategorie = ifelse(variable=="Melk afgeleverd aan fabrieken", paste(variable, unit), hoofdcategorie)) %>% 
  mutate(hoofdcategorie = ifelse(variable=="Bedrijven met rundvee", paste(variable, unit), hoofdcategorie)) %>% 
  mutate(variable = ifelse(variable %in% c("Melk afgeleverd aan fabrieken","Bedrijven met rundvee"), NA, variable)) %>% 
  filter(jaar >= 1950) %>% 
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~hoofdcategorie, scales = "free_y")

# overzicht (melk) dieren per bedrijf en melk productie per bedrijf
t <-
  data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" ) %>%
  filter(grepl("^melk", tolower(variable))) %>% 
  group_by(jaar) %>% 
  mutate(data = 1000 * data) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  mutate(variable = "melkvee") %>% 
  
  # melkproductie
  bind_rows(
    data %>% 
    filter(variable == "Melk afgeleverd aan fabrieken") %>% 
    filter( !is.na(data)) %>% 
    mutate(data = 1000000 * data) %>% 
    group_by(jaar) %>% 
    summarise(data = sum(data, na.rm=TRUE)) %>% 
    mutate(variable = "melkproductie")
  ) %>% 
  
  # rundvee bedrijven
  bind_rows(
    data %>% 
      filter(variable == "Bedrijven met rundvee") %>% 
      filter( !is.na(data)) %>% 
      mutate(data = 1000 * data) %>% 
      group_by(jaar) %>% 
      summarise(data = sum(data, na.rm=TRUE)) %>% 
      mutate(variable = "bedrijven")
  ) %>% 
  
  filter(jaar >= 1950) %>% 
  filter(data > 0) %>% 
  pivot_wider(names_from = variable, values_from = data) %>% 
  
  mutate(dieren_bedrijf = melkvee / bedrijven) %>% 
  mutate(productie_bedrijf = melkproductie / bedrijven) %>% 
  mutate(productie_koe     = melkproductie / melkvee) 

t2a <-
  t %>% 
  pivot_longer(names_to = "variable", values_to = "data", melkvee:productie_koe) %>% 
  mutate(decade = cut(jaar, breaks=seq(1950,2020,10), include.lowest=TRUE, dig.lab=10)) %>% 
  mutate(decade = as.character(decade)) %>% 
  mutate(decade = gsub("\\[|\\(|\\]","", decade)) %>% 
  mutate(decade = ifelse(is.na(decade), "2020", decade)) %>% 
  group_by(decade, variable) %>% 
  summarise(data = mean(data, na.rm=TRUE))

t2 <-
  t2a %>% 
  left_join(
    t2a %>% group_by(variable) %>% filter(row_number()==1) %>% dplyr::select(variable, first=data), 
    by="variable"
  ) %>% 
  mutate(index = data/first) %>% 
  mutate(perc  = (data/first-1))

t %>% 
  dplyr::select(jaar, dieren_bedrijf, productie_bedrijf, productie_koe) %>% 
  pivot_longer(names_to = "variable", values_to = "data", dieren_bedrijf:productie_koe) %>% 

  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~variable, scales = "free_y")

t2 %>% 
  ungroup() %>% 
  separate(decade, into=c("start", "end"), sep=",") %>% 
  mutate(across(c(start, end), as.numeric)) %>% 
  mutate(end = ifelse(is.na(end), start+10, end)) %>% 
  group_by(variable) %>% 
  mutate(index2 = lag(index)) %>% 
  # View()

  ggplot(aes(x=start)) +
  theme_bw() +
  theme(legend.position = "none") +
  # geom_point(aes(colour=variable)) +
  geom_segment(aes(xstart=start, xend=start, y=index, yend=index2), colour="gray", size=0.2) +
  geom_segment(aes(xstart=start, xend=end, y=index, yend=index, colour=variable), size=1.2) +
  # geom_segment(aes(xstart=start, xend=end, ystart=perc, yend=perc, colour=variable), size=1.2) +
  labs(y="", x="") +
  expand_limits(y=0) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  # scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  facet_wrap(~variable, scales = "free_y")

#bedrijven
data %>% 
  filter(
    tolower(subgroep) == "aantal bedrijven",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var = gsub("Totaal","", var)) %>% 
  # View()
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  labs(y="", x="", title="Aantal bedrijven") +
  facet_wrap(~var, scales="free_y")

# aantal dieren per bedrijf
data %>% 
  filter(
    tolower(subgroep) %in% c("aantal bedrijven", "aantal dieren"),
    grepl("totaal", tolower(var) )) %>% 
  mutate(var = gsub("Totaal","", var)) %>% 
  mutate(subgroep = gsub("Aantal ","", subgroep)) %>% 
  
  group_by(var, jaar, subgroep) %>%
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  ungroup() %>% 

  pivot_wider(names_from = subgroep, values_from = data) %>% 
  mutate(dieren_bedrijf = dieren/bedrijven) %>% 
  # View()
  
  ggplot(aes(x=jaar, y=dieren_bedrijf)) +
  theme_bw() +
  geom_bar(stat="identity") +
  labs(y="", x="", title="Dieren per bedrijf") +
  facet_wrap(~var, scales="free_y")

data %>% ungroup() %>% filter(tolower(subgroep)=="aantal dieren") %>% distinct(topic) %>% View()

data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  group_by(jaar, var) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  mutate(var = gsub("Totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(var = forcats::fct_reorder(factor(var), change)) %>% 
  # View()
  
  ggplot(aes(x=var, y=change)) +
  theme_bw() +
  geom_bar(stat="identity") 

data %>% 
  filter(
    tolower(subgroep) == "aantal bedrijven",
    grepl("hokdieren|graasdieren", tolower(Title) )) %>% 
  group_by(jaar, Title) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  # mutate(var = gsub("\\, totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(var = forcats::fct_reorder(factor(Title), change)) %>% 
  # View()
  
  ggplot(aes(x=Title, y=change)) +
  theme_bw() +
  geom_bar(stat="identity") 

t1 <-
  data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  group_by(jaar, var) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  mutate(var = gsub("Totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(var = ifelse(grepl("paarden", tolower(var)), "Paarden", var)) %>% 
  mutate(var = forcats::fct_reorder(factor(var), change)) %>% 
  mutate(type = "aantal dieren")
  # View()
  
t2 <-
  data %>% 
  filter(
    tolower(subgroep) == "aantal bedrijven",
    grepl("hokdieren|graasdieren", tolower(Title) )) %>% 
  group_by(jaar, Title) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  # mutate(var = gsub("\\, totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(Title = ifelse(grepl("graasdieren", tolower(Title)), "Graasdieren", Title)) %>% 
  mutate(Title = ifelse(grepl("hokdieren", tolower(Title)), "Hokdieren", Title)) %>% 
  mutate(var = forcats::fct_reorder(factor(Title), change)) %>% 
  mutate(type = "aantal bedrijven")

bind_rows(t1, t2) %>% 
  mutate(label = paste(var, scales::percent(change, accuracy=1))) %>% 
  ggplot(aes(x=var, y=change)) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  geom_hline(yintercept=0) +
  geom_bar(stat="identity") +
  labs(title="percentuele verandering in 2021 t.o.v. 2000", x="", y="rel. verandering t.o.v. 2000") +
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label=label, y = change + 0.05 * sign(change))) +
  facet_grid(.~type, scales="free_x", space="free_x")


save(data, dwnld, metadata, metadata_df, file="csb_80781ned.RData")

# alle dieren (totalen)
data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  # View()
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=subgroep), stat="identity") +
  facet_wrap(~var, scales="free_y")

# rundvee
data %>% 
  filter(grepl("rundvee", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# rundvee per bedrijf
data %>% 
  filter(grepl("rundvee", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  group_by(hoofdgroep, var2, jaar) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  group_by(var2, jaar) %>% 
  mutate(data2 = lag(data)) %>% 
  mutate(dierenperbedrijf = data/data2) %>% 
  filter(!is.na(data2)) %>% 

  ggplot(aes(x=jaar, y=dierenperbedrijf)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  facet_wrap(~var2, scales="free_y")

# varkens
data %>% 
  filter(grepl("varken", tolower(var))) %>% 
  filter(grepl("totaal", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# cultuurgrond
data %>% 
  filter(grepl("cultuurgrond", tolower(var))) %>% 
  filter(Unit=="are") %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# bloembollen
data %>% 
  filter(grepl("bloembollenenknollen", tolower(var))) %>% 
  filter(Unit=="are") %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data/100)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# grasland
data %>% 
  filter(grepl("grasland", tolower(var))) %>% 
  filter(Unit=="are") %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# alle dieren
data %>% 
  filter(grepl("aantal dieren", tolower(hoofdgroep))) %>% 
  filter(grepl("totaal", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_wrap(hoofdgroep~var2, scales="free_y")


# WIJKEN EN BUURTEN

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel <- "84799NED"
buurt2020     <- cbs_get_data(tabel) 
buurt2020meta <- cbs_get_meta(tabel)

View(buurt2020meta$DataProperties)


# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80428ned"
dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() 
metadata <- 
  cbs_get_meta(tabel)

# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)
# names(dwnld)


extra <- 
  readxl::read_xlsx("C:/Users/marti/Dropbox/CBS 2001 motorvoertuigenpark.xlsx") %>% 
  pivot_longer(names_to = "Brandstofsoort_label", values_to = "value", totaal:lpg) %>% 
  filter(Brandstofsoort_label != "totaal") %>% 
  mutate(variable = "autos") %>% 
  filter(jaar < 2001)
  
data <-
  dwnld %>% 
  filter(LeeftijdVoertuig_label == "Totaal leeftijd voertuig") %>% 
  filter(Eigendomssituatie_label == "Totaal") %>% 
  filter(Brandstofsoort_label != "Totaal") %>% 
  mutate(Brandstofsoort_label = tolower(Brandstofsoort_label)) %>% 
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  group_by(jaar, Brandstofsoort_label) %>% 
  summarise(
    km = sum(KilometersDoorNederlandseVoertuigen_2, na.rm=TRUE),
    kminnederland = sum(KilometersInNederland_5, na.rm=TRUE),
    autos = sum(NederlandsePersonenautoSInGebruik_10, na.rm=TRUE)) %>% 
  pivot_longer(names_to = "variable", values_to = "value", km:autos) %>% 
  mutate(Brandstofsoort_label = tolower(Brandstofsoort_label)) %>% 
  mutate(Brandstofsoort_label = ifelse(grepl("benzine", Brandstofsoort_label), "benzine", Brandstofsoort_label)) %>% 
  filter(value > 0) %>% 
  bind_rows(extra)

  # bind_rows(
  #   data.frame(
  #     variable = rep("autos",3),
  #     jaar = c(1986, 1986, 1986),
  #     Brandstofsoort_label = c("Benzine/overige", "Diesel","LPG"),
  #     value = c(3741000, 380000, 520000)
  #   )) %>% 
  # bind_rows(
  #   data.frame(
  #     variable = rep("autos",3),
  #     jaar = c(1995, 1995, 1995),
  #     Brandstofsoort_label = c("Benzine/overige", "Diesel","LPG"),
  #     value = c(4639000, 614000, 380000)
  #   )) 
  

data %>% 
  ggplot(aes(x=jaar, y=value)) +
  theme_bw() +
  geom_bar(aes(fill=Brandstofsoort_label), stat="identity") +
  facet_wrap(~variable, scales="free_y")

data <-
  dwnld %>% 
  mutate(jaar = as.numeric(as.character(Perioden_label))) 

t <-
  data %>% 
  dplyr::group_by(Broedvogels_label) %>% 
  filter((Broedvogelindex_1==100 & row_number() == 1) |
         (Broedvogelindex_1==100 & row_number() == n())) 

data %>% 
  ggplot(aes(x=jaar, y=Broedvogelindex_1)) +
  theme_bw() +
  geom_hline(yintercept=100, colour="red") +
  geom_point(data=t, colour="red") +
  geom_line() +
  expand_limits(y=0)+
  facet_wrap(~Broedvogels_label, scales="free_y")

dwnld %>% filter(grepl("zilverreiger", tolower(Broedvogels_label))) %>% View()
