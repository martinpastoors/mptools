# Eenmalig uitvoeren:
# install.packages("cbsodataR")

library(cbsodataR)
library(tidyverse)
library(jsonlite)

get_odata <- function(targetUrl) {
  data <- data.frame()
  
  while(!is.null(targetUrl)){
    response <- fromJSON(url(targetUrl))
    data <- bind_rows(data,response$value)
    targetUrl <- response[["@odata.nextLink"]]
  }
  return(data)
}

# tabel    <- 
#   toc %>%  
#   filter(Title == "Overheidsuitgaven en bestedingen; functies, transacties, overheidssectoren") %>% 
#   select(Identifier) %>% 
#   as.character()
# 
# tableUrl <- paste0("https://odata4.cbs.nl/CBS/",tabel)
# targetUrl <- paste0(tableUrl,"/Observations")
# data <- get_odata(targetUrl)
# head(data)

# transacties <- get_odata(paste0(tableUrl))
# transacties <- get_odata(paste0(tableUrl,"/TransactiesGroups"))
# codes <- get_odata(paste0(tableUrl,"/MeasureCodes"))

# Downloaden van tabeloverzicht
# toc <- cbs_get_toc() %>% arrange(Title)

# View(toc)
# toc %>% filter(grepl("overheidsfinancien", tolower(Title))) %>% View()
# toc %>% filter(tolower(Title) == "overheidsfinanciën; kerncijfers") %>% View()
# toc %>% filter(Title == "Overheidsuitgaven en bestedingen; functies, transacties, overheidssectoren") %>% View()


# Downloaden van gehele tabel (kan een halve minuut duren)

dwnld    <- cbs_get_data(tabel) 
metadata <- cbs_get_meta(tabel)

transacties       <- metadata[["Transacties"]] 
overheidsfuncties <- metadata[["Overheidsfuncties"]]
sectoren          <- metadata[["Sectoren"]]

# dataset samenstellen
data <-
  dwnld %>% 
  
  left_join(metadata$Transacties, by=c("Transacties"="Key")) %>% 
  rename(
    Transacties_title       = Title,
    Transacties_description = Description,
    Transacties_id          = CategoryGroupID 
    ) %>% 
  
  left_join(metadata$Overheidsfuncties, by=c("Overheidsfuncties"="Key")) %>% 
  rename(
    Overheidsfuncties_title       = Title,
    Overheidsfuncties_description = Description,
    Overheidsfuncties_id          = CategoryGroupID 
  ) %>% 
  
  left_join(metadata$Sectoren, by=c("Sectoren"="Key")) %>% 
  rename(
    Sectoren_title       = Title,
    Sectoren_description = Description,
    Sectoren_id          = CategoryGroupID 
  ) %>% 
  
  # add jaar
  mutate(jaar = as.integer(substr(Perioden, 1, 4))) %>% 
  
  # rename to value
  rename(value=OverheidsuitgavenEnBestedingen_1) %>% 
  
  # remove totals
  filter(Overheidsfuncties_title != "Totaal") %>% 
  
  # split Overheidsfuncties
  filter(Transacties_title == "Totaal overheidsuitgaven") %>% 
  mutate(test = str_extract(Overheidsfuncties_title, "^[^0-9.]*([0-9.]*)")) %>% 
  mutate(Overheidsfuncties_title = str_remove(Overheidsfuncties_title, "^[^0-9.]*([0-9.]*) ")) %>% 
  tidyr::separate(test, into=c("Overheidsfuncties_main","Overheidsfuncties_sub"), sep="\\.") %>% 
  
  mutate(
    Overheidsfuncties_main = as.integer(Overheidsfuncties_main),
    Overheidsfuncties_sub  = as.integer(Overheidsfuncties_sub)
  ) 
  
  



# overzicht
data %>% 
  filter(jaar > 1995) %>% 
  filter(Sectoren_title == "Overheid") %>% 
  filter(is.na(Overheidsfuncties_sub)) %>% 
  
  group_by(jaar, Overheidsfuncties_main,  Overheidsfuncties_title) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>% 

  group_by(jaar) %>%
  mutate(perc = value/sum(value, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=jaar, y=value)) +
  theme_bw() +
  theme(legend.position="none") +
  geom_bar(aes(fill=Overheidsfuncties_title), stat="identity") +
  facet_wrap(~Overheidsfuncties_title)

# Huisvesting en gemeenschapsvoorzieningen
data %>% 
  
  # filter(grepl("Huisvesting", Overheidsfuncties_title)) %>% 
  filter(Overheidsfuncties_main == 10) %>% 
  # filter(Sectoren_title == "Overheid") %>% 
  filter(jaar > 1995) %>% 
  filter(!is.na(Overheidsfuncties_sub)) %>% 
  
  group_by(jaar, Overheidsfuncties_sub,  Overheidsfuncties_title) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>% 
  
  group_by(jaar) %>%
  mutate(perc = value/sum(value, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=jaar, y=value)) +
  theme_bw() +
  theme(legend.position="none") +
  geom_bar(aes(fill=Overheidsfuncties_title), stat="identity") +
  facet_wrap(~Overheidsfuncties_title)

data %>% 
  filter(jaar == 2019) %>% 
  filter(Overheidsfuncties_title == "Volksgezondheid") %>% 
  # filter(is.na(Overheidsfuncties_sub)) %>% 
  
  # group_by(jaar, Overheidsfuncties_title) %>%
  # summarise(value = sum(value, na.rm=TRUE)) %>% 
  View()

