# ===========================================================================
# map ICES areas.r
# Martin Pastoors

# 29/01/2017 for Sean and Gerard
# 30/03/2017 Update for Gerard
# 04/04/2017 Irish closure added
# 27/04/2017 NAFO area added
# 24/08/2017 Update code for changes in R
# ===========================================================================

library(tidyverse)
library(stringr)
library(mapproj)

# set working directory
setwd("D:/XXX/PRF")

#  load the map files
load("rdata/world.europe.df.RData")
load("rdata/eez.europe.df.RData")
load("rdata/fao27.df.RData")
load("rdata/highsea.df.RData")
load("rdata/uklimit.df.RData")

load("rdata/fao.df.RData")
load("rdata/eez.df.RData")
load("rdata/world.df.RData")

# glimpse(fao.df)

# read FAO AFSIS species list
load(file="rdata/afsis.RData")

# Source all the utiles
source("D:/GIT/mptools/R/my_utils.r")

# Western 
xmin  <- -17; 
xmax  <- 10; 
ymin  <- 47; 
ymax  <- 65

faonames <-
  fao.df %>% 
  group_by(id, F_CODE) %>% 
  summarize(long = mean(range(long), na.rm=TRUE),
            lat  = mean(range(lat), na.rm=TRUE)) 


mycode   <- c("87.3.2","87.3.3","87.2.6",
              "87.1.4")

# mycode   <- c("34.1.11","34.1.12","34.1.13",
#               "34.1.31","34.1.32",
#               "34.3.11","34.3.12","34.3.13",
#               "34.1.2",
#               "34.2",
#               "34.3.2")

mylimits <- 
  filter(fao.df, F_CODE %in% mycode) %>% 
  summarise(xmin = min(long, na.rm=TRUE),
            xmax = max(long, na.rm=TRUE),
            ymin = min(lat, na.rm=TRUE),
            ymax = max(lat, na.rm=TRUE))
mynames <-
  filter(fao.df, F_CODE %in% mycode) %>% 
  group_by(F_CODE) %>% 
  filter(row_number() == 1) %>% 
  select(F_CODE) %>% 
  left_join(faonames, by=c("F_CODE"))

# write.csv(fao.df, file="excel/fao.csv")
# sort(unique(fao.df$F_CODE))

fao.df %>%
  filter(F_CODE  %in% mycode) %>% 
  filter(hole == FALSE) %>% 
  # write.csv(., file="excel/fao.csv")
  
  ggplot() + 
  theme_publication() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing     = unit(c(0,0,0,0), "lines"),
    # axis.text.x      = element_blank(),
    # axis.text.y      = element_blank(),
    # axis.ticks.x     = element_blank(),
    # axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(mylimits$xmin,mylimits$xmax) , ylim=c(mylimits$ymin,mylimits$ymax)) +

  geom_polygon(               aes(long, lat, group=group, fill=factor(F_SUBAREA)),
                              size=0.1, colour="black", alpha=0.6) +
  # geom_polygon(               aes(long, lat, group=group), 
  #                             fill=NA, size=0.1, colour="black", alpha=0.6) +
  geom_polygon(data=world.df, aes(long, lat, group=group), 
                              fill="cornsilk", size=0.1, colour="gray60") +
  geom_polygon(data=eez.df,   aes(long, lat, group=group), 
                              fill=NA, size=0.5, colour="gray80", alpha=0.3, lty="dashed") +
  # geom_polygon(data=eez.df,   aes(long, lat, group=group, fill=factor(MRGID)), 
  #              size=0.5, colour="gray80", alpha=0.3, lty="dashed") +
  
  geom_text(data=mynames, aes(long, lat, label = F_CODE), size=4, fontface="bold")


cnames <-
  fao.df %>% 
  # fao27.df %>% 
  filter(F_LEVEL == "DIVISION") %>% 
  group_by(id, F_DIVISION) %>% 
  summarize(long = mean(range(long), na.rm=TRUE),
            lat  = mean(range(lat), na.rm=TRUE)) %>% 
  mutate(F_DIVISION = str_replace(F_DIVISION,"27.",""),
         F_DIVISION = str_replace(F_DIVISION,"34.",""),
         F_DIVISION = gsub("\\.","", F_DIVISION)) %>% 
  filter(substr(F_DIVISION,1,1) %in% c("4","6","7"))

cnames2 <-
  cnames %>% 
  filter(F_DIVISION != "6a") %>% 
  rbind(data.frame(id=c(999,999),F_DIVISION=c("6an","6as"),long=c(-10,-10),lat=c(58,55.25)))

fao27.4 <-
  fao27.df %>% 
  filter(F_LEVEL   == "SUBAREA",
         F_SUBAREA == "27.4") 

fao27.6 <-
  fao27.df %>% 
  filter(F_LEVEL   == "SUBAREA",
         F_SUBAREA == "27.6") 

fao27.7 <-
  fao27.df %>% 
  filter(F_LEVEL   == "DIVISION",
         F_DIVISION %in% c("27.7.b","27.7.c","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k")) 

fao27.7de <-
  fao27.df %>% 
  filter(F_LEVEL   == "DIVISION",
         F_DIVISION %in% c("27.7.d","27.7.e"))

fao27.7a <-
  fao27.df %>% 
  filter(F_LEVEL   == "DIVISION",
         F_DIVISION %in% c("27.7.a"))


irishclosure <-
  fao27.df %>% 
  filter(F_LEVEL   == "DIVISION",
         F_DIVISION %in% c("27.7.b","27.7.g","27.7.h","27.7.j")) %>% 
  mutate(period="04-08")


fao.df %>%
# fao27.df %>%
  filter(F_AREA =="34") %>% 
  filter(F_LEVEL == "DIVISION") %>% 
  ggplot() + 
  theme_publication() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    # panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  # coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  
  geom_polygon(data=fao27.4, aes(long, lat, group=group), fill="blue", size=1.0, colour="black") +
  geom_polygon(data=fao27.6, aes(long, lat, group=group), fill="red", size=1.0, colour="black") +
  geom_polygon(data=fao27.7, aes(long, lat, group=group), fill="green", size=1.0, colour="black") +
  geom_polygon(data=fao27.7a, aes(long, lat, group=group), fill="yellow", size=1.0, colour="black") +
  geom_polygon(data=fao27.7de, aes(long, lat, group=group), fill="purple", size=1.0, colour="black") +
  
  geom_polygon(aes(long, lat, group=group), fill=NA, size=0.1, colour="gray60") +
  geom_polygon(data=world.europe.df, aes(long, lat, group=group), fill="gray90", size=0.1, colour="gray60") +
  geom_polygon(data=eez.europe.df, aes(long, lat, group=group), fill=NA, size=0.5, colour="gray20", lty="dashed") +
  
  geom_text(data=cnames, aes(long, lat, label = F_DIVISION), size=4, fontface="bold")


vias <- data.frame(x=c(-12,-7,-7,-12,-12),y=c(56,56,54.5,54.5,56),group=1,period="04-08")

# By area
fao27.df %>%
  filter(F_LEVEL == "DIVISION") %>% 
  ggplot() + 
  theme_publication() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  
  # geom_polygon(data=fao27.4, aes(long, lat, group=group), fill="blue", size=1.0, colour="black") +
  # geom_polygon(data=fao27.6, aes(long, lat, group=group), fill="red", size=1.0, colour="black") +
  # geom_polygon(data=fao27.7, aes(long, lat, group=group), fill="green", size=1.0, colour="black") +
  # geom_polygon(data=fao27.7a, aes(long, lat, group=group), fill="yellow", size=1.0, colour="black") +
  # geom_polygon(data=fao27.7de, aes(long, lat, group=group), fill="purple", size=1.0, colour="black") +
  geom_polygon(data=irishclosure, aes(long, lat, group=group), fill="green", size=1.0, colour="black") +
  geom_polygon(data=vias, aes(x, y, group=group), fill="green", size=1.0, colour="black") +

  geom_polygon(aes(long, lat, group=group), fill=NA, size=0.1, colour="gray60") +
  geom_polygon(data=world.europe.df, aes(long, lat, group=group), fill="gray90", size=0.1, colour="gray60") +
  geom_polygon(data=eez.europe.df, aes(long, lat, group=group), fill=NA, size=0.5, colour="gray20", lty="dashed") +
  
  geom_text(data=cnames2, aes(long, lat, label = F_DIVISION), size=4, fontface="bold")

  # geom_text(data=filter(cnames, F_DIVISION %in% c("4a","4b","4c")), 
  #           aes(long, lat, label = F_DIVISION), size=4, fontface="bold")
  # geom_text(data=filter(cnames, F_DIVISION %in% c("6a","6b")), 
  #           aes(long, lat, label = F_DIVISION), size=4, fontface="bold")
  # geom_text(data=filter(cnames, F_DIVISION %in% c("7b","7c","7f","7g","7h","7j","7k")),
  #         aes(long, lat, label = F_DIVISION), size=4, fontface="bold")
  # geom_text(data=filter(cnames, F_DIVISION %in% c("7a")), 
  #         aes(long, lat, label = F_DIVISION), size=4, fontface="bold")
  # geom_text(data=filter(cnames, F_DIVISION %in% c("7d","7e")), 
  #         aes(long, lat, label = F_DIVISION), size=4, fontface="bold")


# ===========================================================================
# Generic plot of certain FAO areas
# ===========================================================================
myareas  <- c(
  "27.1.b"
  # "27.5.a.1", "27.5.a.2",
  # "27.5.b.1.a","27.5.b.1.b", "27.5.b.2",
  # "27.6.b.1", "27.6.b.2",
  # "27.7.c.1", "27.7.j.1", "27.7.k.1",
  # "27.8.e.1", "27.8.d.2","27.8.a","27.8.b","27.8.c",
  # "27.9.b.1","27.9.a",
  # "27.12.a.1","27.12.a.2", "27.12.a.3","27.12.a.4",
  # "27.12.b","27.12.c","27.12.d"
  ) 
mycolour <- "pink" 
myrange <-
  fao.df %>% 
  filter(tolower(F_CODE) %in% myareas) %>%
  group_by(F_CODE) %>% 
  summarise(xmin = min(long), xmax = max(long), ymin = min(lat), ymax=max(lat)) %>% 
  mutate(x = (xmin+xmax)/2, y = (ymin+ymax)/2 )
  
fao.df %>% 
  filter(tolower(F_CODE) %in% myareas) %>% 

  ggplot() + 
  theme_publication() +
  theme(
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    # axis.text.x      = element_blank(),
    # axis.text.y      = element_blank(),
    # axis.ticks.x     = element_blank(),
    # axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    legend.position  = "none",
    panel.border     = element_rect(colour="black" , size=0.2)
  ) +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(min(myrange$xmin), max(myrange$xmax)) ,
                 ylim=c(min(myrange$ymin), max(myrange$ymax))) +

  geom_polygon(data=eez.df, aes(long, lat, group=group), 
               size=0.5, fill="lightblue", colour="gray20", lty="dashed", alpha=0.5) +
  # geom_polygon(data=filter(iho_eez.df, grepl("High sea", MarRegion)), aes(long, lat, group=group), 
  #              size=0.5, fill="darkblue", colour="gray20", lty="dashed", alpha=0.5) +
  geom_polygon(aes(long, lat, group=group, fill=group), 
               alpha=0.5, size=0.5, colour="gray60") +
  geom_polygon(data=world.df, aes(long, lat, group=group), 
               fill="cornsilk", size=0.1, colour="gray60") +
  geom_text(data=myrange, aes(x, y, label = F_CODE), size=3, fontface="bold")


# ===========================================================================
# Generic plot of FAO areas for a specific aggregation level
# ===========================================================================

mylevel <- "DIVISION"
myareas <- c(34)

myrange <-
  fao.df %>% 
  filter(F_LEVEL== mylevel & F_AREA %in% myareas ) %>%
  group_by(F_CODE) %>% 
  summarise(xmin = min(long), xmax = max(long), ymin = min(lat), ymax=max(lat)) %>% 
  mutate(x = (xmin+xmax)/2, y = (ymin+ymax)/2 )


fao.df %>% 
  filter(F_LEVEL==mylevel & F_AREA %in% myareas) %>%
  
  ggplot() + 
  theme_publication() +
  theme(
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    # axis.text.x      = element_blank(),
    # axis.text.y      = element_blank(),
    # axis.ticks.x     = element_blank(),
    # axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  
  coord_quickmap(xlim=c(myrange$xmin,myrange$xmax), ylim=c(myrange$ymin, myrange$ymax)) +
  # coord_map("simpleconic", lat0=40, lat1=60) +
  
  # EEZ
  geom_polygon(data=eez.df, aes(long, lat, group=group), 
               size=0.5, fill="lightblue", colour="gray20", lty="dashed", alpha=0.5) +
  
  # High seas
  # geom_polygon(data=filter(iho_eez.df, grepl("High sea", MarRegion)), aes(long, lat, group=group), 
  #              size=0.5, fill="darkblue", colour="gray20", lty="dashed", alpha=0.5) +
  
  # 12 NM
  # geom_polygon(data=NM12.df, aes(long, lat, group=group), 
  #              size=0.5, fill="darkred", colour="gray20", lty="dashed", alpha=0.5) +
  
  # FAO
  geom_polygon(aes(long, lat, group=group, fill=factor(F_SUBAREA)), 
               alpha=0.8, size=0.8, colour="black") +
  # geom_polygon(aes(long, lat, group=group), 
  #              fill=NA, size=0.1, colour="gray60") +
  
  # World
  geom_polygon(data=world.df, aes(long, lat, group=group), 
               fill="cornsilk", size=0.1, colour="gray60") +
  
  # Text
  geom_text(data=myrange, aes(x, y, label = F_CODE), size=4, fontface="bold") 





# fao.df %>% filter(grepl("27.6.b", F_CODE)) %>% View()
# fao.df %>% filter(grepl("27.6.b", F_CODE)) %>% select(F_CODE) %>% unique()



fao.df %>% 
  filter(F_LEVEL=="DIVISION" & F_AREA %in% c(21)) %>%
  ggplot() + 
  theme(legend.position="none")+
  coord_map("simpleconic", lat0=40, lat1=50) +
  # coord_map("conic", lat0=40) +
  # coord_map("lambert", lat0=40, lat1=50) +
  # coord_map("albers", lat0=30, lat1=60) +
  geom_polygon(aes(long, lat, group=group, fill=factor(F_SUBAREA)), 
               alpha=0.8, size=0.5, colour="gray60") 

