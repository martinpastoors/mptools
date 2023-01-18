# --------------------------------------------------------------------------------
# read_sam.r
#
# Read sam outputs and convert to tidy format
#
# 01/09/2018 First coding. 
# --------------------------------------------------------------------------------

library(stockassessment)

# devtools::install_github("einarhjorleifsson/readsam")
# library(readsam)
library(tidyverse)

# name           <- "WGWIDE2018.1"
# stockname      <- "mac.27.nea"
# assessmentyear <- 2018
# replacerecruit <- TRUE
# savepath       <- "D:/WGWIDE/2018 Meeting Docs/06. Data/mac.27.nea/output/tidy"

name           <- "BW_2018"
stockname      <- "whb.27.1-91214"
assessmentyear <- 2018
replacerecruit <- FALSE
savepath       <- "D:/WGWIDE/2018 Meeting Docs/06. Data/whb.27.1-91214/output/tidy"

# input-by-year-and-age, from assessment.org
ibya <- 
  readsam::read_ibya(name) %>% 
  mutate(
    stock = stockname, 
    assessmentyear = assessmentyear
  )

# get fit from assessment.org
f <- readsam:::get_fit(name)

# get summary data
fy        <- min(f$data$years)
ly        <- max(f$data$years)
fa        <- f$conf$minAge
la        <- f$conf$maxAge
fbarrange <- paste(f$conf$fbarRange, collapse="-")

# results-by-year-and-age
rbya <- 
  readsam::read_rbya_sam(f, ibya) %>% 
  mutate(
    stock          = stockname, 
    assessmentyear = assessmentyear,
    n              = ifelse(replacerecruit & age == fa & year == ly, NA, n)
  )

# results-by-year
rby <-
  readsam::read_rby_sam(f, ibya) %>% 
  mutate(
    year           = ifelse(variable == "catch", year+min(ibya$year), year),
    variable       = ifelse(variable == "fbar", paste0("fbar",fbarrange), variable),
    Estimate       = ifelse(replacerecruit & variable == "rec" & year == ly, NA, Estimate),
    Low            = ifelse(replacerecruit & variable == "rec" & year == ly, NA, Low),
    High           = ifelse(replacerecruit & variable == "rec" & year == ly, NA, High),
    variable       = ifelse(variable == "rec" , paste0("rec",fa), variable),
    stock          = stockname, 
    assessmentyear = assessmentyear
  ) 

# save file rdata file
save(ibya, rbya, rby, file=file.path(savepath, "tidy.RData"))

# save csv files
write.csv(ibya, file=file.path(savepath, "ibya.csv"), row.names = FALSE)
write.csv(rbya, file=file.path(savepath, "rbya.csv"), row.names = FALSE)
write.csv(rby,  file=file.path(savepath, "rby.csv"), row.names = FALSE)


# plot rby
rby %>% 
  dplyr::filter(variable != "tsb") %>% 
  
  ggplot(aes(x=year, y=Estimate)) +
  theme_bw() +
  theme(legend.position = "none") +

  geom_ribbon(aes(ymin = Low, ymax=High, fill=variable), alpha=0.4) +
  geom_line(aes(colour=variable), size=1) +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y") +
  labs(x="", y="", title=stockname)






