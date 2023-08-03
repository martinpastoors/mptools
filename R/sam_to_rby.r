# --------------------------------------------------------------------------------
# sam_to_rby.r
#
# Read sam outputs and convert to tidy format
#
# 01/09/2018 First coding. 
# --------------------------------------------------------------------------------

library(stockassessment)
library(tidyverse)

assessmentyear <- 2022
name           <- paste0("BW-", assessmentyear)
stockname      <- "whb.27.1-91214"
replacerecruit <- FALSE
datapath       <- paste0("D:/ICES/WGWIDE/",assessmentyear," Meeting Docs/06. Data/",stockname,"/output/tidy")
savepath       <- "D:/ICES/WGWIDE/2018 Meeting Docs/06. Data/whb.27.1-91214/output/tidy"

get_file <- function(URL, File)
{
  temporaryFile <- tempfile()
  utils::download.file(paste(URL, File, sep = "/"),
                       destfile = temporaryFile,
                       method = "curl",
                       quiet = T)
  return(temporaryFile)
}


get_fit <- function(ass, web = TRUE, user = "user3") {
  
  url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
               user,
               ass,
               "run",
               sep="/")
  
  fil <- get_file(url, "model.RData")
  attach(fil, pos = 2)
  fit <- fit
  detach(pos = 2)
  return(fit)
  
}


# get fit from assessment.org
f <- get_fit(name)

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






