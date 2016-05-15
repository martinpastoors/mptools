# demo: letting user choose a response variable
#       here use standard evaluation, see: 
library(dplyr)
df <- data_frame(year = c(2000:2010),
                 age = c(10:0),
                 oU = c(10:0), # e.g. survey index
                 oC = c(10:20)) # e.g. catch at age
little_helper <- function(df, variable) {
  df <- 
    df %>% 
    # note: the "underscore" in the next step
    #       which means we have to refer to column names inside quotation
    #       further reading: https://cran.rstudio.com/web/packages/dplyr/vignettes/nse.html
    select_("year", "age", value = variable) %>% 
    mutate(yc = year - age)
  return(df)
  # here could do a plot where we call e.g. aes(  , y = value)
}
little_helper(df, "oU")
little_helper(df, "oC")
