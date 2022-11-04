#' @title Converts FLStock object to rbya
#'
#' @description The rbya (results by year and age) is a long \code{data.frame}
#' popular in Reykjavik
#'
#' @export
#'
#' @param x An FLStock object
#' @param scale scaler used on abundance values (stock in numbers, catches etc)
#' @param project A boolean, if TRUE (default), propagates terminal stock numbers
#' forward by one year (into the assessment year). Note that the weights in the
#' assessment year are the same as in the terminal year.
#' @param plusgroup A boolean, if TRUE (default), last age group is a plus group.
#' Only used if project is TRUE.

tidy_flstock <- function(x, scale=1, project = FALSE, plusgroup = TRUE)

{
  
  # ibya
  
  # rbya
  y    <- reshape2::melt(x@stock.n@.Data,value.name = "n")[,c("year","age","n")]
  y$n  <- y$n/scale
  y$f  <- reshape2::melt(x@harvest@.Data)[,c("value")]
  # if(class(x) != "FLSAM") {  # This may be needed
  y$oC <- reshape2::melt(x@catch.n@.Data)[,c("value")]/scale
  y$cW <- reshape2::melt(x@catch.wt@.Data)[,c("value")]
  y$sW <- reshape2::melt(x@stock.wt@.Data)[,c("value")]
  y$oD  = reshape2::melt(x@discards.n@.Data)[,c("value")]/scale
  y$dW  = reshape2::melt(x@discards.wt@.Data)[,c("value")]
  y$oL  = reshape2::melt(x@landings.n@.Data)[,c("value")]/scale
  y$lW  = reshape2::melt(x@landings.wt@.Data)[,c("value")]
  y$mat = reshape2::melt(x@mat@.Data)[,c("value")]
  y$pF  = reshape2::melt(x@harvest.spwn@.Data)[,c("value")]
  y$pM  = reshape2::melt(x@m.spwn@.Data)[,c("value")]
  y$m   = reshape2::melt(x@m@.Data)[,c("value")]
  
  # propagate stock forward
  if (project) {
    y2 <- y[y$year == max(y$year),]
    y2$year <- y2$year + 1
    y2$n <- y2$n * exp(-(y2$m + y2$f))
    if(plusgroup) {
      y2$n[(nrow(y2)-1)] <- y2$n[(nrow(y2)-1)] + y2$n[nrow(y2)]
    }
    y2$n <- c(NA, y2$n[2:length(y2$n)])
    y2$f <- y2$oC <- y2$oD <- y2$oL <- NA
    
    y <- rbind(y, y2)
  }
  
  # rby
  
  return(dplyr::as_data_frame(y))
  
}

minage       <- fit$data$minAge[[1]]
maxage       <- fit$data$maxAge[[1]]
pg           <- ifelse(fit$conf$maxAgePlusGroup[1]==1,fit$data$maxAge[[1]],NA)
minyear      <- min(fit$data$years)
maxyear      <- max(as.numeric(rownames(cn)))  # max year from data
maxyearsam   <- max(fit$data$years)            # max year from assessment
minfbar      <- fit$conf$fbarRange[1]
maxfbar      <- fit$conf$fbarRange[2]

# Generate FLStock object
FLS  <- FLStock()

# set generate properties
FLS@desc               <- paste("FLStock object generated from SAM:", date(), sep=" ")
FLS@name               <- sao.name
FLS@range["min"]       <- minage
FLS@range["max"]       <- maxage
FLS@range["plusgroup"] <- pg
FLS@range["minyear"]   <- minyear
FLS@range["maxyear"]   <- maxyear
FLS@range["minfbar"]   <- minfbar
FLS@range["maxfbar"]   <- maxfbar

units(FLS)             <- FLCore::standardUnits(FLS)

FLS@catch.n            <- fqs[["catch.n"]] 
FLS@landings.n         <- fqs[["catch.n"]] * fqs[["landings.fraction"]]
FLS@discards.n         <- fqs[["catch.n"]] * (1-fqs[["landings.fraction"]])
FLS@m                  <- fqs[["m"]] %>% FLCore::window(., end=maxyear)
FLS@mat                <- fqs[["mat"]] %>% FLCore::window(., end=maxyear)
FLS@catch.wt           <- fqs[["catch.wt"]]
FLS@landings.wt        <- fqs[["landings.wt"]]
FLS@discards.wt        <- fqs[["discards.wt"]]
FLS@stock.wt           <- fqs[["stock.wt"]] %>% FLCore::window(., end=maxyear)
FLS@harvest.spwn       <- fqs[["harvest.spwn"]] %>% FLCore::window(., end=maxyear) 
FLS@m.spwn             <- fqs[["m.spwn"]] %>% FLCore::window(., end=maxyear)
FLS@landings <- FLS@discards <- FLS@catch  <- FLS@stock <-
  FLQuant(NA, dimnames=list(age="all", year=minyear:maxyear))
FLS@landings          <- quantSums(FLS@catch.n * FLS@catch.wt)
FLS@discards          <- quantSums(FLS@discards.n * FLS@discards.wt)
FLS@catch             <- quantSums(FLS@catch.n * FLS@catch.wt)

# stock numbers
FLS@stock.n          <- FLQuant(NA, dimnames=list(age=minage:maxage, year=minyear:maxyearsam))
FLS@stock.n[,]       <- exp(fit$pl$logN) 
FLS@stock.n          <- FLCore::window(FLS@stock.n, end=maxyear)

# harvest
n.ages               <- nrow(fit$pl$logF)
FLS@harvest          <- FLQuant(NA, dimnames=list(age=minage:maxage, year=minyear:maxyearsam))
FLS@harvest[minage:(minage+n.ages),] <- exp(fit$pl$logF)
FLS@harvest[(n.ages+1),]    <- FLS@harvest[n.ages,]
FLS@harvest          <- FLCore::window(FLS@harvest, end=maxyear)
units(FLS@harvest)   <-  "f"

# ssb
FLS@stock            <- ssb(FLS)
