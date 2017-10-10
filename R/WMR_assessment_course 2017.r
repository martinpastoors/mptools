# ------------------------------------------------------------------------
# WMR assessment course 
#
# 9-10 October 2017
# Niels Hintzen and Thomas Brunel
# ------------------------------------------------------------------------

rm(list=ls())

install.packages(c("FLFleet","ggplotFL"), repos="http://flr-project.org/R")
install.packages("TMB")
install.packages("ellipse")
install.packages("Matrix")


library(FLCore)
library(FLFleet)
library(tidyverse)
library(ggplotFL)
library(FLSAM)
library(cowplot)

# -------------------------------------------------------------------------
# Some general stuff on FLR
# -------------------------------------------------------------------------

flq <- FLQuant(rlnorm(20), units="kg",
               dimnames=list(age=0:3, year=2010:2014))
plot(flq)

# download example data
dir <- tempdir()
download.file("http://flr-project.org/doc/src/loading_data.zip", file.path(dir, "loading_data.zip"))
unzip(file.path(dir, "loading_data.zip"), exdir=dir)

# read catch.n
catch.n <- read.csv(file.path(dir,"catch_numbers.csv"), row=1)
class(catch.n)

# reshaping as matrix
catch.n.matrix <- as.matrix(catch.n)
catch.n.matrix[,1:8]

# making an flquant
catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=1:7, year = 1957:2011))
catch.n.flq[,1:7]

# Read from a VPA text file
catch.n <- readVPAFile(file.path(dir, "her-irlw","canum.txt"))
class(catch.n)

# Read a collection of VPA files, pointing to the Index file:
her <- readFLStock(file.path(dir, 'her-irlw', 'index.txt'))
class(her)
summary(her)

# add the stock numbers
stock.n(her) <- readVPAFile(file.path(dir, "her-irlw", "n.txt"))
harvest(her) <- readVPAFile(file.path(dir,"her-irlw", "f.txt"))

# The sum of products (SOP)
apply(landings.n(her)*landings.wt(her), 2, sum)[,ac(2007:2011)]


# ---------------------------------------------------------------------
# FLSAM (Niels Hintzen)
# ---------------------------------------------------------------------

# use FLStock object
data(SOL)
summary(SOL)

# use FLSAM object
data(SOL.sam)
summary(SOL.sam)

# Running the SOL assessment.
SOL.sam <- FLSAM(SOL,SOL.tun,SOL.ctrl) #+/- 2min

# add assessment 
SOL <- SOL + SOL.sam

slotNames(SOL.sam)
dim(SOL.sam@vcov)
head(SOL.sam@params,29)
head(SOL.sam@residuals,29)

slotNames(SOL.ctrl)

# In case you change parameters, be careful not to estimate e.g. 1 parameter for 
# 2 completely different datasets. 
# Use: SOL.ctrl <- update(SOL.ctrl)

# create a control object from FLStock and FLIndices
SOL.ctrl <- FLSAM.control(SOL,SOL.tun)

catchabilities(SOL.sam) %>% 
  ggplot(aes(x=age,y=value)) +
  geom_line() +
  facet_wrap(~fleet, scales="free_y")

SOL.ctrl@name <- "Sole in 27.4"

barplot(apply(apply(log(SOL@harvest[,drop=T]),1,diff),2,var))

#- Where to get the results yourself -------------------------------------------

params(SOL.sam)
residuals(SOL.sam)
catchabilities(SOL.sam)
ssb(SOL.sam)
rec(SOL.sam)
fbar(SOL.sam)
tsb(SOL.sam)
n(SOL.sam)
f(SOL.sam)
catch(SOL.sam)
coef(SOL.sam)
n.var(SOL.sam)
f.var(SOL.sam)
#- The ones below are not used in the control file, but once you active them, you can use these
#cor.F(SOL.sam)
#power.law.exps(SOL.sam)
#catch.scale(SOL.sam)

#- Diagnostics -----------------------------------------------------------------
windows(record=TRUE)
residual.diagnostics(SOL.sam)
par(mfrow=c(1,1))
obsvar.plot(SOL.sam)
obscv.plot(SOL.sam)
catch   <- catchabilities(SOL.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),scale=list(y="free"),
             subset=fleet %in% c("SNS","BTS-ISIS")))
cor.plot(SOL.sam)
otolith(SOL.sam,n=1000)
sel.pat <- merge(f(SOL.sam),fbar(SOL.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
             groups=year,type="l",as.table=TRUE,
             scale=list(alternating=FALSE),
             main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

#- Running a retrospective analyses --------------------------------------------
SOL.ctrl@residuals      <- FALSE; SOL.sam@control@residuals <- FALSE
SOL.retro               <- retro(SOL,SOL.tun,SOL.ctrl,retro=7)
save(SOL.retro,file="D:/SOL.retro.RData")

plot(SOL.retro)
mohns.rho(SOL.retro,ref.year=2016,span=7,type="fbar")

#- Documenting the assessment --------------------------------------------------
SOL.sam@control@sam.binary <- "character()"
sam.out.file      <- FLSAM.out(SOL,SOL.tun,SOL.sam,format="TABLE 2.6.3.%i Sole in IV.")

head(sam.out.file,20)
write(sam.out.file,file=paste("D:/","sam.out",sep="."))

#- Doing a benchmark -----------------------------------------------------------
# - change parameter bindings in the catchabilities
# - change observation variance binding
# - include a correlation structure of one of the surveys
# - drop one survey from the model
# - change the plusgroup to e.g. 7+
#   - trim the dataset to start in 1980

SOL.sams <- FLSAMs(original=SOL.sam,benchmark=...)
plot(SOL.sams)
AIC(SOL.sams)
lr.test(SOL.sams)

#- Getting the uncertainty in the assessment -----------------------------------

SOL.mc  <- monteCarloStock(SOL,SOL.tun,SOL.sam,realisations=10)
save(SOL.mc,file="D:/SOL.mc.RData")





# -----------------------------------------------------------------------------------------
# EqSim - reference points
# -----------------------------------------------------------------------------------------

# library(devtools)
# install_github("ices-tools-prod/msy")

# packages 
rm(list=ls())
library(ggplot2)
library(FLCore)
library(msy)


# load the assessment FLStock oject
load(file=paste("r/Stk.RData",sep=""))
# plot(Stk); class(Stk)

#### recruitment models estimation
FIT <- 
  eqsr_fit(Stk, remove.years = c(2014:2016), 
           nsamp = 1000, models=c("Ricker","Segreg","Bevholt"))

FIT2srr <- 
  eqsr_fit(Stk, remove.years = c(2014:2016),
           nsamp = 1000, models = c("Ricker", "Segreg")) 

# short term series and then remove data points from the plotting part
FIT2srr_short <- 
  eqsr_fit(Stk, remove.years = c(1957:1980, 2014:2016),
           nsamp = 1000, models = c("Ricker", "Segreg"))
FIT2srr_short$rby <- dplyr::filter(FIT2srr_short$rby, year %in% c(1981:2013))

FITricker <- 
  eqsr_fit(Stk, remove.years = c(2014:2016), 
           nsamp = 1000, models = c("Ricker"))
FITsegreg <- 
  eqsr_fit(Stk, remove.years = c(2014:2016), 
           nsamp = 1000, models = c("Segreg"))

# looks like the Beverton and Holt model is very unlikely for this stock
# run SR pairs  estimation removing the Beverton and Holt model
eqsr_plot(FIT,n=2e4)
eqsr_plot(FIT2srr,n=2e4)
eqsr_plot(FIT2srr_short,n=2e4)
eqsr_plot(FITricker,n=2e4)
eqsr_plot(FITsegreg,n=2e4)


#look at biological vectors to define appropriate conditionning
plot(stock.wt(Stk),type="l")   # any trend ?
plot(catch.wt(Stk))            # any trend ?
plot(mat(Stk))                 # :( constant
# the default  approach is to take the last 10 years

# look at the selection pattern; Return an array obtained from an input array by sweeping out a summary statistic       
sel <- sweep(x=harvest(Stk),MARGIN=c(2:6),STATS=fbar(Stk),FUN="/")
plot(sel[,ac(1990:2016),])     # recent changes in the selection pattern
# when changes are observed, ICES advises to use only recent values (e.g 3 years)

# configuration of the simulations 
SIM <- eqsim_run(FIT, 
                 bio.years     = c(2007, 2016),           # resampling biol. input over the values of the last 10 years
                 bio.const     = FALSE,                   # not using an average, but actual resampling
                 sel.years     = c(2014, 2016),           # resampling the selection patt. over only the last 3 years 
                 sel.const     = FALSE,                   # not using an average, but actual resampling
                 Fscan         = seq(0.01,0.8 , len = 80),# range of F to scan
                 Fcv           = 0.23, 
                 Fphi          = 0.24,                    # estimated independently (ask Ruben how)
                 SSBcv         = 0.31,                    # 
                 rhologRec     = F,                       # we ignore it for now, but we can speficy autocorrelation in the recruitment
                 Blim          = 26300, 
                 Bpa           = 37000,                   # values based on assessment and its uncertainty
                 Btrigger      = 0,                       # first run without Btrigger
                 Nrun          = 100, 
                 process.error = TRUE, 
                 verbose       = TRUE )

# look at the table of outputs
names(SIM)
SIM$Refs

#  now rerun EQSIM using FITricker and FITsegreg to see the sensitivity of Fmsy to the choice
# of the recruitment model
SIMrick       <- eqsim_run(FITricker, bio.years = c(2007, 2016),bio.const = FALSE,sel.years = c(2014, 2016),sel.const = FALSE,Fscan = seq(0.01,0.8 , len = 80), Fcv = 0.23, Fphi = 0.24,SSBcv = 0.31,rhologRec = F,Blim=26300, Bpa=37000,Btrigger = 0, Nrun = 100,process.error = TRUE, verbose = TRUE )
SIMsegreg     <- eqsim_run(FITsegreg, bio.years = c(2007, 2016),bio.const = FALSE,sel.years = c(2014, 2016),sel.const = FALSE,Fscan = seq(0.01,0.8 , len = 80), Fcv = 0.23, Fphi = 0.24,SSBcv = 0.31,rhologRec = F,Blim=26300, Bpa=37000,Btrigger = 0, Nrun = 100,process.error = TRUE, verbose = TRUE )    
SIM2srr_short <- eqsim_run(FIT2srr_short, bio.years = c(2007, 2016),bio.const = FALSE,sel.years = c(2014, 2016),sel.const = FALSE,Fscan = seq(0.01,0.8 , len = 80), Fcv = 0.23, Fphi = 0.24,SSBcv = 0.31,rhologRec = F,Blim=26300, Bpa=37000,Btrigger = 0, Nrun = 100,process.error = TRUE, verbose = TRUE )    

save(SIM, SIMrick, SIMsegreg, SIM2srr_short, file="Eqsim.RData")

# Get Fmsy
Fmsy             <- round(SIM$Refs["lanF","medianMSY"],2)       ### F=0.35
Fmsyrick         <- round(SIMrick$Refs["lanF","medianMSY"],2)    
Fmsysegreg       <- round(SIMsegreg$Refs["lanF","medianMSY"],2)    
Fmsy2srr_short    <- round(SIM2srr_short$Refs["lanF","medianMSY"],2)    


# Could you calculate Fpa based on Bpa?

# Get MSY Btrigger = 5th percentile of SSB at Fmsy
SIMres            <- SIM$rbp[SIM$rbp$variable == "Spawning stock biomass",c("Ftarget","p05", "p50")]
SIMrickres        <- SIMrick$rbp[SIMrick$rbp$variable == "Spawning stock biomass",c("Ftarget","p05", "p50")]
SIMsegregres      <- SIMsegreg$rbp[SIMsegreg$rbp$variable == "Spawning stock biomass",c("Ftarget","p05", "p50")]
SIM2srr_shortres  <- SIM2srr_short$rbp[SIM2srr_short$rbp$variable == "Spawning stock biomass",c("Ftarget","p05", "p50")]

SIMres_MSYBtrigger  <- SIMres$p05[SIMres$Ftarget == Fmsy ] 
SIMrickres_MSYBtrigger  <- SIMrickres$p05[SIMres$Ftarget == Fmsyrick ] 
SIMsegregres_MSYBtrigger  <- SIMsegregres$p05[SIMres$Ftarget == Fmsysegreg ] 
SIM2srr_shortres_MSYBtrigger  <- SIM2srr_shortres$p05[SIMres$Ftarget == Fmsy2srr_short ] 

# Two plot types
eqsim_plot(SIM,catch=F)

eqsim_plot_range(SIM,type="median")  
eqsim_plot_range(SIMrick,type="median")  
eqsim_plot_range(SIMsegreg,type="median")  
eqsim_plot_range(SIM2srr_short,type="median")  



# ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# MSE
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------
#  this script runs a simple MSE on your stock
#  it is based on the same model and configurations as used in the MSY estimation using EQsim

rm(list=ls())
library(ggplot2)
library(FLCore)
library(msy)

# load the assessment FLStock oject
load(file=paste("D:/TEMP/track 2 (assessments)/MSE/Stk.RData",sep=""))
FLCore::plot(Stk)
summary(Stk)

# this is a modified version of EQsim which runs long-term equilibrium simulations for only on Ftarget value
# and returns the variable the needed to evaluate management performance 
source("D:/TEMP/track 2 (assessments)/MSE/functions.r")  

# recruitment models estimation. No apparent link between recruitment and SSB : use a segmented regression
FIT <- eqsr_fit(Stk, remove.years = c(2014:2016), nsamp = 1000, models = c("Segreg"))
eqsr_plot(FIT,n=2e4)

# 1) run one simulation for a specific management scenario (i.e. a given combination of Ftarget and Btrigger)

SIM <- eqsim_run2(fit = FIT,                         
                  bio.years = c(2007, 2016),         
                  bio.const = FALSE,                 
                  sel.years = c(2014, 2016),         
                  sel.const = FALSE,                 
                  Fscan = seq(0.01,0.8 , len = 80),  
                  Fcv = 0.23, Fphi = 0.24,               
                  SSBcv = 0.31,                          
                  rhologRec = F,                         
                  Blim=26300, Bpa=37000,                 
                  Ftarget = 0.25 ,                    
                  Btrigger = 37000,                      
                  Nrun = 100,                        
                  Nits =500 ,                          
                  process.error = TRUE,              
                  verbose = TRUE,                     
                  recruitment.trim = c(3, -3)     )   

traject.plot( SIM[[1]] )    


# 2) sensitivity test : 

# now run the simulation above using different assessment errors (Fcv, Fphi, SSBcv), different recruitment varibility, different number of iteration
SIM <- eqsim_run2(fit = FIT, bio.years = c(2007, 2016), bio.const = FALSE,  sel.years = c(2014, 2016),  sel.const = FALSE,  Fscan = seq(0.01,0.8 , len = 80),   rhologRec = F,Blim=26300, Bpa=37000,                 Ftarget = 0.25 , Btrigger = 37000, Nrun = 100,Nits =500 , process.error = TRUE,verbose = TRUE,recruitment.trim = c(3, -3)   ,
                  Fcv = 0.10, Fphi = 0.1, SSBcv = 0.10  )   # example of other assessment errors
traject.plot( SIM[[1]] )    

#  recruitment variability can be change by modifying the FIT object  :
cv.change <- 0.5      # e.g. devide by 2 the variability
FIT2 <-FIT
FIT2$sr.sto$cv <-  cv.change *  FIT2$sr.sto$cv
#   and then use FIT2 instead of FIT in the code above
SIM <- eqsim_run2(fit = FIT, bio.years = c(2007, 2016), bio.const = FALSE,  sel.years = c(2014, 2016),  sel.const = FALSE,  Fscan = seq(0.01,0.8 , len = 80),   rhologRec = F,Blim=26300, Bpa=37000,                 Ftarget = 0.25 , Btrigger = 37000, Nrun = 100,Nits =500 , process.error = TRUE,verbose = TRUE,recruitment.trim = c(3, -3)   ,
                  Fcv = 0.23, Fphi = 0.24, SSBcv = 0.31)  
traject.plot( SIM[[1]] )    

#  question :  in this case, what influences the most the risk, population natural variability or assessment uncertainty


# 3) conduct an evaluation of the performance of multiple management scenarios

# define the scenarios
mgt.scen<-list(
  "F0.2_B20kt" = list(Ftarget = 0.2 , Btrigger = 20000) ,
  "F0.5_B20kt" = list(Ftarget = 0.5 , Btrigger = 20000) ,
  "F0.2_B50kt" = list(Ftarget = 0.2 , Btrigger = 50000) ,
  "F0.5_B50kt" = list(Ftarget = 0.5 , Btrigger = 50000)    )


# create the objects to store the results
SIM[[1]]$mgt <- NA           # need to have kept the object from step 1
Sim.res <- data.frame (SIM[[1]][SIM[[1]]$years<1,] )
iavRes<-list()

# run the simulations                                                     
for (sc in names(mgt.scen) )
{
  SIM <- eqsim_run2(fit = FIT,                         
                    bio.years = c(2007, 2016),         
                    bio.const = FALSE,                 
                    sel.years = c(2014, 2016),         
                    sel.const = FALSE,                 
                    Fscan = seq(0.01,0.8 , len = 80),  
                    Fcv = 0.23, Fphi = 0.24,               
                    SSBcv = 0.31,                          
                    rhologRec = F,                         
                    Blim=26300, Bpa=37000,                 
                    Ftarget  = mgt.scen[[sc]]$Ftarget ,                    
                    Btrigger = mgt.scen[[sc]]$Btrigger,                      
                    Nrun = 100,                        
                    Nits =1000 ,                          
                    process.error = TRUE,              
                    verbose = TRUE,                     
                    recruitment.trim = c(3, -3)     )   
  SIM[[1]]$mgt <- sc
  Sim.res <- rbind(Sim.res,SIM[[1]])  
  
  iav<-as.data.frame(SIM[[2]])
  iav$mgt <- sc
  iavRes[[sc]] <- SIM[[2]]
  
}                                                     

# visualise the output
traject.plot.mgt( Sim.res )    
diagnostics(Sim.res,iavRes)         

#  Question : try to describe the tradeoff managers would be facing when having to choose from these scenarios                                            


# ---------------------------------------------------------------------------------
# Short term forecast
# ---------------------------------------------------------------------------------

# START R3.3.3 in 32-bit mode !!!!!!!!!!!!
if (Sys.getenv("R_ARCH") != "/i386") {stop("R is in 64 bit mode; please change to 32 bit model")}

# install.packages(c("FLAssess"), repos="http://flr-project.org/R")
# Note: answer no if asked whether to compile the package !!!!

library(FLSAM)
library(FLAssess)

#- Load the data (you can use the readFLStock option too, but this is a bit quicker)
data(SOL)
data(SOL.sam)
load(file="SOL.mc.RData")

#- Assumptions
SOL.stf   <- stf(SOL,nyears=10)
SOL.stf   <- stf(SOL,nyears=10, wts.nyears =10, fbar.nyears=10)

# Inspect
SOL.stf@mat[,ac(2017:2026)]
SOL.stf@m[,ac(2017:2026)]
SOL.stf@harvest[,ac(2017:2026)]
SOL.stf@stock.wt[,ac(2017:2026)]
SOL.stf@catch.wt[,ac(2017:2026)]

#- Different stock-recruitment models

# Geometric mean
SOL.srrME <- list(model="geomean",params=FLPar(exp(mean(log(rec(SOL[,ac(2007:2016)]))))))

# Beverton Holt
SOL.srrBH <- FLSR(
  rec = rec(SOL)[,ac((range(SOL)["minyear"]+1): range(SOL)["maxyear"])],
  ssb = ssb(SOL)[,ac((range(SOL)["minyear"])  :(range(SOL)["maxyear"]-1))],
  model='bevholt')
SOL.srrBH <- fmle(SOL.srrBH)  # minimizer
plot(SOL.srrBH)

# Ricker
SOL.srrRI <- FLSR(
  rec = rec(SOL)[,ac((range(SOL)["minyear"]+1): range(SOL)["maxyear"])],
  ssb = ssb(SOL)[,ac((range(SOL)["minyear"])  :(range(SOL)["maxyear"]-1))],
  model='ricker')
SOL.srrRI <- fmle(SOL.srrRI)
plot(SOL.srrRI)

# Define a custom SR AR1 model
rickerAR1 <- function() {
  ## log-likelihood
  logl <- function(a, b, rho, rec, ssb)
    loglAR1(log(rec), log(a*ssb*exp(-b*ssb)), rho=rho)
  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2]), rho=0))},
    lower=rep(-Inf, 3),
    upper=rep( Inf, 3))
  ## model to be fitted
  model  <- rec~a*ssb*exp(-b*ssb)
  return(list(logl=logl, model=model, initial=initial))}

# now estimate the AR1 model
SOL.srrRiAR1 <- SOL.srrRI
model(SOL.srrRiAR1)<-rickerAR1()
SOL.srrRiAR1 <- fmle(SOL.srrRiAR1)
plot(SOL.srrRiAR1)

#- Do the forecast with geomean recruitment
fwd.ctrl  <- fwdControl(data.frame(year=c(2017:2026),
                                   quantity="f",
                                   val=c(0.2)))
SOL.stf   <- fwd(SOL.stf,ctrl=fwd.ctrl,sr=SOL.srrME)

#- Combination of catch and F (0.2) based scenarios
fwd.ctrl  <- fwdControl(data.frame(year=c(2017:2026),
                                   quantity=c("catch","catch",rep("f",8)),
                                   val=c(16123,14900,rep(0.2,8))))
SOL.stf   <- fwd(SOL.stf,ctrl=fwd.ctrl,sr=SOL.srrME)
fbar(SOL.stf)

#- Shorter prediction
SOL.stf   <- stf(SOL,nyears=3)
fwd.ctrl  <- fwdControl(data.frame(year=c(2017:2019),
                                   quantity="f",
                                   val=c(0.2)))
SOL.stf   <- fwd(SOL.stf,ctrl=fwd.ctrl,sr=SOL.srrME)

# calculate some metrics
ssb(SOL.stf)
fbar(SOL.stf)
rec(SOL.stf)
catch(SOL.stf)

#- Scan over range of F values
fmult.targs  <- seq(0, 2, by=0.1)

# create list to store the forecasts
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year=c(2017:2026),
                        quantity=c("catch","catch",rep("f",8)),
                        val=c(16123,14900,rep(fmult,8))))
  })
names(mult.opts.l) <- sprintf("Fmult(2017) = %4.3f",fmult.targs)

# run the forecasts
SOL.mult.opts <- lapply(mult.opts.l,function(ctrl) {
  fwd(SOL.stf,ctrl=ctrl,sr=SOL.srrME)
  }) 

mult.table    <- do.call(rbind,lapply(SOL.mult.opts,function(x){
  return(x@catch[,ac(2017:2026)])
}))
colnames(mult.table) <- 2017:2026
matplot(t(mult.table),type="b")

mult.table    <- do.call(rbind,lapply(SOL.mult.opts,function(x){
  return(fbar(x[,ac(2017:2026)]))
}))
colnames(mult.table) <- 2017:2026
matplot(t(mult.table),type="b")

mult.table    <- do.call(rbind,lapply(SOL.mult.opts,function(x){
  return(ssb(x[,ac(2017:2026)]))
}))
colnames(mult.table) <- 2017:2026
matplot(t(mult.table),type="b")


#- Forecast with multiple iterations (from the monte-carlo stock)
data(SOL)
data(SOL.sam)
SOL.stf   <- stf(SOL.mc,nyears=10)
fwd.ctrl  <- fwdControl(data.frame(year=c(2017:2026),
                                   quantity=c("catch","catch",rep("f",8)),
                                   val=c(16123,14900,rep(0.2,8))))
SOL.stf   <- fwd(SOL.stf,ctrl=fwd.ctrl,sr=SOL.srrME)
fbar(SOL.stf)
plot(SOL.stf)

#- Forecast from retrospective run
data(SOL)
data(SOL.sam)
load("SOL.retro.RData")
SOL.stfs    <- list()
iYr <- 2015
for(iYr in 2016:2010){
  SOL       <- window(SOL, end=iYr) + SOL.retro[[ac(iYr)]]
  
  SOL.stf   <- stf(SOL,nyears=2019-iYr)
  SOL.srrME <- list(model="geomean",params=FLPar(exp(mean(log(rec(SOL[,ac((iYr-9):iYr)]))))))
  fwd.ctrl  <- fwdControl(data.frame(year=c((iYr+1):2019),
                                     quantity="f",
                                     val=c(0.2)))
  SOL.stf   <- fwd(SOL.stf,ctrl=fwd.ctrl,sr=SOL.srrME)
  SOL.stfs[[ac(iYr)]] <- SOL.stf
}
SOL.stfs    <- lapply(SOL.stfs,window,start=2015)
SOL.stfs    <- as(SOL.stfs,"FLStocks")
plot(SOL.stfs)
pldat       <- as.data.frame(lapply(SOL.stfs,catch))
xyplot(data ~ year,group=qname,data=pldat,type="l",auto.key=T)

# Each year is a retro year
rec(SOL.stfs[["2016"]])
harvest(SOL.stfs[["2010"]])

summary(SOL)
