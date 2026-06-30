# model.R - Run analysis, write model results
# Author: Chun Chen (WMR) <iago.mosqueira@wur.nl>
#


# INPUT: data/XX
# OUPUT: model/

library(icesTAF)
library(FLfse)
library(FLCore)
library(stockassessment)

mkdir("model")

## functions
source("utilities.R")

## load input data: dat
load('data/stock_input_assessment_SAM.RData')

## settings 
## mean F ages (total&landings and for discards)
meanFages  <- c(2,6)
meanFDages <- c(2,3)

## minimum age
minAge     <- 1

## Plusgroup age
pGrp       <- 10

## SAM configuration
rm(conf)
source("SAM_config.R")

## Fit the model
conf1   <- conf
par     <- defpar(dat, conf1)
run1    <- sam.fit(dat,conf1, par)  

# Convergence checks
run1$opt$convergence          # 0 = convergence
run1$opt$message              # 4 = ok
AIC(run1)


## residuals 
res  <- residuals(run1)
resp <- procres(run1)

## retro
re   <- retro(run1, year=5)

#plot(re)
#catchplot(re)
#recplot(re)
#mohn(re)

## leave one out
loo  <- leaveout(run1)

## save
save(run1, file=paste0("model/SAM_WGNSSK_model_output.RData"))
saveConf(conf1 , file = paste0("model/SAM_WGNSSK_.cfg"))
save(loo, file=paste0("model/SAM_WGNSSK_loo.RData"))
save(res, resp, file=paste0("model/SAM_WGNSSK_residual.RData"))
save(re, file=paste0("model/SAM_WGNSSK_retro.RData"))

## SAM to FLSTOCK ple4-------------
ple4        <- SAM2FLStock(run1)
units(ple4) <- standardUnits(ple4)
save(ple4, file=paste0("model/FLR_WGNSSK_model_output.RData"))


