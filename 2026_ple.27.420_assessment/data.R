# data.R - Preprocess input data into SAM/FLR object

# Author: Chun Chen (WMR) <chun.chen@wur.nl>
#

# INPUT: boot/data/XXX
# OUPUT: data/stock_input_assessment_SAM.RData


library(icesTAF)
library(FLCore)
library(stockassessment)
library(TMB)

mkdir("data")

## Set path
data_path1 <- "boot/data/catchfolder/"
data_path2 <- "boot/data/"

## functions
source("utilities.R")

fleet_name <- "fleet_2026.dat"

# import data -----------------------------------------------------
my_M_file         <- "nm_Peterson_TIV.dat"
file_names        <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat", 
                       "mo.dat", my_M_file, "pf.dat", "pm.dat", "sw.dat", 
                       "fleet_2026.dat")
stock_data        <- lapply(file_names[1:10],function(x)read.ices(file.path(data_path1,x))) 
file_names2       <- file_names[11]
temp              <- lapply(file_names[11],function(x)read.ices(file.path(data_path2,x))) 
stock_data        <- append(stock_data, temp)
names(stock_data) <- unlist(strsplit(file_names,".dat"))
## change fleet name
names(stock_data)[names(stock_data)== sub(".dat", "", fleet_name)] <- "survey"
## change nm name
names(stock_data)[names(stock_data)== "nm_Peterson_TIV"] <- "nm"

## select survey and age
names(stock_data$survey)
surveys <- stock_data$survey[c(1,2,3,4,5)]
names(surveys)

## process survey
surveys[[1]]              <- surveys[[1]][,1:8]
attr(surveys[[1]],"time") <- c(0.66, 0.75)
surveys[[4]]              <- surveys[[4]][,1:6]
attr(surveys[[4]],"time") <- c(0.66, 0.75)
surveys[[5]]              <- surveys[[5]][,1:6]
attr(surveys[[5]],"time") <- c(0.66, 0.75)

## already load time-invariant M, no need to process here
## average M across time
#tmp          <-matrix(rep(colMeans(stock_data$nm), nrow(stock_data$nm)), nrow=nrow(stock_data$nm), byrow=TRUE)
#rownames(tmp)<-rownames(stock_data$nm)
#colnames(tmp)<-colnames(stock_data$nm)
#stock_data$nm<-tmp

#matplot(1957:2022,stock_data$nm, ylab="Peterson and Wroblewski", xlab="")

## set SAM readable object
dat <- setup.sam.data(surveys=surveys,
                      residual.fleet=stock_data$cn, 
                      prop.mature=stock_data$mo, 
                      stock.mean.weight=stock_data$sw, 
                      catch.mean.weight=stock_data$cw, 
                      dis.mean.weight=stock_data$dw, 
                      land.mean.weight=stock_data$lw,
                      prop.f=stock_data$pf, 
                      prop.m=stock_data$pm, 
                      natural.mortality=stock_data$nm,
                      land.frac=stock_data$lf)
names(dat)
#dat$natMor

## save
save(dat, file="data/stock_input_assessment_SAM.RData", compress="xz")

## load and save FLR object
load(paste0(data_path2, "stock_input.RData"))
save(stock, ass.indices, file="data/stock_input_FLR.RData", compress="xz")

## --- EXPORT csv tables
stocktables <- lapply(metrics(stock, catage=catch.n, latage=landings.n,
                              datage=discards.n, wlandings=landings.wt, wdiscards=discards.wt,
                              wstock=stock.wt, natmort=m, maturity=mat), function(x) plus(flr2taf(x)))

indextables <- lapply(FLQuants( survey.BTS_IBTSQ3=index(ass.indices[["BTS_IBTSQ3"]]),
                                survey.IBTSQ1=index(ass.indices[["IBTSQ1"]]),
                                survey.SNS1=index(ass.indices[["SNS1"]]),
                                survey.SNS2=index(ass.indices[["SNS2"]]),
                                survey.BTS_early=index(ass.indices[["BTS-Isis-early"]])), function(x) plus(flr2taf(x)))

write.taf(c(stocktables, indextables), dir="data")
