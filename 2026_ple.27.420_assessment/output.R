## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(flextable)
library(FLCore)
library(stockassessment)
library(rmarkdown)

mkdir("output")

## load input
load("data/stock_input_FLR.RData") ## stock, ass.indices
## load model output: 
load("model/FLR_WGNSSK_model_output.RData") ## ple4
load("model/SAM_WGNSSK_model_output.RData") ## run1
load("model/SAM_WGNSSK_retro.RData") ## re

dy <- 2025

## 1. model output and parameters into a word document
render("output_sam_fit.Rmd",
       output_file = "output_sam_fit.docx",
       encoding = "UTF-8")
cp("output_sam_fit.docx", "output", move = TRUE)

## 2. save into tables

## Model Parameters
partab <- partable(run1)

## Fs
## apply rounding?
fatage <- faytable(run1)
fatage <- as.data.frame(fatage)

## Ns
## apply rounding
natage <- as.data.frame(ntable(run1))

## estimated Catch
## apply rounding
catab <- as.data.frame(catchtable(run1))
colnames(catab) <- c("Catch", "Low", "High")

# TSB
tsb <- as.data.frame(tsbtable(run1))
colnames(tsb) <- c("TSB", "Low", "High")

# Summary Table
tab.summary <- cbind(as.data.frame(summary(run1)), tsb)
tab.summary <- cbind(tab.summary, catab)
# should probably make Low and High column names unique R_Low etc.

mohns_rho <- mohn(re)
mohns_rho <- as.data.frame(t(mohns_rho))

## Write tables to output directory
write.taf(
  c("partab", "tab.summary", "natage", "fatage", "mohns_rho"),
  dir = "output"
)

# --- TABLES

tables <- qtables <- list()

# - Model parameters (modparams)

# TODO Intercatch

# - Time-series landings at age (in thousands) 
qtables$landings.n <- landings.n(stock)

# - Time-series discards at age (in thousands) 
qtables$discards.n <- discards.n(stock)[, ac(2002:dy)]

# - Time-series of the mean weights-at-age in the landings 
qtables$landings.wt <- landings.wt(stock)

# - Time-series of the mean weights-at-age in the discards of 
qtables$discards.wt <- discards.wt(stock)

# - Time-series of mean stock weights at age 
qtables$stock.wt <- stock.wt(stock)

# - Survey indices used in the assessment of 

qtables$BTS_IBTSQ3 <- index(ass.indices[["BTS_IBTSQ3"]])

qtables$IBTSQ1     <- index(ass.indices[["IBTSQ1"]])
qtables$SNS1       <- index(ass.indices[["SNS1"]])
qtables$SNS2       <- index(ass.indices[["SNS2"]])
qtables$BTS_early       <- index(ass.indices[["BTS-Isis-early"]])

# - Numbers-at-age

qtables$stock.n <- stock.n(ple4)

# - Fishing mortality-at-age

qtables$harvest <- harvest(ple4)

# ftables

ftables <- lapply(qtables, as_flextable)

# SSB and F w/error
tssb <-   data.frame(Year=row.names(ssbtable(run1)),ssbtable(run1))
colnames(tssb) <- c("Year", "SSB", "SSB lower", "SSB upper")
tfbar <- fbartable(run1)
colnames(tfbar) <- c("F", "F lower", "F upper")
tables$ssbf <- cbind(tssb, tfbar)
ftables$ssbf <- flextable(tables$ssbf)

# - Time-series of the official landings by country

#setnames(stats, c("year", "other", "official", "bms", "ices", "tac"),
#         c("Year", "Other", "Official", "BMS", "ICES", "TAC"))

#tables$catches <- stats
#ftables$catches <- flextable(stats)

# MAT & M

#matm <- data.table(model.frame(FLQuants(Maturity=mat(run)[,'2019'],
#                                        M=m(run)[,'2019']), drop=TRUE))
#setnames(matm, "age", "Age")

#tables$matm <- matm
#ftables$matm <- autofit(flextable(matm))

# REFERENCE POINTS


# SAVE tables

save(tables, ftables, file="output/tables.RData", compress="xz")




# FLStock for WGMIXFISH

landings(ple4) <- computeLandings(ple4)
discards(ple4) <- computeDiscards(ple4)
catch(ple4)    <- computeCatch(ple4)

save(ple4, file="output/ple27.420_FLStock_WGMIXFISH.RData", compress="xz")

# COPY
#cp("data/stock_input_assessment_FLR.RData", "output/")
#cp("model/FLR_WGNSSK_model_output_2023.RData", "output/")
