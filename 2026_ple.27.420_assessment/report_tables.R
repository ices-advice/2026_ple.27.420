## Prepare plots and tables for report


library(icesTAF)
library(dplyr)
library(htmlTable)

mkdir("report")

## SAM fit: run1
load("model/SAM_WGNSSK_model_output.RData")
years <- unique(run1$data$aux[, "year"])

## 1. observed ICES catage: from stock, SOP corrected ----
## rounding applied
catage <- round(read.taf("data/catage.csv"), digit=0)
row.names(catage) <- years[1:nrow(catage)]
#catage <- cbind(catage, total = rowSums(catage))
#catage <- rbind(catage, mean = colMeans(catage))
write.taf(catage, "report/catage.csv")
