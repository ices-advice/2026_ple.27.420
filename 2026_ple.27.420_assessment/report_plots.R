## Prepare plots and tables for report

library(icesTAF)
library(ggplot2)


mkdir("report")

## load input data
load("data/stock_input_assessment_FLR.RData")

## load model
load("model/SAM_WGNSSK_model_output.RData")
fit <- run1

## load retro
load("model/SAM_WGNSSK_retro.RData")



## model output plots ##
taf.png("summary", width = 1600, height = 2000)
plot(fit)
dev.off()

taf.png("SSB")
ssbplot(fit, addCI = TRUE)
dev.off()

taf.png("Fbar")
fbarplot(fit, xlab = "", partial = FALSE)
dev.off()

taf.png("Rec")
recplot(fit, xlab = "")
dev.off()

taf.png("Landings")
catchplot(fit, xlab = "")
dev.off()

taf.png("retrospective", width = 1600, height = 2000)
plot(re)
dev.off()


summary_catch <-
  read.taf("data/summary_catch.csv")

# catch time series
taf.png("summary_catch")
print(
  ggplot(data = summary_catch, aes(x = Year, y = Total)) +
    geom_bar(stat = "identity", fill = taf.blue) +
    ylab("Total Catch (t)") +
    theme_minimal()
)
dev.off()


taf.png("catches_by_halfyear_stack")
print(
  p_halfyear +
    geom_area(position = 'stack')
)
dev.off()

taf.png("catches_by_halfyear_bar")
print(
  p_halfyear +
    geom_bar(stat = "identity", position=position_dodge())
)
dev.off()
