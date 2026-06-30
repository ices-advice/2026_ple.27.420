## Prepare plots and tables for report

## Before: bootstrap/
##           data/
##             sandeel_assessment_1982_2018.csv
##         output/
##           summary.csv
## After: report/
##          catch_table.csv
##          sumary.png
##          catches_by_halfyear_bar
##          Catches_by_halfyear_stack

library(rmarkdown)

source("utilities.R")

mkdir("report")

# combine into a word document
render("report.Rmd",
       output_file = "report.docx",
       encoding = "UTF-8")
cp("report.docx", "report", move = TRUE)
