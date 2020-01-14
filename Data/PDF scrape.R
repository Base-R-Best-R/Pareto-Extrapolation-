#PDF scraping
#setwd("~/GitHub/Pareto-Extrapolation-/Data")
#install.packages("pdftools")
library(tabulizer)
library(stringr)
install.packages("XLConnect")
library(XLConnect)
##############################################################################################################
# load all pdf's
# Unfortunately in 2016 statistics austria updated their design and hence we cannot treat the tables prior and after 2016 equally
#for tables in between 2010 and 2015 (similar design)
txt_tables <- list()
y <- paste(2010:2015, "Table.pdf")
#
# options(java.parameters = "-Xmx4g" ) #extend memory for rJava
for(i in seq.int(length(y))){
  txt_tables[[i]] <- el(extract_tables(y[i], encoding="UTF-8"))
  xlcFreeMemory() # clear Java machine mem. (java specific bottleneck)
}
# subset
for(i in seq.int(length(y))){
  txt_tables[[i]] <- txt_tables[[i]][4:23,]
}
View(txt_tables[[3]])
## WORK IN PROGRES