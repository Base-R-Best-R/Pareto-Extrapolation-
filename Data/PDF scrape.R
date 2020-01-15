# PDF scraping
# rm(list = ls())
getwd()
# setwd("~/GitHub/Pareto-Extrapolation-/Data")
library(tabulizer)
library(stringr)
library(XLConnect)
##############################################################################################################
# load all pdf's
# Unfortunately in 2016 and statistics austria updated their design and hence we cannot treat the tables prior and after 2016 equally
# for tables in between 2010 and 2015 (similar design)
p <- "Table.pdf"
y1 <- paste(2010:2011, p)
y2 <- paste(2012:2015, p)
y3 <- paste(2016:2018, p)
#
listo <- list(l1 = list(),
     l2 = list(),
     l3 = list())
#
# options(java.parameters = "-Xmx4g" ) #extend memory for rJava
# the above option change is not required for this size of list, still worth meantioning that larger lists may require a manual memory extension
for(k in 1:3){
for(i in seq.int(length(eval(parse(text = paste0("y",k)))))){
  if(k == 3){
    t <- "stream"
  } else{
    t <- "decide"
  }
     listo[[k]][[i]] <- el(extract_tables(eval(parse(text = paste0("y", k, "[", i, "]"))), 
                                             encoding="UTF-8", method = t))
     xlcFreeMemory() # clear Java machine mem. (java specific bottleneck)
}
}
## Currently only scrapes the header after the design update in 2016
# subset
for(i in seq.int(length(y))){
  txt_tables[[i]] <- txt_tables[[i]][4:23,]
}
View(txt_tables[[3]])

#
listo

## WORK IN PROGRES