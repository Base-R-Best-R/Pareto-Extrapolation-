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
    n <- F
    l <- list(c(63, 25, 165.34, 24.2))
  } else{
    t <- "decide"
    n <- T
    l <- NULL
  }
     listo[[k]][[i]] <- el(extract_tables(eval(parse(text = paste0("y", k, "[", i, "]"))), 
                                              method = t, guess = n, area = l))
     xlcFreeMemory() # clear Java machine mem. (java specific bottleneck)
}
}
# Even after setting the area manually the table cannot be detected i suspect that they inserted
# somesort of picturesque file which cannot be detected...
############
# cleaning the obtained datafranes, unfortunately all seperate as they all differ 
# even within the same design...
listo[[1]][[1]] <- listo[[1]][[1]][4:23, c(1, 2, 4)]
# split by " " and remove the value to the right as this corresponds to an individual column
listo[[1]][[1]][,2] <- sapply(strsplit(listo[[1]][[1]][, 1], " ", fixed = T), function(x){
  x[length(x)]
})
# rownames 
rownames <- sapply(strsplit(listo[[1]][[1]][, 1], " ", fixed = T), function(x){
  paste(x[-c(length(x))], collapse = " ")
})
# switch column to rownames
listo[[1]][[1]] <- listo[[1]][[1]][, -1] 
#
colnames(listo[[1]][[1]]) <- c("Steuerpflichtige insg.", "Unselbstständige Erwerbstätige")
listo[[1]][[1]] <- gsub("\\.|\\,", "", listo[[1]][[1]])
#
listo[[1]][[1]] <- apply(listo[[1]][[1]], 2, as.numeric)
row.names(listo[[1]][[1]]) <- rownames
View(listo[[1]][[1]]) # first one done 
######################################
listo[[1]][[2]] <- listo[[1]][[2]][4:23, c(1, 4)]
#
listo[[2]][[1]] <- listo[[2]][[1]][10:29, c(1, 3, 4)]
listo[[2]][[2]] <- listo[[2]][[2]][9:28, c(1, 3, 4)]
listo[[2]][[3]] <- listo[[2]][[3]][11:30, c(1, 3, 4)]
listo[[2]][[4]] <- listo[[2]][[4]][11:30, c(1, 3, 4)]

View(listo[[1]][[2]])
listo[[1]][[1]][4:23, c("V1", "V2", "V4")]
?extract_tables()


## WORK IN PROGRES