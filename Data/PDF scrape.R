# PDF scraping
# rm(list = ls())
getwd()
# setwd("~/GitHub/Pareto-Extrapolation-/Data")
library(tabulizer)
library(stringr)
library(XLConnect)
library(miniUI)
library(shiny)
##############################################################################################################
# load all pdf's
# Unfortunately in 2016 and 2012 statistics austria updated their design and hence we cannot treat the tables equally
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
    t <- "stream" # used for table that have now line seperations
    n <- F
    # locate_areas("2016 Table.pdf", 1), similar for every pdf post 2016
    l <- list(c(180.53, 71.96, 372.13, 525.15097)) # tabulizer couldn't auto detect
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
# Even after setting the area manually the table cannot be detected I suspect that they inserted
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
#colnames
col <- c("Steuerpflichtige insg.", "Unselbstständige Erwerbstätige")
# switch column to rownames
listo[[1]][[1]] <- listo[[1]][[1]][, -1] 
#  little function that shortens code since this will be repeated for every df
nameit.num <- function(x){
  x <- gsub("\\.|\\,", "", x)
  x <- apply(x, 2, as.numeric)
  x <- as.data.frame(x)
  rownames(x) <- rownames
  colnames(x) <- col
  x
}
#
# first one done 
#list for finished df
listf <- list()
listf[[1]] <- nameit.num(listo[[1]][[1]]) 
######################################
listo[[1]][[2]] <- listo[[1]][[2]][4:23, c(1, 4)]
#
listo[[1]][[2]][, 1] <- sapply(strsplit(listo[[1]][[2]][, 1], " ", fixed = T), function(x){
  x[length(x)]
})
# second one done
listf[[2]] <- nameit.num(listo[[1]][[2]])
######################################
listo[[2]][[1]] <- listo[[2]][[1]][10:29, c(1, 3, 4)]
#
listo[[2]][[1]] <- listo[[2]][[1]][, -1]
# third one done
listf[[3]] <- nameit.num(listo[[2]][[1]])
######################################
listo[[2]][[2]] <- listo[[2]][[2]][9:28, c(1, 3, 4)]
#
listo[[2]][[2]] <- listo[[2]][[2]][, -1]
# 4th one done 
listf[[4]] <- nameit.num(listo[[2]][[2]])
######################################
listo[[2]][[3]] <- listo[[2]][[3]][11:30, c(1, 3, 4)]
#
listo[[2]][[3]] <- listo[[2]][[3]][, -1]
# 5th one done
listf[[5]] <- nameit.num(listo[[2]][[3]])
######################################
listo[[2]][[4]] <- listo[[2]][[4]][11:30, c(1, 3, 4)]
#
listo[[2]][[4]] <- listo[[2]][[4]][, -1]
# 6th one done
listf[[6]] <- nameit.num(listo[[2]][[4]])
######################################
# luckily scrape result of last design yielded similar enough results to loop the cleaning ops.
for(i in 1:3){
  listf[[i + 6]] <- nameit.num(listo[[3]][[i]][1:20, c(4, 5)])
}
# name the list
names(listf) <- paste(2010:2018)
## WORK IN PROGRESS
listf

