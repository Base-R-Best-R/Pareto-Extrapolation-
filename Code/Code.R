# Income inequality in austria, based on data from statistics austria between 2010 and 2018
# firstly create ecdf 
getwd()
# setwd("~/GitHub/Pareto-Extrapolation-/Data")
# CleanedTablesList.rds
lst <- readRDS(file = "CleanedTablesList.rds")# available in the Data folder
#
# install.packages("spatstat")
library(spatstat)
library(stringr)
#
Lim <- as.numeric(str_extract(rownames(lst[[1]]), "^[0-9]+"))
# append p = fraction, f = cummulative sum of p (required for the cdf), lim = limits for the ecdf step function
# f amount of people bellow this threshold
# p people (relative) within the bracket, note that here we assume a uniform distribution within each bracket
lst <- lapply(lst, function(x){
  p <- x[,1] / x[20, 1]
  f <- cumsum(p)
  cbind(x, Lim, p, f, 1 - f)
})
# weighetd ecdf list of weighted ecdfs: 
lwecdf <- list()
lwecdf <- lapply(lst, function(x){
  ewcdf(x$Lim, x$p)
})
## plot every wecdf in same plot different colours
col <- c("2010" = "blue", "2011" = "brown", "2012" = "green", "2013" = "darkred", "2014" = "deeppink", "2015" = "darkorange",
         "2016" = "blueviolet", "2017" = "burlywood", "2018" = "aquamarine")
#
col1 <- unname(col)
# time to plot
for( i in seq.int(length(col))){
  if(i == 1){
    plot(lwecdf[[i]], col = col[i], main = "Empirical Weighted Cummulative Distribution Function", 
         xlab = "Income in 1000 ???")
  }
  else{
    plot(lwecdf[[i]], col = col[i], add = TRUE)
  }
}
# 
legend(130, 0.9, legend = c(paste(2010:2018), expression(paste("Exp: ", lambda, "= 0.04"))), fill = c(col1, "black"),
       box.lty = 0)
# add exponentialfunction
expF <- function(x) 1 - exp(-0.05*x)
# draw curve onto plot 
curve(expF, 0, 200, add = T, type = "l", pch = 19, lty = 2)
##################################################################################################################
# pareto tail 
# to get the calc first only for 2010
get.alpha <- function(x){
  # people within group of interest
  n1 <- x[x[,3] == 150 & !is.na(x[,3]), 1]
  n2 <- x[x[,3] == 200 & !is.na(x[,3]), 1]
  # sum
  n <- n1 + n2
  # ML estimate obtained from the first order condition of the log likelyhood function
  log(n2/n) / log(150000/200000)
}
# alphas for every year
alpha_hat <- sapply(lst, get.paretotail)
##################################################################################################################
# income shares for top 10%, 5% and 1%
quanta <- list("10%" = list(),
               "5%" = list(),
               "1%" = list())
# find value closest to given income group
c.value.within <- function(x, y, Index = F, col = 6){
  if(Index){
    which(abs(x[, col] - y) == min(abs(x[, col] - y)))
  } 
  else{
  # where x is the vector and y is the number one wants to find the closest value to 
  x[, col][which(abs(x[, col] - y) == min(abs(x[, col] - y)))]
  }
}
#
for(i in c(0.1, 0.05, 0.01)){
k <- lapply(lst, c.value.within, y = i, Index = T)
k
}
# WORK IN PROGRESS










