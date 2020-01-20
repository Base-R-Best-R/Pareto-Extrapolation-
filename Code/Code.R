# Income inequality in austria, based on data from statistics austria between 2010 and 2018
# firstly create ecdf 
getwd()
# setwd("~/GitHub/Pareto-Extrapolation-/Data")
# CleanedTablesList.rds
lst <- readRDS(file = "CleanedTablesList.rds")# available in the Data folder
#
install.packages("spatstat")
library(spatstat)
library(stringr)
#
Lim <- as.numeric(str_extract(x, "\\ [^\\ ]*$"))

# append p = fraction, f = cummulative sum of p (required for the cdf), lim = limits for the ecdf step function
# f amount of peeple bellow this threshold
# p people (relative) within the bracket, note that here we assume a uniform distribution within each bracket
lst <- lapply(lst, function(x){
  p <- x[,1] / x[20, 1]
  f <- cumsum(p)
  cbind(x, Lim, p, f)
})
# weighetd ecdf( list of weighted ecdfs: 
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
legend(165, 0.9, legend = c(paste(2010:2018), expression(paste("Exp: ", lambda, "= 0.04"))), fill = c(col1, "black"),
       box.lty = 0)
# add exponentialfunction
expF <- function(x) 1 - exp(-0.04*x)
# draw curve onto plot 
curve(expF, 0, 200, add = T, type = "l", pch = 19, lty = 2)
####################################################################################################




