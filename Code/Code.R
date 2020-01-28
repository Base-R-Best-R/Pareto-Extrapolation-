# Income inequality in austria, based on data from statistics austria between 2010 and 2018
# rm(list = ls())
getwd()
 setwd("~/GitHub/Pareto-Extrapolation-/Data")
# CleanedTablesList.rds
# install.packages("spatstat")
library(spatstat)
library(stringr)
#
lst <- readRDS(file = "CleanedTablesList.rds")# available in the Data folder
ext <- readRDS(file = "IncGroup.rds")
ext <- lapply(ext, function(x) x * 1000)
Lim <- as.numeric(str_extract(rownames(lst[[1]]), "^[0-9]+"))
#
# merge
for(i in 1:9){
  lst[[i]] <- cbind(lst[[i]], ext[[i]])
  names(lst[[i]])[3] <- "Bruttolöhne Insg." 
}
# check for correct sum of income 
s <- sapply(lst, function(x){
sum(x[1:19, 3]) == x[20, 3]
})
sum(s) # even though the differences are marginal in order to have bracket income shares add up to one 
       # I will recalc those sums
for(i in 1:9){
  lst[[i]][20, 3] <- sum(lst[[i]][1:19, 3])
}
# double check
s2 <- sapply(lst, function(x){
  sum(x[1:19, 3]) == x[20, 3]
})
sum(s2) == 9 # TRUE
# append pp = fraction, fp = cummulative sum of pp (required for the cdf), lim = limits for the ecdf step function
# fp amount of people bellow this threshold
# pp people (relative) within the bracket
#
# Weighted mean above bracket:
wm.above <- function(x){
  a <- NULL
  for(j in 1:18){
   i <- which(x[, "Lim"] == Lim[j]) + 1 # convert lim to index
   a[j] <- weighted.mean(x[i:19, 4], x[i:19, 1]) # weighted by ppl within bracket 
  }
  c(a, NA, NA)
}
#
lst <- lapply(lst, function(x){
  pp <- x[,1] / x[20, 1] # percentage of people within bracket  
  fp <- cumsum(pp)        
  AveInc <- x[, 3] / x[, 1] # average income within bracket
  ShareInc <- x[, 3] / x[20, 3] # income share of bracket
  ShareIncf <- cumsum(ShareInc)
  cbind(x, AveInc, ShareInc, ShareIncf, 1 - ShareIncf, Lim, pp, fp, 1 - fp)
})

# forgot ave income above
lst <- lapply(lst, function(x){
  AveIncAbove <- wm.above(x)
  cbind(x, AveIncAbove)
})
lst[[1]]
# NA recode
for(i in 1:9){
  lst[[i]][20, which(lst[[i]][20,] %in% c(-1, 2))] <- NA
}
View(lst[[1]])
#########################################################################################################################
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
legend(130, 0.9, legend = c(paste(2010:2018), expression(paste("Exp: ", lambda, "= 0.05"))), fill = c(col1, "black"),
       box.lty = 0)
# add exponentialfunction
expF <- function(x) 1 - exp(-0.05*x)
# draw curve onto plot 
curve(expF, 0, 200, add = T, type = "l", pch = 19, lty = 2)
##################################################################################################################
# alphas for every bracket besides the top code 
# note that the exponential distribution asssumption is not valid for lower income brackets
# I will still calculate alphas for every bracket, otherwise I would have t set a somewhat arbitrary pount at which
# I believe that the distribution assumption holds, at the end of the day this work will focus on the top of the 
# distribution where the pareto assumption is likely to hold
Lim1 <- c(Lim[-1], NA)*1000
#
get_alpha_hat <- function(x){
  x[, "AveIncAbove"] / (x[, "AveIncAbove"] - Lim1)
}
#
for(i in 1:9){
  lst[[i]][, "alpha_hat"] <- get_alpha_hat(lst[[i]])
}
##################################################################################################################
# pareto tail 
get.tail <- function(x){
  # people within group of interest
  n1 <- x[x[, "Lim"] == 150 & !is.na(x[, "Lim"]), 1]
  n2 <- x[x[, "Lim"] == 200 & !is.na(x[, "Lim"]), 1]
  # sum
  n <- n1 + n2
  # ML estimate obtained from the first order condition of the log likelyhood function
  log(n2/n) / log(150000/200000)
}
for(i in 1:9){
  lst[[i]][19, "alpha_hat"] <- get.tail(lst[[i]])
}
### export final df list
# setwd("C:/Users/blasc/OneDrive/Documents/GitHub/Pareto-Extrapolation-/Data")
# saveRDS(lst, "Merged_Calc_lst.rds")
####################################################################################################################
# income shares for top 10%, 5% and 1%
quanta <- list("10%" = c(),
               "5%" = c(),
               "1%" = c())
#na.rm
na.rm <- function(x){
  x[!is.na(x)]
}
# find value closest to given income group
c.value.within <- function(x, y, Index = F){
  if(Index){
    which(abs(na.rm(x[, "1 - fp"]) - y) == min(abs(na.rm(x[, "1 - fp"]) - y)))
  } 
  else{
  # where x is the vector and y is the number one wants to find the closest value to 
  x[, "1 - fp"][which(abs(na.rm(x[, "1 - fp"]) - y) == min(abs(na.rm(x[, "1 - fp"]) - y)))]
  }
}
# Atkinson
top_income_share <- function(x, p){
  c <- c.value.within(x, p, Index = T)
  b <- c.value.within(x, p)
  a <- x[c, "alpha_hat"]# find the correct alpha
  ((p / b)^((a - 1) / a)) * x[c, "1 - ShareIncf"]
  #((p^((a - 1) / a)) / b) * x[c, "1 - ShareIncf"]
}
# calc all top income shares across years and groups
sh <- c(0.1, 0.05, 0.01)

for(i in seq.int(length(sh))){
quanta[[i]] <- sapply(lst, top_income_share, p = sh[i])
}

# Plot Income Shares 
ny <- as.numeric(names(quanta[[1]]))
plot(ny, quanta[[1]], type = "o", pch = 1, lty = 0, ylim = c(0,0.45), ylab = "Income Share",
     xlab = "Years", main = "Income Shares", col = "darkgreen")
points(ny, quanta[[2]], type ="o", pch = 2, lty = 0, col = "blue")
points(ny, quanta[[3]], type = "o", pch = 3, lty = 0, col = "red")
legend("topleft", legend = c("Top 10%", "Top 5%", "Top 1%"), fill = c("darkgreen", "blue", "red"))
# Plot Income Shares relative to basis year 2010

quantar <- lapply(quanta, function(x){
  x / x[1] * 100
})
quantar
### WORK IN PROGRESS
plot(ny, quantar[[1]], type = "o", pch = 1, lty = 0, ylim = c(90,110), ylab = "Income Share",
     xlab = "Years", main = "Income Shares relative to 2010", col = "darkgreen")
points(ny, quantar[[2]], type ="o", pch = 2, lty = 0, col = "blue")
points(ny, quantar[[3]], type = "o", pch = 3, lty = 0, col = "red")
legend("topleft", legend = c("Top 10%", "Top 5%", "Top 1%"), fill = c("darkgreen", "blue", "red"))
abline(h = 100)


?sin
