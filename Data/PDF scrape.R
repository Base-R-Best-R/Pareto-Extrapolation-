#PDF scraping
#setwd("~/GitHub/Pareto-Extrapolation-/Data")
#install.packages("pdftools")
library(pdftools)
library(stringr)
##############################################################################################################
# load all pdf's
txt_tables <- NULL
y <- paste(2010:2018, "Table.pdf")
#
for(i in seq.int(length(y))){
  txt_tables[i] <- pdf_text(y[i])
}
# First clean one table and then automate:
cat(txt_tables[1])# first look - need to split twice and probably manuall reassign column names
test <- txt_tables[1]
str_count(test, pattern = "Männer\r\n")# split
# fixed = TRUE, no regex required yet
test <- unlist(strsplit(test, "Männer\r\n", fixed = T))[1] 
# worked just fine, now split off the top, cant use strsplit here becasue there is more then one match for insgesamt
str_count(test, "Insgesamt") # 2 -->
test <- unlist(regmatches(test, regexpr("Insgesamt", test), invert = TRUE))[2]
cat(test) # succesfully isolated 
#
k <- unlist(strsplit(test, "\r\n", fixed = T))
###
