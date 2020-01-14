#PDF scraping
setwd("~/VWL/Hauptsemester 4/Economics of Distribution/Pareto Extraploation/PDF/Subset Pages Holding Tables")
#
install.packages("pdftools")
library(dplyr)
library(pdftools)
##############################################################################################################
# load all pdf's
txt_tables <- list()
y <- paste(2010:2018, "Table.pdf")
for(i in seq.int(length(y))){
  txt_tables[[i]] <- pdf_text(y[[i]])
}
txt_tables[[1]]
gsub(".*Insgesamt .","",txt_tables[[1]])
strsplit(txt_tables[[1]], "insgesamt\r\n")
sub("^.*\\insgesamt\r\n","", t1)
t1 <- gsub(" ", "", txt_tables[[1]], fixed = TRUE)
strsplit(t1,"insgesamt\r\n")
?grepl
grep("0b", t1)
l <- strsplit(t1, "insgesamt")
cat(l[[1]][2])

l
gsub(".*insgesamt\r\n","" ,t1)
