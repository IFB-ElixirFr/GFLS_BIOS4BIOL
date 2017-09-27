###########################################################################
#            Quality controls and descriptive analysis plots              #
###########################################################################
# Authors: Melanie Petera                                                 #
###########################################################################
# Description : This script allows various displays of data for quality   #
#      control and descriptive analysis. The input data is a matrix of    #
#      quantitative variables, and it returns chosen plots in png format  #
#      and a table with chosen statistics.                                #
###########################################################################
# Specific R packages:                                                    #
#      - edgeR (needed for MA plots)                                      #
###########################################################################
# Version 1 (06-06-2014): display boxplot, histogram, density plot,       #
#      MA plot, pairs plot, and return a table of chosen statistics       #
#      (quantiles, mean, variance, standard error of the mean)            #
###########################################################################


##------------------------------
## Libraries laoding
##------------------------------
# For parseCommandArgs function
library(batch) 

# R script call
source_local <- function(fname)
{
	argv <- commandArgs(trailingOnly = FALSE)
	base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
	source(paste(base_dir, fname, sep="/"))
}

#Import the different functions used for Summary_Statistics
source_local("summary_statistics.R")

##------------------------------
## Lecture parametres
##------------------------------
argLs <- parseCommandArgs(evaluate=FALSE)

desc_fct(file.in=argLs[["file_in"]],
nacode=argLs[["NA_code"]],
table_file=argLs[["table_file"]],
graph_file=argLs[["graph_file"]],
stat=argLs[["stat"]],
chosen.stat=argLs[["stat_chosen"]],
ploting=argLs[["ploting"]],
chosen.plot=argLs[["plot_chosen"]],
log_file=argLs[["log_file"]])


