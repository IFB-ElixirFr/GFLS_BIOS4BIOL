#!/usr/local/bioinfo/bin/Rscript --vanilla --slave --no-site-file

# R Script making the bridge between Galaxy and the call of the normalization method 
#-----------------------------------------------------------------
# Authors : luc.jouneau(at)inra.fr
#           valentin.marcon(at)inra.fr
# Version : 0.9
# Date    : 30/08/2017
#---------------------------------------------------------------

##------------------------------
## Options
##------------------------------
strAsFacL <- options()$stringsAsFactors
options(stringsAsFactors = FALSE)

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

#Import the different functions used for PCA
source_local("normalization.R")

##------------------------------
## Lecture parametres
##------------------------------
argLs <- parseCommandArgs(evaluate=FALSE)

normalization(input_file=argLs[["input_file"]],
    transformation_method=argLs[["transformation_method"]],
    na_encoding=argLs[["na_encoding"]],
    output_file=argLs[["output_file"]],
    log_file=argLs[["log_file"]],
    variable_in_line=argLs[["variable_in_line"]])

