#!/usr/local/bioinfo/bin/Rscript --vanilla --slave --no-site-file

# R Script producing a hierarchical clustering
# Input : a file containing a table with numeric values
#         except for the first column containing sample names
#         and the first line containing variable names
#         separator expected is <TAB>
#
# Clustering method :
#         euclidean, correlation, ...
#
# Ouptut : a file containing the image of the clustering
#-----------------------------------------------------------------
# Authors : sophie.lamarre(at)insa-toulouse.fr
#           ignacio.gonzalez(at)toulouse.inra.fr
#           luc.jouneau(at)inra.fr
#           valentin.marcon(at)inra.fr
# Version : 0.9
# Date    : 06/09/2017

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

#Import the different functions used for the hierarchical clustering
source_local("h_clust.R")

##------------------------------
## Lecture parametres
##------------------------------
argLs <- parseCommandArgs(evaluate=FALSE)

group_member_file=argLs[["group_member_file"]]
if (group_member_file=="NO"){
    group_member_file<-NULL
} 

h_clust(input_file=argLs[["input_file"]],
     group_member_file=group_member_file,
     output_file=argLs[["output_file"]],
     log_file=argLs[["log_file"]],
     format_image_out="jpeg",
     distance_method=argLs[["distance_method"]],
     agglomeration_method=argLs[["agglomeration_method"]],
     column_clustering=argLs[["column_clustering"]],
     select=argLs[["select"]],
     plot_title=argLs[["plot_title"]],
     xlab=argLs[["xlab"]],
     ylab=argLs[["ylab"]],
     width=argLs[["width"]],
     height=argLs[["height"]],
     ppi=argLs[["ppi"]],
     na_encoding=argLs[["NA_code"]])
