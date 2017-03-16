#!/usr/local/bioinfo/bin/Rscript --vanilla --slave --no-site-file

################################################################################################
# pcaFactoMineR_galaxy                                                                         #
#                                                                                              #
# Author : Sandrine Laguerre / Marie Tremblay-Franco / Jean-Francois Martin                    #
# User : Galaxy                                                                                #
# Original data : --                                                                           #
# Starting date : 24-03-2015                                                                   #
# Version 1 : 20-05-2015                                                                       #
# Version 2 : 24-02-2016								       #
#                                                                                              #
#                                                                                              #
# Input files : dataMatrix.txt                                                                 #
# Output files : graph_output.pdf ;                                                            #
#                                                                                              #
################################################################################################

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
library(pcaMethods)
library(FactoMineR)

# R script call
source_local <- function(fname)
{
	argv <- commandArgs(trailingOnly = FALSE)
	base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
	source(paste(base_dir, fname, sep="/"))
}

#Import the different functions used for PCA
#sarah maman smaman ajout path 
#source_local("/usr/local/bioinfo/src/galaxy-test/galaxy-dist/tools/R_ACP_CATIBIOSDBIOL/pca_FactoMiner_galaxy_functions.R")
 source_local("pcaFactoMineR_functions.R")

##------------------------------
## Constants
##------------------------------
topEnvC <- environment()
flagC <- "\n"


##------------------------------
## Lecture parametres
##------------------------------
argLs <- parseCommandArgs(evaluate=FALSE)

log <- argLs[["logOut"]]

  # Inputs
	# Matrice donnees
data <- read.table(argLs[["datafile"]],header=TRUE,sep="\t",dec=".",check.names = FALSE)
rownames(data) <- data[,1]
print(rownames(data))
data <- data[,-1]
print(data)

	# Facteur biologique
hb=0
if(argLs[["factor"]] != "None")
{
  facteur <- data[,1]
  if(mode(facteur) != 'character')
  {
    stop("\n First column must be a factor (variable qualitative)\n")
  }
  facteur <- as.factor(facteur)
  data[,1] <- as.factor(data[,1])
  hb=1
}

  # Appel de la fonction
eigenplot=0
contribplot=0
scoreplot=0
loadingplot=0

if (argLs[["plotev"]]=="yes")
{
  eigenplot=1
}

if (argLs[["plotcontrib"]]=="yes")
{
  contribplot=1
}


if (argLs[["plotindiv"]]=="yes")
{
  scoreplot=1
}


if (argLs[["plotvar"]]=="yes")
{
  loadingplot=1
}

  # Outputs
nomGraphe <- argLs[["outgraphpdf"]]
res.pca <- pca.main(ids=data,bioFact=facteur,ncp=argLs[["npc"]],hb=hb,scalingMethod=argLs[["scaleoption"]],
                    minContribution=c(argLs[["contribh"]],argLs[["contribv"]]),mainTitle=argLs[["title"]],
                    textSize=argLs[["tc"]],principalPlane=c(argLs[["pch"]],argLs[["pcv"]]),eigenplot=eigenplot,
                    contribplot=contribplot,scoreplot=scoreplot,loadingplot=loadingplot,
                    nomGraphe=argLs[["outgraphpdf"]])


################################# fin  ##############################################
