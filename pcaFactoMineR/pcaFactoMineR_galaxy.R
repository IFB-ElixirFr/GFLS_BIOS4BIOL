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

log_file <- argLs[["logOut"]]
  # Inputs
	# Matrice donnees
data <- read.table(argLs[["datafile"]],header=TRUE,sep="\t",dec=".",check.names = FALSE)
rownames(data) <- data[,1]

	# Facteur biologique
hb=0
if(argLs[["factor"]] != "None")
{
  metadatasample <- read.table(argLs[["samplemetadata"]],header=TRUE,sep="\t",dec=".",check.names = FALSE)
  rownames(metadatasample) <- metadatasample[,1]
# Test si le  facteur choisi est bien dans le samplemetadata
  if (any(argLs[["factor"]] %in% colnames(metadatasample)) ==FALSE)
  {
    #log_error(simpleCondition("Factor is not in samplemetadata."))
    cat("<HTML><HEAD><TITLE>PCA FactoMineR report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
    cat("&#9888 An error occurred while trying to read your factor table.\n<BR>",file=log_file,append=T,sep="")
    cat("Please check that:\n<BR>",file=log_file,append=T,sep="")
    cat("<UL>\n",file=log_file,append=T,sep="")
    cat("  <LI> you wrote the name of the column of the factor matrix corresponding to the qualitative variable </LI>\n",file=log_file,append=T,sep="")
    cat("  <LI> you wrote the column name correctly (it is case sensitive)</LI>\n",file=log_file,append=T,sep="")
    cat("</UL>\n",file=log_file,append=T,sep="")
    cat("</BODY></HTML>\n",file=log_file,append=T,sep="")
    q(save="no",status=1)
  }
# On cree une dataframe avec l’id des samples (1ere colonne de metadatasample+ le facteur choisi 
# qui est en colonne “colfactor”
  colfactor <- which(argLs[["factor"]]  == colnames(metadatasample))
  facteur <- data.frame(metadatasample[,1], metadatasample[[colfactor]])
  facteur[[2]] <- as.factor(facteur[[2]])
  hb=1
}

  # Appel de la fonction
eigenplot=0
contribplot=0
scoreplot=0
loadingplot=0
variable_in_line=0

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

if (argLs[["varinline"]]=="yes")
{
  variable_in_line=1 
}





  # Outputs
nomGraphe <- argLs[["outgraphpdf"]]
res.pca <- pca.main(ids=data,bioFact=facteur,ncp=argLs[["npc"]],hb=hb,
                    minContribution=c(argLs[["contribh"]],argLs[["contribv"]]),mainTitle=argLs[["title"]],
                    textSize=argLs[["tc"]],principalPlane=c(argLs[["pch"]],argLs[["pcv"]]),eigenplot=eigenplot,
                    contribplot=contribplot,scoreplot=scoreplot,loadingplot=loadingplot,
                    nomGraphe=argLs[["outgraphpdf"]],variable_in_line=variable_in_line,log_file=log_file)


################################# fin  ##############################################
