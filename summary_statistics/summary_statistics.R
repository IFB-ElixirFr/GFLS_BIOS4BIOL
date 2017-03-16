###########################################################################
#            Quality controls and descriptive analysis plots              #
###########################################################################
# Authors : Melanie Petera                                                #
###########################################################################
# Description : This script allows various displays of data for quality   #
#      control and descriptive analysis. The input data is a matrix of    #
#      quantitative variables, and it returns chosen plots in png format  #
#      and a table with chosen statistics.                                #
###########################################################################
# Specific R packages :                                                   #
#      - edgeR (needed for MA plots)                                      #
###########################################################################
# Version 1 (06-06-2014) : display boxplot, histogram, density plot,      #
#      MA plot, pairs plot, and return a table of chosen statistics       #
#      (quantiles, mean, variance, standard error of the mean)            #
###########################################################################
        
desc_fct <- function(file.in, nacode, stat.out, stat, chosen.stat, ploting, chosen.plot){
  # Parameters :
  # - file.in : count matrix input (tab-separated) [file name]
  # - nacode : missing value coding character
  # - stat.out : results file of chosen statistics [file name]
  # - stat : should statistics be calculated ? (TRUE/FALSE)
  # - chosen.stat : character listing the chosen statistics (comma-separated)
  # - ploting : should graphics be displayed ? (TRUE/FALSE)
  # - chosen.plot : character listing the chosen plots (comma-separated)

# Data import - - - - - - - - - - - - - - - - - 

Dataset <- read.table(file.in,header=TRUE,na.strings=nacode,sep="\t")


# Statistics table computation - - - - - - - - -

if(stat & length(chosen.stat)!=0){
  
  stat.list <- strsplit(chosen.stat,",")[[1]]
  
  stat.res <- t(Dataset[0,,drop=FALSE])
  
  
  if("mean" %in% stat.list){  
    stat.res <- cbind(stat.res,c("Mean",colMeans(Dataset[,-1],na.rm=TRUE)))  
  }
  
  if("sd" %in% stat.list){
    colSd <- apply(Dataset[,-1],2,sd,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Std.Dev",colSd))
  } 
  
  if("variance" %in% stat.list){
    colVar <- apply(Dataset[,-1],2,var,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Variance",colVar))
  }
  
  if(("median" %in% stat.list)&&(!("quartile" %in% stat.list))){
    colMed <- apply(Dataset[,-1],2,median,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Median",colMed))
  }
  
  if("quartile" %in% stat.list){
    colQ <- apply(Dataset[,-1],2,quantile,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Min",colQ[1,]),c("Q1",colQ[2,]),
                      c("Median",colQ[3,]),c("Q3",colQ[4,]),c("Max",colQ[5,]))
  }
  
  if("decile" %in% stat.list){
    colD <- t(apply(Dataset[,-1],2,quantile,na.rm=TRUE,seq(0,1,0.1)))
    colD <- rbind(paste("D",seq(0,10,1),sep=""),colD)
    stat.res <- cbind(stat.res,colD)
  }
  
  write.table(stat.res,stat.out,col.names=FALSE,sep="\t",quote=FALSE)
  
} # end if(stat)



# Graphics generation - - - - - - - - - - - - - 

if(ploting & length(chosen.plot)!=0){
  
  
  graph.list <- strsplit(chosen.plot,",")[[1]]
  
  if("boxplot" %in% graph.list){
    for(ech in 2:ncol(Dataset)){
      png(paste("boxplot_",colnames(Dataset)[ech],".png",sep=""))
      boxplot(Dataset[,ech],xlab=colnames(Dataset)[ech])
      dev.off()
    }
  }
  
  if("histogram" %in% graph.list){
    for(ech in 2:ncol(Dataset)){
      png(paste("histogram_",colnames(Dataset)[ech],".png",sep=""))
      hist(Dataset[,ech],main=colnames(Dataset)[ech],xlab=NULL)
      dev.off()
    }
  }
  
  if("density" %in% graph.list){
    for(ech in 2:ncol(Dataset)){
      png(paste("density_",colnames(Dataset)[ech],".png",sep=""))
      plot(density(Dataset[,ech],na.rm=TRUE),main=colnames(Dataset)[ech])
      dev.off()
    }
  }
  
  if("pairsplot" %in% graph.list){
    png("pairsplot.png",width=(ncol(Dataset)-1)*200,height=(ncol(Dataset)-1)*200,res=100)
    pairs(Dataset[,-1])
    dev.off()
  }
  
  if("MAplot" %in% graph.list){
    if(min(Dataset[,-1],na.rm=TRUE)<0){stop("\n----\nError : MAplot only avaible for positive variables\n----")}
    library(edgeR)
    for(ech in 2:(ncol(Dataset)-1)){
      for(ech2 in (ech+1):ncol(Dataset)){
        png(paste("MAplot_",colnames(Dataset)[ech],"_",colnames(Dataset)[ech2],".png",sep=""))
        temp.pair <- na.omit(Dataset[,c(ech,ech2)])
        maPlot(temp.pair[,1],temp.pair[,2],main=paste(colnames(Dataset)[ech],"VS",colnames(Dataset)[ech2]))
        dev.off()
      }
    }
    
  }
  
} # end if(ploting)



} # end of function

