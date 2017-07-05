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
        
desc_fct <- function(file.in, nacode, stat.out, stat, chosen.stat, ploting, chosen.plot, log_file){
  # Parameters :
  # - file.in : count matrix input (tab-separated) [file name]
  # - nacode : missing value coding character
  # - stat.out : results file of chosen statistics [file name]
  # - stat : should statistics be calculated ? (TRUE/FALSE)
  # - chosen.stat : character listing the chosen statistics (comma-separated)
  # - ploting : should graphics be displayed ? (TRUE/FALSE)
  # - chosen.plot : character listing the chosen plots (comma-separated)
  # - log_file : a log file [file name]


##########################################################
# Read and verify data - - - - - - - - - - - - 
# Checks valids for all modules

	line_use="line"
	column_use="column"

log_error=function(message="") {
		cat("<HTML><HEAD><TITLE>Normalization report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
		cat("&#9888 An error occurred while trying to read your table.\n<BR>",file=log_file,append=T,sep="")
		cat("Please check that:\n<BR>",file=log_file,append=T,sep="")
		cat("<UL>\n",file=log_file,append=T,sep="")
		cat("  <LI> the table you want to process contains the same number of columns for each line</LI>\n",file=log_file,append=T,sep="")
		cat("  <LI> the first line of your table is a header line (specifying the name of each ",column_use,")</LI>\n",file=log_file,append=T,sep="")
		cat("  <LI> the first column of your table specifies the name of each ",line_use,"</LI>\n",file=log_file,append=T,sep="")
		cat("  <LI> both individual and variable names should be unique</LI>\n",file=log_file,append=T,sep="")
		cat("  <LI> each value is separated from the other by a <B>TAB</B> character</LI>\n",file=log_file,append=T,sep="")
		cat("  <LI> except for first line and first column, table should contain a numeric value</LI>\n",file=log_file,append=T,sep="")
		cat("  <LI> this value may contain character '.' as decimal separator or '",nacode,"' for missing values</LI>\n",file=log_file,append=T,sep="")
		cat("</UL>\n",file=log_file,append=T,sep="")
		cat("-------<BR>\nError messages recieved :<BR><FONT color=red>\n",conditionMessage(message),"</FONT>\n",file=log_file,append=T,sep="")
		cat("</BODY></HTML>\n",file=log_file,append=T,sep="")
		q(save="no",status=1)
}

tab_in=tryCatch(
	{
		tab_in=read.table(file.in,header=TRUE,na.strings=nacode,sep="\t",check.names=FALSE,quote="\"")
	},
	error=function(cond) {
		log_error(message=cond)
		return(NA)
	},
	warning=function(cond) {
		log_error(message=cond)
		return(NA)
	},
	finally={
		#Do nothing special
	}
)

if (ncol(tab_in)<2) {
	log_error(simpleCondition("The table you want to use contains less than two columns."))
}

rn=as.character(tab_in[,1])
if (length(rn)!=length(unique(rn))) {
	duplicated_rownames=table(rn)
	duplicated_rownames=duplicated_rownames[duplicated_rownames>1]
	duplicated_rownames=names(duplicated_rownames)
	if (length(duplicated_rownames)>3) {
		duplicated_rownames=c(duplicated_rownames[1:3],"...")
	}
	duplicated_rownames=paste(duplicated_rownames,collapse=", ")
	log_error(simpleCondition(
		paste("The table you want to use have duplicated values in the first column (",
			" - duplicated names : ",duplicated_rownames,sep="")
	))
}
tab=tab_in[,-1]
rownames(tab)=rn

#Check all columns are numerical
tab=as.matrix(tab)
cell.with.na=c()
for (i in 1:ncol(tab)) {
	na.v1=is.na(tab[,i])
	na.v2=is.na(as.numeric(tab[,i]))
	if (sum(na.v1)!=sum(na.v2)) {
		sel=which(na.v1!=na.v2)
		sel=sel[1]
		value=tab[sel,i]
		log_error(simpleCondition(
			paste("Column '",colnames(tab)[i],"' of your table contains non numerical values. Please check its content (on line #",sel," : value='",value,"').",sep="")
		))
	}
	if (length(cell.with.na)==0 & sum(na.v1)!=0) {
		cell.with.na=c(i,which(na.v1)[1])
	}
}

Dataset <- tab_in

##########################################################
# Statistics table computation - - - - - - - - -

if(stat & length(chosen.stat)!=0){
  
  stat.list <- strsplit(chosen.stat,",")[[1]]
  
  stat.res <- t(Dataset[0,,drop=FALSE])
  
  numdig <- 5
  
  if("mean" %in% stat.list){  
    stat.res <- cbind(stat.res,c("Mean",round(colMeans(Dataset[,-1],na.rm=TRUE),digits=numdig)))
  }
  
  if("sd" %in% stat.list){
    colSd <- apply(Dataset[,-1],2,sd,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Std.Dev",round(colSd,digits=numdig)))
  } 
  
  if("variance" %in% stat.list){
    colVar <- apply(Dataset[,-1],2,var,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Variance",round(colVar,digits=numdig)))
  }
  
  if(("median" %in% stat.list)&&(!("quartile" %in% stat.list))){
    colMed <- apply(Dataset[,-1],2,median,na.rm=TRUE)
    stat.res <- cbind(stat.res,c("Median",round(colMed,digits=numdig)))
  }
  
  if("quartile" %in% stat.list){
    colQ <- round(apply(Dataset[,-1],2,quantile,na.rm=TRUE),digits=numdig)
    stat.res <- cbind(stat.res,c("Min",colQ[1,]),c("Q1",colQ[2,]),
                      c("Median",colQ[3,]),c("Q3",colQ[4,]),c("Max",colQ[5,]))
  }
  
  if("decile" %in% stat.list){
    colD <- round(t(apply(Dataset[,-1],2,quantile,na.rm=TRUE,seq(0,1,0.1))),digits=numdig)
    colD <- rbind(paste("D",seq(0,10,1),sep=""),colD)
    stat.res <- cbind(stat.res,colD)
  }
  
  write.table(stat.res,stat.out,col.names=FALSE,sep="\t",quote=FALSE)
  
} # end if(stat)


##########################################################
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


##########################################################
# Treatment successfull
##########################################################
cat("<HTML><HEAD><TITLE>Summary statistics report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
cat("&#10003; Your process is successfull !<BR>",file=log_file,append=T,sep="")
cat("</BODY></HTML>\n",file=log_file,append=T,sep="")


} # end of function

