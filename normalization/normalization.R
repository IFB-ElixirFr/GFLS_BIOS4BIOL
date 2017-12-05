# R Script implementing different kind of normalisation 
# Input : a file containing a table with numeric values
#	  except for the first column containing sample names
#	  and the first line containing variable names
#	  separator expected is <TAB>
#
# Normalization method :
#	  log, DESeq2, Rlog, Standard_score, Pareto, TSS, TSS+CLR, Pareto
#
# Ouptut : input table with values normalized according
#	   to the normalization procedure chosen
#-----------------------------------------------------------------
# Authors : luc.jouneau(at)inra.fr
#	    valentin.marcon(at)inra.fr
# Version : 0.9
# Date    : 30/08/2017
#-----------------------------------------------------------------

normalization=function(
##########################################################
# Function input
##########################################################
#Possible values : "log", "DESeq2", "Rlog", "Standard_score", "Pareto", "TSS", "TSS_CLR"
transformation_method="Standard_score",
na_encoding="NA",
#Path to file containg table of values (separator="tab")
input_file="",
#Path to file produced after transformation
output_file="out/table_out.txt",
#Path to file containing messages for user if something bad happens
log_file="log/normalization_report.html",
#Boolean flag (0/1) indicating if variables are in line or in columns
variable_in_line="1") {

##########################################################
# Read and verify data
##########################################################
#1°) Checks valids for all modules
if (variable_in_line=="1") {
	column_use="individual"
	line_use="variable"
} else {
	line_use="individual"
	column_use="variable"
}
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
		cat("  <LI> this value may contain character '.' as decimal separator or '",na_encoding,"' for missing values</LI>\n",file=log_file,append=T,sep="")
		cat("</UL>\n",file=log_file,append=T,sep="")
		cat("-------<BR>\nError messages recieved :<BR><FONT color=red>\n",conditionMessage(message),"</FONT>\n",file=log_file,append=T,sep="")
		cat("</BODY></HTML>\n",file=log_file,append=T,sep="")
		q(save="no",status=1)
}

tab_in=tryCatch(
	{
		tab_in=read.table(file=input_file,sep="\t",header=T,quote="\"",na.strings=na_encoding,check.names=FALSE)
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
	log_error(simpleCondition("The table you want to normalize contains less than two columns."))
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
		paste("The table you want to normalize have duplicated values in the first column (",
			line_use," names) - duplicated ",line_use," names : ",duplicated_rownames,sep="")
	))
}
tab=tab_in[,-1]
rownames(tab)=rn

#Check all columns are numeric
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
			paste("Column '",colnames(tab)[i],"' of your table contains non numeric values. Please check its content (on line #",sel," : value='",value,"').",sep="")
		))
	}
	if (length(cell.with.na)==0 & sum(na.v1)!=0) {
		cell.with.na=c(i,which(na.v1)[1])
	}
}

#2°) Checks only valid for normalization module
if (transformation_method %in% c("DESeq2","Rlog")) {
	#Check there is no missing values
	if (length(cell.with.na)!=0) {
		log_error(simpleCondition(
			paste("Column '",colnames(tab)[cell.with.na[1]],"' of your table contains missing values (see line #",cell.with.na[2],").\n",
			      transformation_method," normalization does not accept missing values. ",sep="")
		))
	}
}
if (transformation_method %in% c("DESeq2","Rlog","TSS","TSS_CLR")) {
	#Check values are integer
	for (i in 1:ncol(tab)) {
		if (!is.integer(tab[,i])) {
			sel=which(!is.integer(tab[,i]))
			sel=sel[1]
			value=tab[sel,i]
			log_error(simpleCondition(
				paste("Column '",colnames(tab)[i],"' of your table contains non integer values.\n",
				      transformation_method," normalization only accepts integer values. ",
				      "Please check its content (on line #",sel," : value=",value,").",sep="")
			))
		}
	}
}

if (transformation_method %in% c("log","TSS","TSS_CLR","DESeq2","Rlog")) {
	#Check values are positive
	for (i in 1:ncol(tab)) {
		if (sum(tab[,i]>=0 | is.na(tab[,i]))!=nrow(tab)) {
			sel=which(tab[,i]<0)
			sel=sel[1]
			value=tab[sel,i]
			log_error(simpleCondition(
				paste("Column '",colnames(tab)[i],"' of your table contains negative values.\n",
				      transformation_method," normalization only accepts positive or null values. ",
				      "Please check its content (on line #",sel," : value=",value,").",sep="")
			))
		}
	}
}

##########################################################
# End of data checks
##########################################################

### Transpose if variable are in line ###
if (variable_in_line=="1") {
	#Transpose matrix
	tab=t(tab)
}

##########################################################
### Value transformation
##########################################################

#Avoid null values when there is a log transformation
na.replaced=c()
log.transformed=FALSE
if (transformation_method %in% c("log","TSS_CLR")) {
	log.transformed=TRUE
	for (idx_col in 1:ncol(tab)) {
		sel=tab[,idx_col]==0
		na.replaced=cbind(na.replaced,sel)
		tab[sel,idx_col]=1e-2
	}
}

### log ###
if (transformation_method=="log") {
	tab=log2(tab)
}

### DESeq2 or Rlog ###
if (transformation_method %in% c("DESeq2","Rlog")) {
	library(DESeq2)
	n <- ncol(tab)
	dds <- DESeqDataSetFromMatrix(tab,
				      colData = data.frame(condition = c("a", rep("b", n - 1))),
				      design = formula(~ condition))
	colnames(dds) <- colnames(tab)
	dds <- estimateSizeFactors(dds)
	tab <- switch(transformation_method,
                DESeq2 = counts(dds, normalized = TRUE),
                Rlog   = assay(rlogTransformation(dds))
	)
}

### Standard_score ###
if (transformation_method=="Standard_score") {
	tab=scale(tab)
}

### Pareto ###
if (transformation_method=="Pareto") {
	tab.centered <- apply(tab, 2, function(x) x - mean(x,na.rm=TRUE))
	tab.sc <- apply(tab.centered, 2, function(x) x/sqrt(sd(x,na.rm=TRUE)))
	tab=tab.sc
}

### TSS ###
if (transformation_method=="TSS") {
	tab= t(apply(tab, 1, function(x) x/sum(x,na.rm=TRUE)))
}

### TSS + CLR avec function de mixOmics ###
if (transformation_method=="TSS_CLR") {
	#From http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
	geometric.mean = function(x, na.rm=TRUE){
		exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
	}
	tab = t(apply(tab+1e-2,1,function(x) log(x/geometric.mean(x,na.rm=TRUE))))
}


#If there is a log transformation put 0 where there was NA
if (log.transformed) {
	for (idx_col in 1:ncol(tab)) {
		tab[na.replaced[,idx_col],idx_col]=0
	}
}

#If there are missing values, replace it with NA_enconding
for (idx_col in 1:ncol(tab)) {
	sel=is.na(tab[,idx_col])
	tab[sel,idx_col]=na_encoding
}

##########################################################
# Prepare and write output table
##########################################################
if (variable_in_line=="1") {
	#Transpose matrix again
	tab=t(tab)
}

tab_out=cbind(rownames(tab),tab)
colnames(tab_out)[1]=colnames(tab_in)[1]

write.table(file=output_file,tab_out,sep="\t",row.names=F,quote=F)

##########################################################
# Treatment successfull
##########################################################
cat("<HTML><HEAD><TITLE>Normalization report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
cat(paste("&#10132; You choose to apply the transformation method :",transformation_method,"<BR>"),file=log_file,append=F,sep="")
cat("&#10003; Your normalization process is successfull !<BR>",file=log_file,append=T,sep="")
cat("</BODY></HTML>\n",file=log_file,append=T,sep="")

q(save="no",status=0)

} # end of function

##########################################################
# Test
##########################################################
#Used for debug : LJO 6/3/2017
#normalization()
#setwd("H:/INRA/cati/groupe stats/Galaxy/normalisation")
#normalization(transformation_method="Standard_score",na_encoding="NA",input_file="datasets/valid - decathlon.txt",output_file="out/table_out.txt",log_file="log/normalization.html",variable_in_line="0")
#normalization(transformation_method="Pareto",na_encoding="NA",input_file="datasets/valid - decathlon.txt",output_file="out/table_out.txt",log_file="log/normalization.html",variable_in_line="1")

