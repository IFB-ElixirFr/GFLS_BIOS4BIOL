# R Script producing a hierarchical clustering
# Input : a file containing a table with numeric values
#	  except for the first column containing sample names
#	  and the first line containing variable names
#	  separator expected is <TAB>
#
# Clustering method :
#	  euclidean, correlation, ...
#
# Ouptut : a file containing the image of the clustering
#-----------------------------------------------------------------
# Authors : sophie.lamarre(at)insa-toulouse.fr
#	    ignacio.gonzalez(at)toulouse.inra.fr
#	    luc.jouneau(at)inra.fr
#	    valentin.marcon(at)inra.fr
# Version : 0.9
# Date    : 13/3/2017

# The function -------------------------------------
#---------------------------------------------------
h_clust <- function(input_file,
                      group_member_file = NULL,
                      output_file = "out/myplot",
                      log_file = "log/H_Clust.html",
                      format_image_out = "png",
                      distance_method = "euclidean",
                      agglomeration_method = "ward",
                      column_clustering = TRUE,
                      select = NULL,
                      plot_title = "",
                      xlab = "",
                      ylab = "Height",
                      width = 7,
                      height = 7,
                      ppi = 300,
		      na_encoding="NA"
) {
  
  # This function allows to generate hierarchical cluster analysis on a table  according to differents parameters.
  # It needs a dataset : the table of data and optionally a group_member data to set colored labels.
  # It generates a classification tree graphic from hierarchical clustering.
  #
  # Parameters :
  # - input_file : input_file name
  # - group_member_file : input sample/tag group_member_file name
  # - output_file : output_file name
  # - log_file : log file name
  # - format_image_out : graphic format of the output_file. This must be one of "png", "jpeg", "tiff", "pdf"
  # - distance_method : the distance measure to be used. This must be one of "euclidean", "correlation", "maximum", "manhattan", "canberra", "binary" or "minkowski"
  # - agglomeration_method : the agglomeration_method to be used. This should be one of "ward", "single", "complete", "average", "mcquitty", "median" or "centroid"
  # - column_clustering : if TRUE clustering is performed on the columns
  # - select : number of top variables to use for clustering, selected by highest row variance. If NULL all the variables are selected
  # - plot_title : an overall title for the plot
  # - xlab : a title for the x axis
  # - ylab : a title for the y axis
  # - width : the width of the graphics region in inches
  # - height : the height of the graphics region in inches
  # - ppi : the nominal resolution in ppi
  # - na_encoding : label used to indicate missing values
  
  library(RColorBrewer)
  
  #---------------------------------------------------
  # Auxiliary function
  #---------------------------------------------------
  insert.blank = function(x) {paste(strsplit(x, "@$Â§", fixed = TRUE)[[1]], collapse = " ")}
  
  #---------------------------------------------------
  # Titles 
  #---------------------------------------------------
  plot_title = insert.blank(plot_title)
  xlab = insert.blank(xlab)
  ylab = insert.blank(ylab)
  
  #---------------------------------------------------
  # Read and verify data
  #---------------------------------------------------
  #1°) Checks valid for all modules
  if (column_clustering) {
	variable_in_line=1
  	column_use="individual"
  	line_use="variable"
  } else {
	variable_in_line=0
  	line_use="individual"
  	column_use="variable"
  }
	
  log_error=function(message="") {
  		cat("<HTML><HEAD><TITLE>Hierarchical clustering report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
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
  		tab_in=read.csv(file=input_file,sep="\t",header=T,quote="",na.strings=na_encoding)
  		#column names may have been transformed if they conatin space, begin with a digit, ...
  		first_line=readLines(input_file,n=1)
  		colnames(tab_in)=unlist(strsplit(first_line,"\t"))
  		tab_in
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
  
  if (length(dim(tab_in)) != 2 | ncol(tab_in) < 2 | nrow(tab_in) < 2) {
  	log_error(simpleCondition("The table on which you want to do a clustering must be a data table with at least 2 rows and 2 columns."))
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
  		paste("The table on which you want to do a clustering have duplicated values in the first column (",
  			line_use," names) - duplicated ",line_use," names : ",duplicated_rownames,sep="")
  	))
  }
  tab=tab_in[,-1]
  rownames(tab)=rn
  
  #Transpose if clustering on columns
  if (column_clustering) {
	tab=t(tab)
  }
  
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

  #2°) Check specific to clustering procedure
  if (!is.null(group_member_file)) {
    group_member <- read.table(group_member_file, header = FALSE,sep="\t")
    #pour l'instant, on prend la première colonne du fichier de groupe
    if (length(dim(group_member))==2) {
	ncol_group_file=ncol(group_member)
	nrow_group_file=nrow(group_member)
    	n_tab = nrow(tab)
	if (nrow_group_file == n_tab + 1) {
		#We suppose first line is a header
		header=drop(t(group_member[1,]))
		colnames(group_member)=header
		group_member=group_member[-1,]
		nrow_group_file=nrow(group_member)
	}

	if (ncol_group_file==1) {
		if (length(dim(group_member))==2) {
			group_member=as.factor(group_member[,1])
		} else {
			group_member=as.factor(as.character(group_member))
		}
		nrow_group_file=length(group_member)
	} else {#first column specifies the names
		rn=as.character(group_member[,1])
		group_member=as.factor(group_member[,2])
		names(group_member)=rn
    		cluster_names = rownames(tab)
		n1=sum(names(group_member) %in% cluster_names)
		n2=sum(cluster_names %in% names(group_member))
		if (n1 != n2 | n1!=n_tab | n2 !=n_tab) {
			names_in_error="<UL>"
			for( name in names(group_member)) {
				if (!(name %in% cluster_names)) {
					names_in_error=paste(names_in_error,"<LI>",name,"</LI>",sep="")
				}
			}
			names_in_error=paste(names_in_error,"</UL>",sep="")
  			log_error(simpleCondition(
  				paste("You specified a group member file for coloring ",line_use,"s in your cluster.<BR>\n",
				      "This file contains more than one column, therefore we suppose that the first column refers to ",
				      "the ",line_use," names.<BR>\n",
				      "We observed that there is not a complete correspondance between ",line_use," names in the two files you proposed.<BR>\n",
				      "List of ",line_use," names in data group member file which does not have any match with data table file :<BR>",names_in_error,"\n",
				      sep=""
				)
  			))
		}
		#Order colors with names
		group_member=group_member[cluster_names]
	}
    }
    if (nrow_group_file != n_tab) {
  	log_error(simpleCondition(
  		paste("You specified a file for coloring ",line_use,"s in your cluster but this file contains ",
		      nrow_group_file," lines and your table contains ",n_tab," ",line_use,"s.",sep="")
  	))
    }
  }
  

  if (!is.null(select)) {
	  if (!is.numeric(select) | select<=0)
	  	log_error(simpleCondition(
	  		paste("You specified a value of top ",line_use,"s for your clustering. ",
			      "This value should be a positive integer.",sep="")
	  	))
	  select <- ceiling(select)
	  select <- min(select, nrow(tab))
	  select <- order(apply(tab,1,sd), decreasing = TRUE)[1:select]
	  if (!is.null(group_member_file)) group_member <- group_member[select]
	  tab=tab[select,]
  }

  #Does this table contains NA values
  for (i in 1:nrow(tab)) {
	if (sum(is.na(tab[i,]))!=0) {
		tab[i,is.na(tab[i,])]=mean(tab[i,],na.rm=T)
	}
  }
  
  # Distance matrix ----------------------------------
  #---------------------------------------------------
  
  if (distance_method == "correlation") {
	tab.dist <- as.dist(1 - cor(t(as.matrix(tab)), method = "pearson"))
  } else {
	tab.dist <- dist(tab, method = distance_method)
  }

  
  # Hierarchical cluster -----------------------------
  #---------------------------------------------------
  hc <- as.dendrogram(hclust(tab.dist, method = agglomeration_method))
  
  # Output -------------------------------------------
  #---------------------------------------------------
  lab.cex = min(1, 1/log(attr(tab.dist, "Size"), base = 5))
  output_file = paste(output_file, format_image_out, sep = ".")
  
  if (is.null(group_member_file)) {
    if (format_image_out == "png") {
      png(output_file, width = width, height = height, units = "in", res = ppi)
      plot(hc, main = plot_title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
    
    if (format_image_out == "jpeg") {
      jpeg(output_file, width = width, height = height, units = "in", res = ppi)
      plot(hc, main = plot_title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
    
    if (format_image_out == "tiff") {
      tiff(output_file, width = width, height = height, units = "in", res = ppi)
      plot(hc, main = plot_title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
    
    if (format_image_out == "pdf") {
      pdf(output_file, width = width, height = height)
      plot(hc, main = plot_title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
  } else {
    # Color label function -----------------------------
    #---------------------------------------------------
    colLab <- function(dend, gp.member, lab.cex) {
      label.colors <- brewer.pal(max(3, nlevels(gp.member)), "Set1")
      if(is.leaf(dend)) {
        att <- attributes(dend)
        labCol <- label.colors[gp.member[which(names(gp.member) == att$label)]]
        attr(dend, "nodePar") <- c(att$nodePar, list(lab.col = labCol, pch = c(NA, NA), lab.cex = lab.cex))
      }
      dend
    }
    
    names(group_member) <- attr(tab.dist, "Labels")
    
    if (format_image_out == "png") {
      png(output_file, width = width, height = height, units = "in", res = ppi)
      plot(dendrapply(hc, colLab, group_member, lab.cex), main = plot_title, xlab = xlab, ylab = ylab)
      dev.off()
    }
    
    if (format_image_out == "jpeg") {
      jpeg(output_file, width = width, height = height, units = "in", res = ppi)
      plot(dendrapply(hc, colLab, group_member, lab.cex), main = plot_title, xlab = xlab, ylab = ylab)
      dev.off()
    }
    
    if (format_image_out == "tiff") {
      tiff(output_file, width = width, height = height, units = "in", res = ppi)
      plot(dendrapply(hc, colLab, group_member, lab.cex), main = plot_title, xlab = xlab, ylab = ylab)
      dev.off()
    }
    
    if (format_image_out == "pdf") {
      pdf(output_file, width = width, height = height)
      plot(dendrapply(hc, colLab, group_member, lab.cex), main = plot_title, sub = xlab, ylab = ylab)
      dev.off()
    }
  }

  ##########################################################
  # Treatment successfull
  ##########################################################
  cat("<HTML><HEAD><TITLE>Hierarchical clustering report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
  cat("&#10003; Your clustering process is successfull !<BR>",file=log_file,append=T,sep="")
  cat("</BODY></HTML>\n",file=log_file,append=T,sep="")
   
  q(save="no",status=0)

} # end of function

#### Test clustering ####
#LJO : 13/3/2017
#setwd("H:/INRA/cati/groupe stats/Galaxy/hclust")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon1")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon2",column_clustering=FALSE,
#         xlab="Competitors",ylab="Distance")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon3",column_clustering=FALSE,
#         select=5)
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon4",column_clustering=FALSE,
#         distance_method="correlation",agglomeration_method="average",
#         format_image_out="tiff",ppi=100,width=3,height=3
#)
##Group : competitors
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon5",column_clustering=FALSE,
#         xlab="Competitors",ylab="Distance",group_member_file="in/competitors_groups - 1 column.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon6",column_clustering=FALSE,
#         xlab="Competitors",ylab="Distance",group_member_file="in/competitors_groups - 2 columns.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon7",column_clustering=FALSE,
#         xlab="Competitors",ylab="Distance",group_member_file="in/competitors_groups - 1 column with header.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon8",column_clustering=FALSE,
#         xlab="Competitors",ylab="Distance",group_member_file="in/competitors_groups - 2 columns with header.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon8",column_clustering=FALSE,
#         xlab="Competitors",ylab="Distance",group_member_file="in/competitors_groups - 2 columns - with error.txt")
#
##Group : competitions
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon9",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",group_member_file="in/competitions_groups - 1 column.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon10",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",group_member_file="in/competitions_groups - 2 columns.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon11",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",group_member_file="in/competitions_groups - 1 column with header.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon12",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",group_member_file="in/competitions_groups - 2 columns with header.txt")
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon13",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",group_member_file="in/competitions_groups - 2 columns - with error.txt")
#
##Missing values
#hclustfun(input_file="in/decathlon - with NA.txt",plot_title="declathlon",output_file="out/decathlon14",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",na_encoding="missing_value")
#
##Top 5 competitions
#hclustfun(input_file="in/decathlon.txt",plot_title="declathlon",output_file="out/decathlon15",column_clustering=TRUE,
#         xlab="Competitions",ylab="Distance",select=5)
