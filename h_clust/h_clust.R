# The function -------------------------------------
#---------------------------------------------------
h_clust <- function(count.file,
                      group.member.file = NULL,
                      output.file = "myplot%d",
                      format.image.out = "png",
                      distance.method = "euclidean",
                      agglomeration.method = "ward",
                      sample.clustering = TRUE,
                      select = NULL,
                      plot.title = "",
                      xlab = "",
                      ylab = "Height",
                      width = 7,
                      height = 7,
                      ppi = 300) {
  
  # This function allows to generate hierarchical cluster analysis on a table  according to differents parameters.
  # It needs a dataset : the table of data and optionally a group member data to set colored labels.
  # It generates a classification tree graphic from hierarchical clustering.
  #
  # Parameters :
  # - count.file : input count file name
  # - group.member.file : input sapmle/tag group member file name
  # - output.file : output file name
  # - format.image.out : graphic format of the output file. This must be one of "png", "jpeg", "tiff", "pdf"
  # - distance.method : the distance measure to be used. This must be one of "euclidean", "correlation", "maximum", "manhattan", "canberra", "binary" or "minkowski"
  # - agglomeration.method : the agglomeration method to be used. This should be one of "ward", "single", "complete", "average", "mcquitty", "median" or "centroid"
  # - sample.clustering : if TRUE clustering is performed on the samples
  # - select : number of top genes to use for clustering, selected by highest row variance. If NULL all the genes are selected
  # - plot.title : an overall title for the plot
  # - xlab : a title for the x axis
  # - ylab : a title for the y axis
  # - width : the width of the graphics region in inches
  # - height : the height of the graphics region in inches
  # - ppi : the nominal resolution in ppi
  
  library(RColorBrewer)
  
  # Auxiliary function -------------------------------
  #---------------------------------------------------
  insert.blank = function(x) {paste(strsplit(x, "@$§", fixed = TRUE)[[1]], collapse = " ")}
  
  # Titles -------------------------------------------
  #---------------------------------------------------
  plot.title = insert.blank(plot.title)
  xlab = insert.blank(xlab)
  ylab = insert.blank(ylab)
  
  # Input --------------------------------------------
  #---------------------------------------------------
  tab.data <- as.matrix(read.delim(count.file, row.names = 1))
  if (length(dim(tab.data)) != 2 | ncol(tab.data) < 2 | nrow(tab.data) < 2)
    stop(paste0("the '", basename(count.file), "' file must be a data table with at least 2 rows and 2 columns."),
         call. = FALSE)
  
  if (!is.null(group.member.file)) {
    group.member <- as.factor(t(read.delim(group.member.file, header = FALSE)))
    if (sample.clustering) n = ncol(tab.data) else n = nrow(tab.data)
    if (length(group.member) != n)
      stop(paste0("the '", basename(group.member.file), "' file must be a data table with ", n, " rows."),
           call. = FALSE)
  }
  
  if (!sample.clustering) {
    if (is.null(select)) select <- nrow(tab.data)
    if (!is.numeric(select) | select<=0)
      stop(paste0("'select' must be a positive integer specifying the desired number of top genes."),
           call. = FALSE)
    select <- round(select)
    select <- min(select, nrow(dds))
    select <- order(rowMeans(tab.data), decreasing = TRUE)[1:select]
    if (!is.null(group.member.file)) group.member <- group.member[select]
  }
  
  # Distance matrix ----------------------------------
  #---------------------------------------------------
  if (sample.clustering) tab.data <- t(tab.data) else tab.data <- tab.data[select, ]
  
  if (distance.method == "correlation") tab.dist <- as.dist(1 - cor(t(as.matrix(tab.data)), method = "pearson"))
  else tab.dist <- dist(tab.data, method = distance.method)
  
  # Hierarchical cluster -----------------------------
  #---------------------------------------------------
  hc <- as.dendrogram(hclust(tab.dist, method = agglomeration.method))
  
  # Output -------------------------------------------
  #---------------------------------------------------
  lab.cex = min(1, 1/log(attr(tab.dist, "Size"), base = 5))
  output.file = paste(output.file, format.image.out, sep = ".")
  
  if (is.null(group.member.file)) {
    if (format.image.out == "png") {
      png(output.file, width = width, height = height, units = "in", res = ppi)
      plot(hc, main = plot.title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
    
    if (format.image.out == "jpeg") {
      jpeg(output.file, width = width, height = height, units = "in", res = ppi)
      plot(hc, main = plot.title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
    
    if (format.image.out == "tiff") {
      tiff(output.file, width = width, height = height, units = "in", res = ppi)
      plot(hc, main = plot.title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
    
    if (format.image.out == "pdf") {
      pdf(output.file, width = width, height = height)
      plot(hc, main = plot.title, xlab = xlab, ylab = ylab,
           nodePar = list(pch = c(NA, NA), lab.cex = lab.cex))
      dev.off()
    }
  }
  else {
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
    
    names(group.member) <- attr(tab.dist, "Labels")
    
    if (format.image.out == "png") {
      png(output.file, width = width, height = height, units = "in", res = ppi)
      plot(dendrapply(hc, colLab, group.member, lab.cex), main = plot.title, xlab = xlab, ylab = ylab)
      dev.off()
    }
    
    if (format.image.out == "jpeg") {
      jpeg(output.file, width = width, height = height, units = "in", res = ppi)
      plot(dendrapply(hc, colLab, group.member, lab.cex), main = plot.title, xlab = xlab, ylab = ylab)
      dev.off()
    }
    
    if (format.image.out == "tiff") {
      tiff(output.file, width = width, height = height, units = "in", res = ppi)
      plot(dendrapply(hc, colLab, group.member, lab.cex), main = plot.title, xlab = xlab, ylab = ylab)
      dev.off()
    }
    
    if (format.image.out == "pdf") {
      pdf(output.file, width = width, height = height)
      plot(dendrapply(hc, colLab, group.member, lab.cex), main = plot.title, sub = xlab, ylab = ylab)
      dev.off()
    }
  }
} # end of function
