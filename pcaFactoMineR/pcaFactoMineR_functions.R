####################  fonctions utilisées #####################################################

standardisation <- function(rawX,centrage=TRUE,scaling=c("none","uv","pareto"))
{
  # rawX : matrice nxp contenant les données brutes
    # Forme à confirmer avec SLA colonne 1 = identifiant,colonne 2 = facteur biologique???
    # Si oui "supprimer" ces 2 colonnes pour n'avoir que variables quantitatives
  # centrage = parametre booleen (TRUE/FALSE),par défaut = TRUE,indiquant si centrage donnees à faire
  # scaling = nom de la methode de standardisation à appliquer aux donnees
    # none = aucune standardisation
    # uv = division par ecart type
    # pareto = division par racine carree ecart type
  # scaledX : matrice nxp contenant les variables quantitatives standardisees (centrage +/- scaling)
  scaledX <- prep(rawX,center=centrage,scale=scaling)
  return(scaledX)
}

###############################################################################################
plot_ebouli <- function(res.PCA,mt,cexc)
{ 
  # res.PCA résultat de l'ACP est renvoyé par la fonction PCA (factoMineR) de type list contient
  # l'ensemble des résultats dont les valeurs propres et % variance assicées à chaque cp
  # La fonction plot les valeurs propres et les % de variance 
  # expliquée par chaque composante et la variance cumulée
  
  eig <- res.PCA$eig
  barplot(eig[[1]])
  #abline(h=1)
  title(main=mt,sub="Eigen values")
  barplot(eig[[2]],ylim=c(0,100))
  points(eig[[3]],type="b")
  title(main=mt,sub="Variance explained",cex=cexc)

}
  
###############################################################################################
plot.contrib <- function(res.PCA,mt,cexc) 
{
  # Fonction de plot des contributions sur les n composantes 
  # principales choisies par l'utilisateur (lastpc)
  # En entrée,la fonction prend le résultats de l'ACP FactoMineR de type list 

  lastpc  <- dim(res.PCA$var$contrib)[2] 
  ## plot contributions
  contrib <- res.PCA$var$contrib
  for (c in 1:lastpc) {
    barplot(quantile(contrib[,c],probs=seq(0,1,0.025)))  
    title(main=mt,sub=paste("Quantile plot Contrib PC",c,sep=""),cex=cexc)
    barplot(contrib[order(contrib[,c]),1])
    title(main=mt,sub=paste("Contributions PC",c,sep=""),cex=cexc)
  }
}

###############################################################################################
pca.var <- function(res.PCA,contribmin=c(0,0),mt,cexc,linev=3,plotax=c(1,2)) 
{
  # Loading plot avec eventuellement selection des variables par leurs contributions
  # En entrée,la fonction prend le résultats de l'ACP FactoMineR de type list 
  # On passe en arguments : les contributions pour les 2 axes définis par l'utilisateur,
  # le titre du graphique,la taille des caractères (cex), 
  # ??? est-ce que la fonction doit plotter toutes les composantes ???
  
  ### sélection des variables sur leur contribution. 
  # les valeurs des contributions par défaut (0,0) permettent d'afficher 
  # toutes les variables.
  selvar=c(which(res.PCA$var$contrib[,plotax[1]]>contribmin[1]),
           which(res.PCA$var$contrib[,plotax[2]]>contribmin[2]))	 
  selvar <- selvar[!duplicated(selvar)]
  fres.PCA <- res.PCA
  fres.PCA$var$coord <- res.PCA$var$coord[selvar,]
  fres.PCA$var$cor <- res.PCA$var$cor[selvar,]
  fres.PCA$var$cos2 <- res.PCA$var$cos2[selvar,]
  fres.PCA$var$contrib <- res.PCA$var$contrib[selvar,]

  #### Plot du cercle des corrélations
  plot.PCA(fres.PCA,choix="var",cex=cexc,axes=plotax,title=NULL, habillage=1)  
  title(main = mt,line = linev,cex=cexc)
}

###############################################################################################
pca.indiv <- function(res.PCA,hb,facteur=NULL,contribmin=c(0,0),mt,cexc,linev=3,plotax=c(1,2)) 
{
  # Espace des individus
  # En entrée,la fonction prend le résultats de l'ACP FactoMineR de type list 
  # On passe en arguments : les contributions pour les 2 axes définis par l'utilisateur,
  # le titre du graphique,la taille des caractères (cex), 
  # ??? est-ce que la fonction doit plotter toutes les composantes ???
  selvar=c(which(res.PCA$var$contrib[,plotax[1]]>contribmin[1]),
           which(res.PCA$var$contrib[,plotax[2]]>contribmin[2]))
  
  fres.PCA <- res.PCA
  fres.PCA$var$coord <- res.PCA$var$coord[selvar,]
  fres.PCA$var$cor <- res.PCA$var$cor[selvar,]
  fres.PCA$var$cos2 <- res.PCA$var$cos2[selvar,]
  fres.PCA$var$contrib <- res.PCA$var$contrib[selvar,]
  
  #### Plot l'espace des individus
  if (hb ==1 ) 
  {
    aa <- cbind.data.frame(facteur,res.PCA$ind$coord)
    bb <- coord.ellipse(aa,bary=TRUE,level.conf=0.99) 

    colVal = rainbow(length(unique(facteur)))
    plot.PCA(fres.PCA,choix="ind",habillage=1,cex=cexc,axes=plotax,title=NULL,invisible="quali")
    title(main = mt,line = linev,cex=cexc)

    plot.PCA(fres.PCA,choix="ind",habillage=1,label="none",cex=cexc,axes=plotax,title=NULL,ellipse=bb) 
    title(main = mt,line = linev,cex=cexc)
  }
  else 
  {
    plot.PCA(fres.PCA,choix="ind",cex=cexc,axes=plotax,title=NULL) 
    title(main = mt,line = linev,cex=cexc)
  }
}

###############################################################################################
## MODIFICATIONS 15062017
## Suppression standardisation car fonction externe
## Concatenation samplemetadata et datamatrix avec merge si pas meme ordre de tri
## HYPOTHESES : 1) datamatrix : nxp et 2) colonne 1 = identifiants individus
###############################################################################################

pca.main <- function(ids,bioFact,ncp,hb=0,minContribution=c(0,0),mainTitle=NULL,textSize=0.5,linev=3,
                     principalPlane=c(1,2),eigenplot=0,contribplot=0,scoreplot=0,loadingplot=0,nomGraphe,
                     variable_in_line=0,log_file) 
{

  res<-NULL

  ## Variable in line or column?
  if (variable_in_line==1){
        column_use="individual"
        line_use="variable"
  } else {
        line_use="individual"
        column_use="variable"
  }
  

  ## Fonction d'écriture du fichier de log

log_error=function(message="") {
  cat("<HTML><HEAD><TITLE>PCA FactoMineR report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
  cat("&#9888 An error occurred while trying to read your table.\n<BR>",file=log_file,append=T,sep="")
  cat("Please check that:\n<BR>",file=log_file,append=T,sep="")
  cat("<UL>\n",file=log_file,append=T,sep="")
  cat("  <LI> the table you want to process contains the same number of columns for each line</LI>\n",file=log_file,append=T,sep="")
  cat("  <LI> the first line of your table is a header line (specifying the name of each ",column_use,")</LI>\n",file=log_file,append=T,sep="")
  cat("  <LI> the first column of your table specifies the name of each ",line_use,"</LI>\n",file=log_file,append=T,sep="")
  cat("  <LI> both individual and variable names should be unique</LI>\n",file=log_file,append=T,sep="")
  cat("  <LI> each value is separated from the other by a <B>TAB</B> character</LI>\n",file=log_file,append=T,sep="")
  cat("  <LI> except for first line and first column, table should contain a numeric value</LI>\n",file=log_file,append=T,sep="")
  cat("  <LI> this value may contain character '.' as decimal separator </LI>\n",file=log_file,append=T,sep="")
  cat("</UL>\n",file=log_file,append=T,sep="")
  cat("-------<BR>\nError messages recieved :<BR><FONT color=red>\n",conditionMessage(message),"</FONT>\n",file=log_file,append=T,sep="")
  cat("</BODY></HTML>\n",file=log_file,append=T,sep="")
  q(save="no",status=1)
}

  # Sortie graphique
  if (eigenplot==1 || contribplot==1 || scoreplot==1 || loadingplot==1)
    pdf(nomGraphe,onefile=TRUE)

  # Verify data
verif_data=function(){
  if (length(dim(ids)) != 2 | ncol(ids) < 2 | nrow(ids) < 2)
      log_error(simpleCondition("The table on which you want to do PCA must be a data table with at least 2 rows and 2 columns."))
  
    tab=as.matrix(data)
    cell.with.na=c()
    colstart=2-variable_in_line #
    for (i in colstart:ncol(tab)) {
      na.v1=is.na(tab[,i])
      na.v2=is.na(as.numeric(tab[,i]))
      if (sum(na.v1)!=sum(na.v2)) {
        sel=which(na.v1!=na.v2)
        sel=sel[1]
        value=tab[sel,i]
        log_error(simpleCondition(
          paste("Column '",colnames(tab)[i],"' of your table contains non numeric values. Please check its content (on line #",sel," : value='",value,"'). Maybe you will need to specify that variable are in column.",sep="")
        ))
      }
      if (length(cell.with.na)==0 & sum(na.v1)!=0) {
        cell.with.na=c(i,which(na.v1)[1])
      }
    }  
}


  ## Disposition matrice de donnees
    ## Transposition si variables en ligne
  Tids <- ids

  if (variable_in_line == 1)
  {
    
    rownames(Tids) <- Tids[,1]
    Tids <- Tids[,-1]
    Tids <- t(Tids)
    Tids <- data.frame(rownames(Tids), Tids)
    colnames(Tids)[1] <- "Sample"
  }
  print(Tids)
  rownames(Tids) <- as.character(Tids[,1])
  print(Tids)

   
  ## suivant la presence variable qualitative (hb=1),l'appel a la fonction PCA est modifié
  if (hb==1) 
  {
    ## Concatenation
    data <- merge(bioFact,Tids,by.x=1,by.y=1)
    ## Suppression identifiants individus
    data <- data[,-1]
    data[,1] <- as.factor(data[,1])
    facteur <- as.factor(bioFact[,-1]) 
    verif_data()   

    ## Analyse
    res <- PCA(data,scale.unit=FALSE,ncp,graph=F,quali.sup=1)    
  }
  else 
  { 
    ## Suppression identifiants individus
    data <- Tids[,-1]
    verif_data()

    ## Analyse
    res <- PCA(data,scale.unit=FALSE,ncp,graph=F)
  }
  
  if (eigenplot==1) 
  {
    par(mfrow=c(1,2))
    plot_ebouli(res,mt=mainTitle,cexc=textSize)
  }
    
  if (contribplot==1)
  {
    par(mfrow=c(1,2))
    plot.contrib(res,mt=mainTitle,cexc=textSize)
  }
    
  if (scoreplot==1) 
  { 
    if (hb==0)
    {    
      par(mfrow=c(1,1))
      pca.indiv(res,hb=0,contribmin=minContribution,mt=mainTitle,cexc=textSize,plotax=principalPlane)
    }
    if (hb==1)
    { 
      par(mfrow=c(1,1))
      pca.indiv(res,hb=1,facteur=facteur,contribmin=minContribution,mt=mainTitle,cexc=textSize,plotax=principalPlane) 
    }
  }
  if (loadingplot==1) 
  { 
    par(mfrow=c(1,1))
    pca.var(res,contribmin=minContribution,mt=mainTitle,cexc=textSize,plotax=principalPlane)
  }
  
  if (eigenplot==1 || contribplot==1 || scoreplot==1 || loadingplot==1)
    dev.off()


  if (!is.null(res)){
    cat("<HTML><HEAD><TITLE>PCA FactoMineR report report</TITLE></HEAD><BODY>\n",file=log_file,append=F,sep="")
    cat("&#10003; Your process is successfull !<BR>",file=log_file,append=T,sep="")
    cat("</BODY></HTML>\n",file=log_file,append=T,sep="")



  }
}

###############################################################################################
