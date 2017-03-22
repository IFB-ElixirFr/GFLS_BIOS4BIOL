#Declaration de l espace de travail et du chemin de l executable R :
use strict;
use Carp;
use Statistics::R;
use Cwd qw(abs_path);
use File::Basename;

use POSIX;

#Variables Globales ###########################
defined $ENV{'MY_GALAXY_DIR'} || die "MY_GALAXY_DIR environment variable not defined";
use lib "$ENV{'MY_GALAXY_DIR'}";
use GalaxyPath;

my ($working_dir,$r_bin,$log_dir,$R,$cmd,$line);

#pour voir ce que le xml renvoie au wrapper perl. Sachant que $0 est la ligne de commande et @ARGV les arguments
#printf STDERR "%s\n",join("***** ",$0,@ARGV);

# Parameters :
  # - input_file : input file name
  # - group_member_file : input sample/tag group member file name
  # - output_file : output file name
  # - log_file : log file name
  # - format_image_out : graphic format of the output file. This must be one of "png", "jpeg", "tiff", "pdf"
  # - distance_method : the distance measure to be used. This must be one of "euclidean", "correlation", "maximum", "manhattan", "canberra", "binary" or "minkowski"
  # - agglomeration_method : the agglomeration method to be used. This should be one of "ward", "single", "complete", "average", "mcquitty", "median" or "centroid"
  # - column_clustering : if TRUE clustering is performed on the columns
  # - select : number of top variables to use for clustering, selected by highest row variance. If NULL all the variables are selected 
  # - plot_title : an overall title for the plot
  # - xlab : a title for the x axis
  # - ylab : a title for the y axis
  # - width : the width of the graphics region in inches
  # - height : the height of the graphics region in inches
  # - ppi : the nominal resolution in ppi 
  # - na_code : label used to indicate missing values

#Parametres d'entree
my $input_file               =$ARGV[0]; #input file name
my $group_member_file        =$ARGV[1]; #input sapmle/tag group member file name
my $output_file              =$ARGV[2]; #output file name
my $log_file                 =$ARGV[3]; #log file name
my $distance_method          =$ARGV[4]; #the distance measure to be used. This must be one of "euclidean", "correlation", "maximum", "manhattan", "canberra", "binary" or "minkowski"
my $agglomeration_method     =$ARGV[5]; #the agglomeration method to be used. This should be one of "ward", "single", "complete", "average", "mcquitty", "median" or "centroid"
my $column_clustering        =$ARGV[6]; #if TRUE clustering is performed on the columns
my $select                   =$ARGV[7]; #number of top variables to use for clustering, selected by highest row variance. If NULL all the variables are selected 
my $plot_title               =$ARGV[8]; #an overall title for the plot
my $xlab                     =$ARGV[9]; #a title for the x axis
my $ylab                     =$ARGV[10];#a title for the y axis
my $width                    =$ARGV[11];#the width of the graphics region in inches
my $height                   =$ARGV[12];#the height of the graphics region in inches
my $ppi                      =$ARGV[13];#the nominal resolution in ppi 
my $NA_code                  =$ARGV[14];#label used to indicate missing values


#ligne de commande test :
#perl h_clust.pl Data/gene_counts.txt Data/member.csv outfile none  euclidean  ward TRUE 100 titre x y 7 7 300

# SETTING GLOBAL VARIABLES ################################################################################
my $cfg = GalaxyPath->new( -file => $ENV{"GALAXY_CONFIG_FILE"} );
my $file_path = $cfg->my_path( 'workPath', 'MYWORKSPACE' );
$r_bin = $cfg->my_path( 'toolsPath', 'R_BIN_PATH' );
my $SCRIPTS_R_path = $cfg->my_path( 'workPath', 'SCRIPTS_R_path' );


#Récuperer le numero (unique) de l'output
my ($nb) = ($output_file=~/galaxy_dataset_(\d+)\.\S+$/);

#remplacer les espaces par des underscores
$plot_title =~ s/@$§/ /g;
$xlab =~ s/@$§/ /g;
$ylab =~ s/@$§/ /g;

#On suppose que le fichier de donnees en entree a ete place dans un repertoire
#de travail working_dir
## smaman : $working_dir=abs_path($file_in);
#Repertoire de sortie cree par le script, verif des droits d'ecriture sur ce repertoire de sortie
`cd $file_path/; mkdir $nb; chmod -R 777 $nb/; cd $nb/;`;
my $working_dir= "$file_path/$nb";
#$working_dir=~s/\/[^\/]*$//;
#$file_in=~s/^.*\///;

print STDOUT "Working directory : $working_dir \n";

#Recuperation de la matrice entrante dans le repertoir de travail
#`cp $group_member_file $working_dir/group_member_file`;

#Fichier de sortie des donnees de description statistiques
#$stat_out = "$working_dir/stat_out";

#Log dir
$log_dir = $working_dir;

#déaration de l'objet $R et ouverture du pont :
$R = Statistics::R->new(
    "r_bin"   => $r_bin,
    "log_dir" => $log_dir,
) or die "Problem with R : $!\n";
# Ouverture du pont
$R->startR;


print STDOUT "Ouverture du pont R \n";

#envoi du repertoire de travail
#$R->send(qq`setwd("$working_dir")`);

#Read R script
$cmd="";
open IN,"< $SCRIPTS_R_path/h_clust.R"
	or die "Unable to read R script : $!\n";
while ($line=<IN>) {
	$cmd.=$line;
}
close(IN);

#Declare R function
#$R->send($cmd);

print STDOUT "Declaration du script R \n";

if ($group_member_file eq "NO"){
    $group_member_file="NULL"
} else {
    $group_member_file="'$group_member_file'"
}
    
    
$R->send(
     "setwd('$working_dir')\n".
     "$cmd\n".
     "h_clust( ".
     "input_file = '$input_file', ".  #########
     "group_member_file = $group_member_file, ".
     "output_file = '$working_dir/out', ".
     "log_file = '$log_file', ".
     "format_image_out = 'jpeg', ".
     "distance_method = '$distance_method', ".
     "agglomeration_method = '$agglomeration_method', ".
     "column_clustering = $column_clustering, ".  #########
     "select = $select, ".
     "plot_title = '$plot_title', ".
     "xlab = '$xlab', ".
     "ylab = '$ylab', ".
     "width = $width,  ".
     "height = $height, ".
     "ppi = $ppi, ".
     "na_encoding= $NA_code)\n");
     
print STDOUT "\n h_clust(^M
      input_file = \"$input_file\",^M
      group_member_file = $group_member_file,^M
      output_file = \"$working_dir/out\",^M
      log_file = \"$log_file\",^M
      format_image_out = \"jpeg\",^M
      distance_method = \"$distance_method\",^M
      agglomeration_method = \"$agglomeration_method\",^M
      column_clustering = $column_clustering,^M
      select = $select,^M
      plot_title = \"$plot_title\",^M
      xlab = \"$xlab\",^M
      ylab = \"$ylab\",^M
      width = $width, ^M
      height = $height,^M
      ppi = $ppi
      na_encoding= $NA_code)\n";

print STDOUT "Envoi duscript R \n";


#Fermeture du pont
$R->stopR();

print STDOUT "Fermeture du pont R \n";

#Mise en forme d'un rapport HTML
open OUT,"> $working_dir/statistics_report.html"
	or die "Unable to create HTML output file : $!\n";
print OUT "<HTML><HEAD><TITLE>Descriptive statistics</TITLE></HEAD>\n";
print OUT "<BODY>\n";

#if (-f "$working_dir/$stat_out" ) {
if (-f "$working_dir/out.jpeg" ) {
	print OUT "<H1>Hierarchical clustering :</H1>\n";
	my $file_url = "$working_dir/out.jpeg";
        my $tar = "$file_url.zip";
        `cp $file_url $tar`;
	$file_url =~s/^$file_path/\/galaxy\/download/;
        print OUT "<A HREF=\"$file_url\"><IMG  height=\"700\" width=\"700\" src=\"$file_url\"></A>";
        print OUT "<BR /><BR /><BR />";
        $tar =~s/^$file_path/\/galaxy\/download/;
        print OUT "<A HREF=\"$file_url\">Download here your hierarchical clustering map.</A>";
        print OUT "</HTML>\n";
        print OUT "</BODY>\n";
}

print STDOUT "Redaction du report html \n";

#Recuperation des outputs dans Galaxy
my $cmdhtml ='';
my $out= "$working_dir/statistics_report.html";
if (! -e $out){print STDERR "Pas de fichier html produit \n";}
else {$cmdhtml = "(mv $out $output_file) >& ./cp_html.log 2>&1";
system $cmdhtml;}

my $status =  WEXITSTATUS($?);    # état de sortie
if (! -f "$working_dir/out.jpeg" ) {    # vrai pour une fin non normale
        print STDERR "Etat du process : $status \n";
}
print STDOUT "Etat du process : $status \n";

