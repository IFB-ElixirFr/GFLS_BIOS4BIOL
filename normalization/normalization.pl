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
  # - count.file : input count file name
  # - group.member.file : input sapmle/tag group member file name
  # - output.file : output file name
  # - formatformat.image.out : graphic format of the output file. This must be one of "png", "jpeg", "tiff", "pdf"
  # - transformation.method : count data transformation for graphical display. This must be one of "none", "rld", "vsd"
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

#Parametres d'entree
my $input_file               =$ARGV[0]; #input file name
my $transformation_method    =$ARGV[1]; #transformation method. This must be one of "log", "DESeq2", "Rlog", "Standard_score", "TSS", "TSS_CLR"
my $na_encoding              =$ARGV[2]; #string used to indicate NA values
my $output_file              =$ARGV[3]; #output file produced after transformation
my $log_file                 =$ARGV[4]; #output file containing messages for user if something bad happens
my $variable_in_line         =$ARGV[5]; #boolean flag (0/1) indicating if variables are in lin or in columns

#ligne de commande test :
#perl sm_hclustfun.pl Data/gene_counts.txt Data/member.csv outfile none  euclidean  ward TRUE 100 titre x y 7 7 300

# SETTING GLOBAL VARIABLES ################################################################################
my $cfg = GalaxyPath->new( -file => $ENV{"GALAXY_CONFIG_FILE"} );
my $file_path = $cfg->my_path( 'workPath', 'MYWORKSPACE' );
$r_bin = $cfg->my_path( 'toolsPath', 'R_BIN_PATH' );
my $SCRIPTS_R_path = $cfg->my_path( 'workPath', 'SCRIPTS_R_path' );


#Récuperer le numero (unique) de l'output
my ($nb) = ($output_file=~/galaxy_dataset_(\d+)\.\S+$/);

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
open IN,"< $SCRIPTS_R_path/normalization.R"
	or die "Unable to read R script :$SCRIPTS_R_path  $!\n";
while ($line=<IN>) {
	$cmd.=$line;
}
close(IN);

#Declare R function
#$R->send($cmd);

print STDOUT "Declaration du script R \n";
    
$R->send(
     "setwd('$working_dir')\n".
     "$cmd\n".
     "normalization(".
     "transformation_method= '$transformation_method', ".
     "na_encoding= '$na_encoding', ".
     "input_file= '$input_file', ".
     "output_file= '$output_file', ".
     "log_file= '$log_file', ".
     "variable_in_line= $variable_in_line)\n");

print STDOUT "\n normalization(

     transformation_method = \"$transformation_method\",
     na_encoding= \"$na_encoding\",
     input_file= \"$input_file\",
     output_file= \"$output_file\",
     log_file= \"$log_file\",
     variable_in_line= $variable_in_line)\n";

print STDOUT "Envoi du script R \n";

my $status = WEXITSTATUS($?);    # état de sortie
if ( -z $output_file ) {    # vrai pour une fin non normale
        print STDERR "Etat du process : $status \n";
}
print STDOUT "Etat du process : $status \n";


#Fermeture du pont
$R->stopR();

print STDOUT "Fermeture du pont R \n";
