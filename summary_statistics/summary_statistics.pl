#declaration de l espace de travail et du chemin de l executable R :
use strict;
use Carp;
use Statistics::R;
use Cwd qw(abs_path);
use File::Basename;
use Getopt::Long;

#Variables Globales ###########################
defined $ENV{'MY_GALAXY_DIR'} || die "MY_GALAXY_DIR environment variable not defined";
use lib "$ENV{'MY_GALAXY_DIR'}";
use GalaxyPath;

my ($working_dir,$script_dir,$r_bin,$log_dir,$R,$cmd,$line,$stat_out);

###############
#if $debug set to 1 works locally and produces R output in a ./tmp diectory
my $debug=0;
###############


#pour voir ce que le xml renvoie au wrapper perl. Sachant que $0 est la ligne de commande et @ARGV les arguments
#printf STDERR "%s\n",join("***** ",$0,@ARGV);
   
           
#Parametres d'entree
my $file_in; 	 #fichier de donnees en entree                        #--filein $file_in 
my $NA_code; 	 #ex : "NA" Comment specifier ""                      #--NAcode $NA_code  
my $stat;     	 #Valeurs possibles T or F                            #--statcondstat $stat_cond.stat
my $stat_chosen; #ex : "mean,sd,variance,median,quartile,decile"      #--statcondstatchosen $stat_cond.stat_chosen (IF)
my $ploting;     #Valeurs possibles T or F                            #--plotingcondploting  $ploting_cond.ploting 
my $plot_chosen; #ex : "boxplot,histogram,density,pairsplot,MAplot"   #--statcondplotchosen  $stat_cond.plot_chosen (IF)
my $outputfile;                                                       #--outputfile $outputfile 
my $logfile;                                                          #--logfile $logfile

Getopt::Long::Configure( 'no_ignorecase', 'bundling' );
GetOptions (
'filein=s' => \$file_in,
'NAcode=s' => \$NA_code,  
'statcondstat=s' => \$stat,
'statcondstatchosen=s' => \$stat_chosen,
'plotingcondploting=s' => \$ploting, 
'plotingcondplotchosen=s' => \$plot_chosen,
'outputfile=s' => \$outputfile,
'logfile=s' => \$logfile
) or die "Usage: Error in command line arguments\n";

print STDOUT "Input file : $file_in \n
NAcode : $NA_code\n
stat cond : $stat \n
stat chosen : $stat_chosen \n
ploting cond $ploting \n
plot chosen : $plot_chosen \n
outputfile : $outputfile  \n
logfile : $logfile  \n
";

# SETTING GLOBAL VARIABLES ################################################################################
my $cfg = GalaxyPath->new( -file => $ENV{"GALAXY_CONFIG_FILE"} );
my $file_path = $cfg->my_path( 'workPath', 'MYWORKSPACE' );
$r_bin = $cfg->my_path( 'toolsPath', 'R_BIN_PATH' );
my $SCRIPTS_R_path = $cfg->my_path( 'workPath', 'SCRIPTS_R_path' );




#Récuperer le numero (unique) de l'output
my ($nb) = ($outputfile=~/galaxy_dataset_(\d+)\.\S+$/);

#On suppose que le fichier de donnees en entree a ete place dans un repertoire
#de travail working_dir
## smaman : $working_dir=abs_path($file_in);
#Repertoire de sortie cree par le script, verif des droits d'ecriture sur ce repertoire de sortie
if ($debug==0) {
	`cd $file_path; mkdir $nb; chmod 777 $nb/; cd $nb/;`;
	$working_dir= "$file_path/$nb";
	$script_dir= $SCRIPTS_R_path;
} else {
	my $local_dir=`pwd`;
	chomp($local_dir);
	$working_dir= "$local_dir/tmp";
	$script_dir= "$local_dir";
}
#$working_dir=~s/\/[^\/]*$//;
#$file_in=~s/^.*\///;


#Recuperation de la matrice entrante dans le repertoir de travail
`cp $file_in $working_dir/infile`;

#Fichier de sortie des donnees de description statistiques
#smaman
#$stat_out=$file_in;
#$stat_out=~s/\.([^.]*)$/_statistics.$1/;
#smaman
$stat_out = "$working_dir/stat_out";

print STDOUT "Output file : $stat_out \nWorking dir : $working_dir \n ";

# Rbin path
#$r_bin   = '/usr/local/bioinfo/src/R/current/bin/R';
#Log dir
$log_dir = $working_dir;

#déaration de l'objet $R et ouverture du pont :
$R = Statistics::R->new(
    "r_bin"   => $r_bin,
    "log_dir" => $log_dir,
) or die "Problem with R : $!\n";
# Ouverture du pont
$R->startR;

#envoi du repertoire de travail
$R->send(qq`setwd("$working_dir")`); 
print STDOUT "1 - envoi du repertoire de travail\n";

#Read R script
$cmd="";
open IN,"< $script_dir/summary_statistics.R"
	or die "Unable to read R script : $!\n";
while ($line=<IN>) {
	$cmd.=$line;
}
close(IN);
print STDOUT "2 - Read summary_statistics.R\n";

#Declare R function
#$R->send($cmd);
print STDOUT "3- Apres l appel de  summary_statistics.R\n";

print STDOUT "APPEL R : desc_fct\n";

my $NA_code2 = " ";
if ($NA_code eq ""){$NA_code = $NA_code2}else {$NA_code = $NA_code};

			 
$R->send("$cmd\n".
	 "desc_fct(file.in='$working_dir/infile',".
         "         nacode='$NA_code',stat.out='$working_dir/stat_out',stat=$stat, ".
         "         chosen.stat='$stat_chosen',ploting=$ploting,chosen.plot='$plot_chosen',log_file='$logfile')");





#Fermeture du pont
$R->stopR();

print STDOUT "4 - Fermeture du pont R\n";


#Mise en forme d'un rapport HTML
open OUT,"> $working_dir/statistics_report.html"
	or die "Unable to create HTML output file : $!\n";
print OUT "<HTML><HEAD><TITLE>Descriptive statistics</TITLE></HEAD>\n";
print OUT "<BODY>\n";

#if (-f "$working_dir/$stat_out" ) {
if (-f "$working_dir/stat_out" ) {
	print OUT "<H1>Statistical measures :</H1>\n";
	print OUT "<TABLE border=1>\n";
#	open IN,"< $working_dir/$stat_out"
	open IN,"< $working_dir/stat_out"
#		or die "Unable to open $working_dir/$stat_out : $!\n";
		or die "Unable to open $working_dir/stat_out : $!\n";
	my $noLine=0;
	while ($line=<IN>) {
		chomp($line);
		$noLine++;
		if ($noLine==1) {
			$line=~s/\t/<\/TH><TH>/g;
			print OUT "	<TR><TH>$line</TH></TR>\n";
			next;
		}
		$line=~s/\t/<\/TD><TD>/g;
		$line=~s/<\/TD>/<\/TH>/; #pour la premiere colonne
		print OUT "	<TR><TH>$line</TD></TR>\n";
	}
	#print OUT "</TABLE><BR><A HREF=\"./$stat_out\">Download table</A><P>\n";
	my ($file,$file2);
	$file ="$working_dir/stat_out";
	if ($debug==0) {
		$file2 = "$file_path/$nb.stat_out" ;
		`cp $file $file2;`;
		$file2 =~s/^$file_path/\/galaxy\/download/;
	} else {
		$file2=$file;
	}
	#print OUT "</TABLE><BR><A HREF=\"./$file\">Download table</A><P>\n";
	print OUT "</TABLE><BR><A HREF=\"$file2\">Download table</A><P>\n";
}


my ($idxFile,$graphicType,$i,$label,$file,$graphicTypeList,$graphicList);
my (@files);
my (%treated);
@files=glob("$working_dir/*png");
if ($#files!=-1) {
	$graphicList=join " ",@files;
	$graphicList=~s/ [^ ]*\// /g;
	$graphicList=~s/^[^ ]*\///g;
	system("cd $working_dir; tar czf all_graphics.tgz $graphicList");
	
	for ($idxFile=0;$idxFile<=$#files;$idxFile++) {
		if ($idxFile==0) {
			print OUT "<H1>Statistical graphics :</H1>\n";
		}
		if (defined $treated{$files[$idxFile]}) {
			next;
		}
		$graphicType=$files[$idxFile];
		$graphicType=~s/^.*\///;
		$graphicType=~s/[.]png$//;
		$graphicType=~s/_.*$//;
	
		$graphicTypeList="";
	
		$i=$idxFile;
		#smaman
		#my $graph = "${graphicType}_graphics.tgz";
		my $graph = "${graphicType}_graphics.tgz";
	#	print OUT "1 : $graph";
		
		my ($graph_url,$graph_url2);
		$graph_url = "$working_dir/$graph";
		if ($debug==0) {
			$graph_url2 = "$file_path/$nb.$graph" ;
			`ln -s $graph_url $graph_url2;`;
			$graph_url2 =~s/^$file_path/\/galaxy\/download/;
		} else {
			$graph_url2=$graph_url;
		}
	#    print OUT "cp $graph_url $graph_url2;";
		print OUT "<TABLE><TR><TH valign=top><A HREF=\"$graph_url2\">$graphicType</A></TH>";
		while ($files[$i]=~/\/${graphicType}/) {
			$treated{$files[$i]}=1;
			$file=$files[$i];
			$file=~s/^.*\///;
			if ($graphicTypeList ne "") {$graphicTypeList.=" ";}
			$graphicTypeList.=$file;
			my ($file_url,$file_url2);
			$file_url = "$working_dir/$file";
			if ($debug==0) {
				$file_url2 = "$file_path/$nb.$file" ;
				`cp $file_url $file_url2;`;
				$file_url2 =~s/^$file_path/\/galaxy\/download/;
			} else {
				$file_url2=$file_url;
			}
			print OUT "<TD><A HREF=\"$file_url2\"><IMG src=\"$file_url2\"></A></TD>";
			$i++;
			if ($i>$#files) {last;}
		}
		print OUT "</TR></TABLE>\n";
	
		system("cd $working_dir; tar czf ${graphicType}_graphics.tgz $graphicTypeList");
	}
	#smaman
	my ($graphs_tar, $graphs_tar2);
	$graphs_tar = "$working_dir/all_graphics.tgz";
	if ($debug==0) {
		$graphs_tar2 =  "$file_path/$nb.all_graphics.tgz" ;
		`cp $graphs_tar $graphs_tar2;`;
		$graphs_tar2 =~s/^$file_path/\/galaxy\/download/;
	} else {
		$graphs_tar2=$graphs_tar;
	}
	#print OUT "<A HREF=\"./all_graphics.tgz\">Download all graphics</A>\n";
	print OUT "<A HREF=\"$graphs_tar2\">Download all graphics</A>\n";
	print OUT "</BODY></HTML>";
	close(OUT);
}


print STDOUT "5 - Fin de la ise en forme du rapport HTML\n";

#Recuperation des outputs dans Galaxy
my $cmdhtml ='';
my $out= "$working_dir/statistics_report.html";
if (! -e $out){print STDERR "Pas de fichier html produit \n";}
else {$cmdhtml = "(mv $out $outputfile) >& ./cp_html.log 2>&1";
system $cmdhtml;}
