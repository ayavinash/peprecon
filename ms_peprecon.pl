### parser for mascot dat file
### This script is executed with a hyperlink provided by the patch script
### inside mascot server result loading script


use warnings;
use CGI qw(:standard);
use CGI::Carp(fatalsToBrowser);
use File::Basename; 	#Load Required modules
use MIME::Explode;
use URI::Escape;

my $query = new CGI;
    print $query->header( -charset => "utf-8" );
print $query->h1("---------------------------------------------------------------PEPRECON 2.0----------------------------------------------------------------"),hr;
my $DATFILE;
my $DATFILE=param('file'); #Defined only when form is submitted   
    print "Processing Data file------",$DATFILE,hr;
my $IMAGEDIR = "D:/inetpub/mascot/peprecon/";        #All Charts will be here
my $IMAGEURI = "/mascot/peprecon/";
my $TMPDIR  = "D:/inetpub/mascot/tmp/" . basename($DATFILE, '.dat') . ".split";  #Create temporary directory
my $CSV_DIR = "D:/inetpub/mascot/peprecon2/csv/". basename($DATFILE, '.dat') ."/";    #CSV direstories are file specific
my $CSV_FILE=$CSV_DIR."data.csv";
my $PNGFILE = basename($DATFILE, '.dat') . '.png';
my $PNGFILE_PATH = $IMAGEDIR . $PNGFILE;
my $EXP_TITLE;
my $EXP_DB;
my $EXP_TAXONOMY;
my $EXP_RELEASE;
my $exp=0.05;      # initialize default parameters
my $regions=-1;
my $grey_percent=-1;
my $mz_binsize=-1;
my $el_binsize=-1;
my $fine=1;

if (param()) {      # Check if any parameters are passed
$exp= param('expectation');    # Subsitute default values with form values
$regions= param('regions');
$grey_percent= param('grey_percent');
$mz_binsize= param('mz_binsize');
$el_binsize= param('el_binsize');
    if (ok()) { 	#All parameters are legal and $fine is returned as 1
    check_CSV();}
    else { 			# If any illegal parameters are passed
    print h3("<B> <font color=#ff0000>##...INPUT ERROR...##.RESUBMIT ILLEGAL PARAMETERS.## DEFAULT IMAGE ON DISPLAY...</font></B>"), hr;
    check_CSV();}}
else {  			# This is the default situation
$PNGFILE = basename($DATFILE, '.dat')."e".$exp."r".$regions."g".$grey_percent."m".$mz_binsize."el" .$el_binsize .'.png';
        $PNGFILE_PATH = $IMAGEDIR . $PNGFILE;
    check_CSV();}

sub ok() {     # Should check if parameters are legal
    $fine=1;	# Value when all parameters are legal	
    if(!($exp)) { print '(1)..The Default Expectation filter value used is 0.05 as suggested by MASCOT.',br;
        $exp =0.05; }
    elsif (!(($exp >0) && ($exp< 1))) { print "<B> <font color=#ff0000>(1)..Bad input for expectation filter value!## ".$exp." ----!!Using Default value of 0.05.</font></B>",br;
                $exp=0.05;     # Replace exp with default if form paramter is illegal
                $fine=0;	}
    else { print "<B> <font color=#0000ff>(1)..User Defined Expectation filter is ## ".$exp."</font></B>",br;}
    if (!$regions) { print '(2)..The default scatterplot is not divided into any GPF regions.', br;
    $regions=-1;}
    elsif (!((($regions > 1) && ($regions<=20) && (int($regions)==$regions)) or ($regions==-1))) { print "<B> <font color=#ff0000>(2)..Bad input for No of regions value!##".$regions." ----!!Using undefined value of -1.</font></B> ", br;
                $regions=-1;    #Replace regions will default value if form parameter is illegal
            $fine=0;}
    else { print "<B> <font color=#0000ff>(2)..User Defined GPF Regions in scatterplot is ## ".$regions. "</font></B>",br;}
    if (!$grey_percent) { print '(3)..The default plot will not take any grey-threshold value for peptides in grey region.',br;
        $grey_percent=-1;}
    elsif (!((($grey_percent>0) && ($grey_percent <=100)) or ($grey_percent==-1))) {print "<B> <font color=#ff0000>(3)..Bad input for % grey-threshold margin value!##".$grey_percent." ----!!Using undefined value of -1.</font></B>", br;
                $grey_percent=-1;
                $fine=0;}
    else { print "<B> <font color=#0000ff>(3)..User Defined % grey-threshold below Mascot Score Cutoff is ## ".$grey_percent."</font></B>",br;}
    if (!$mz_binsize) {print '(4)..Using default settings for plotting m/z Vs Peak Counts Plot.',br;
        $mz_binsize=-1;	}
    elsif (!((($mz_binsize>0) and ($mz_binsize<=100)) or ($mz_binsize==-1))) {print "<B> <font color=#ff0000>(4)..Bad input for m/z window size value!##".$mz_binsize." ----!! Using undefined value of -1.</font></B>", br;
                $mz_binsize=-1;
                $fine=0;}
    else { print "<B> <font color=#0000ff>(4)..User Defined m/z window size is ## ".$mz_binsize."</font></B>",br;}	
    if (!$el_binsize) {print '(5)..Using default settings for plotting Elution-time Vs Peak Counts Plot.',br;
        $el_binsize=-1;	}
    elsif (!((($el_binsize>0) and($el_binsize<=100))  or ($el_binsize==-1))) {print "<B> <font color=#ff0000>(5)..Bad input for Elution-time window size value!##".$el_binsize." ----!!Using undefined value of -1.</font></B>", br;
                $el_binsize=-1;
                $fine=0;}
    else { print "<B> <font color=#0000ff>(5)..User Defined Elution-time window size is ## ".$el_binsize."</font></B>",br;}
    $PNGFILE = basename($DATFILE, '.dat')."e".$exp."r".$regions."g".$grey_percent."m".$mz_binsize."el".$el_binsize.'.png'; #Unique filename
        $PNGFILE_PATH = $IMAGEDIR . $PNGFILE;	
    print hr;
    return $fine;}

sub check_CSV() {       ###Check if CSV exists, png file exists and display form
    if (-e $CSV_FILE) {       # Check if CSV exists
        if (-e $PNGFILE_PATH) {  #  Check if png file exists
            myform();}	   # Call the form ,situation when the search is revisited
    create_chart();
    myform();}
else  {					##Make a new CSV, Chart and display form
    mkdir $CSV_DIR;
    create_CSV();
    create_chart();
    myform();}}

sub myform() { print start_form, h2('-------------------------------------------------------------------Form  Parameters-----------------------------------------------------------------------'),hr,
    '(1)......Expectation filter (0< E-Score <1)...........................: ', textfield('expectation',0.05,5), br,
    '(2)......No of GPF Regions in Scatter Plot (1< No <20)....................: ', textfield('regions',-1,5), br,
    '(3)......% Grey-threshold below Mascot score cutoff (0< % <100)..........: ', textfield('grey_percent',-1,5), br,
    '(4)......Window size for m/z vs Peak counts Plot (0< Da <100)..........: ', textfield('mz_binsize',-1,5), br,
    '(5)......Window size for Elution time/Scan number vs Peak counts Plot (0< min <100): ', textfield('el_binsize',-1,5), br,
            hidden('file',$DATFILE),br,  # same DAT file is processed
    submit(-value=>"Create Chart",-size=>"10"),hr,
    end_form;
print h2('-----------------------------------------------------------------------------PepRecon 2.0 Evaluation Chart-----------------------------------------------------------------------------'),hr;
print "<P align=\"center\"><IMG SRC=\"$IMAGEURI$PNGFILE\" WIDTH=1600 HEIGHT=1600 ALT=\"Chart\">\n",hr;
print '-------PNG file location-----',"$IMAGEDIR$PNGFILE",hr,;
print '###################################--------------------------------------------------DONE-------------------------------------------------------------------###################################',hr,;
exit;}      # Program should always end here.

### Split dat file
sub create_CSV() {
    mkdir $TMPDIR; #  To store all the section files fom DAT file
    my $explode = MIME::Explode->new(output_dir => $TMPDIR, decode_subject => 0,check_content_type => 0,);
    my $fd_DATFILE;
    open $fd_DATFILE, $DATFILE;
    $explode->parse($fd_DATFILE);
    close $fd_DATFILE;
    
### Initialize all values for CSV
    my $mascot_query = 0;
    my $mascot_score = 0;
    my $pep_mass = 0;
    my $pep_calc_mass = 0;
    my $pep_mass_diff = 0;
    my $pep_mz = 0;
    my $pep_charge = 0;
    my $pep_qmatch= 0;
    my $pep_qplughole= 0;
    my $pep_sequence = "";
    my $elution_start = 0;
    my $elution_end = 0;
    my $peaksfrom1 =0;
    my $peaksfrom2 =0;
    my $peaksfrom3 =0;
    my @proteins = ();
    my $ms_period = 0;
    my $ms_cycle_start = 0;
    my $ms_cycle_end = 0;
    my $ms_experiment = 0;

sub consume_mascot_peptide($) { #collect peptide information
    my $line = shift;
    my @data = split /,/, $line;
    $pep_calc_mass = $data[1];
    $pep_mass_diff = $data[2];
    $pep_sequence = $data[4];
    $mascot_score = $data[7];
    $peaksfrom1 =$data[5];           # Collect the peak counts
    $peaksfrom2 =$data[9];
    $peaksfrom3 =$data[10];}

sub consume_mascot_protein($) { #collect protein hits
    my $line = shift;
    my @data = split /:/, $line;
    my $protein = $data[0];
    $protein =~ s/"//g;
    push(@proteins, $protein);}

sub consume_mascot_protein_hits($) {
    my $line = shift;
    my @data = split /,/, $line;
    my $protein = "";
    @proteins = ();
    foreach $protein (@data) {
        consume_mascot_protein($protein);}}

sub consume_qmass($) { #collect experimental mass
    my $line = shift;
    my ($key, $value) = split /=/;
    $pep_mass = $value;}

sub consume_qexp($) {  #collect peptide charge
    my $line = shift;
    my ($key, $value) = split /=/;
    ($pep_mz, $pep_charge) = split /,/, $value;
    $pep_charge =~ s/\+//g;}

sub consume_qmatch($) { #collect Qmatch value for calculating identity threshold
    my $line = shift;
    my ($key, $value) = split /=/;
    $pep_qmatch= $value;}

sub consume_qplughole($) {   #collect Qplughole value
    my $line = shift;
    my ($key, $value) = split /=/;
    $pep_qplughole=$value;}

sub consume_title_qstar($) { #Catch elution info case 1
    my $line = shift;
    $line = uri_unescape($line);
    my ($key, $value) = split /=/, $line;
    my ($file, $sample, $elution, $period, $cycle) = split /, [A-Za-z\(\)]*: /, $value;
    $elution_start = $elution;
    $elution_start =~ s/^([0-9.]*).*/$1/;
    if ($elution =~ / to /) {
        $elution_end = $elution;
        $elution_end =~ s/.*to ([0-9.]*) min/$1/;
    } else {
        $elution_end = $elution_start;}
    $ms_period = $period;
    $ms_cycle_start = $cycle;
    $ms_cycle_start =~ s/([0-9]*).*/$1/;
    if ($cycle =~ /-/) {
        $ms_cycle_end = $cycle;
        $ms_cycle_end =~ s/.*-([0-9]*).*/$1/;
    } else {
        $ms_cycle_end = $ms_cycle_start;}
    $ms_experiment = $cycle;
    $ms_experiment =~ s/.*Experiment ([0-9]*).*/$1/;}

sub consume_title_esquire($) { #Catch elution info  case 2
    my $line = shift;
    $line = uri_unescape($line);
    my ($key, $value) = split /=/, $line;
    my ($cmpd, $msn, $elution) = split /, /, $value;
    $elution =~ s/ min//;
    $elution_start = $elution;
    $elution_end = $elution;
    $ms_period = "";
    $ms_cycle_start = "";
    $ms_cycle_end = "";
    $ms_experiment = "";}

sub reset_vars() {  #Reset all CSV values
    $mascot_query = 0;
        $mascot_score = -1;
        $pep_mass = 0;
        $pep_calc_mass = 0;
        $pep_mass_diff = 0;
        $pep_mz = 0;
        $pep_charge = 0;
        $pep_qmatch= 0;
        $pep_qplughole= 0;
        $pep_sequence = "";
        $elution_start = 0;
        $elution_end = 0;
        $peaksfrom1 =0;
        $peaksfrom2 =0;
        $peaksfrom3 =0;
        $ms_period = 0;
        $ms_cycle_start = 0;
        $ms_cycle_end = 0;
        $ms_experiment = 0;
        @proteins = ();}

my $PEPTIDES;
my $SUMMARY;
my $PEPTIDES_PATH = $TMPDIR . "/peptides";   #Path to peptides section file
my $SUMMARY_PATH = $TMPDIR . "/summary";	#Path to summary section file
open $PEPTIDES, '<', $PEPTIDES_PATH or debug("Cannot open file peptides");
open $SUMMARY, '<', $SUMMARY_PATH or debug("Cannot open file summary");
my $CSV_FILE = "D:/inetpub/mascot/peprecon2/csv/". basename($DATFILE,'.dat') . "/data.csv";
my $CSV;
open($CSV, '>', $CSV_FILE) or debug("Hmmm, cannot open file". "D:/inetpub/mascot/peprecon2/csv/". basename($DATFILE,'.dat') . "/data.csv");
info();    # Take all database info for writing into CSV
print $CSV "mascot_query,mascot_score,pep_mass,pep_mz,pep_charge,pep_qmatch,pep_qplughole,pep_calc_mass,pep_mass_diff,pep_sequence,elution_start,elution_end,peaksfrom1,peaksfrom2,peaksfrom3,ms_period,ms_cycle_start,ms_cycle_end,ms_experiment,pep_proteins,$EXP_TITLE,$EXP_DB,$EXP_TAXONOMY,$EXP_RELEASE\n";
while ( <$PEPTIDES> ) {
    s/\r//;
    s/\n//;
    reset_vars();   # Set all variables to initial values.
    my ($key, $value) = split /=/;
#    next if ($value eq "-1"); # Ignore unidentified spectra
    ## Only process the useful queries (spectra)
    $mascot_query = $key;
    $mascot_query =~ s/q([0-9]*)_p.*/$1/;  
    my $peptide_hit = $key;
    $peptide_hit =~ s/q[0-9]*_p([0-9]*).*/$1/;
    next if ($peptide_hit != 1);  #only use the first peptide assignment (highest score)
    next if ($key ne ("q" . $mascot_query . "_p1")); # Just match the first entry not any others
    if ( $value =~ /;/ ) {
        my ($peptide_info, $protein_hits) = split /;/, $value;
        consume_mascot_peptide($peptide_info);
        consume_mascot_protein_hits($protein_hits);}
    # Parse next few lines of the summary file
    while ( <$SUMMARY> ) {  # Loop through summary section file
        s/\r//g;
        s/\n//g;
        if (/^qmass$mascot_query/) {consume_qmass($_); next}
        if (/^qexp$mascot_query/) {consume_qexp($_); next}
        if (/^qmatch$mascot_query/) {consume_qmatch($_); next}
        if (/^qplughole$mascot_query/) {consume_qplughole($_);last}}
    
    ## Retrieve information from query file.
    my $QUERYFILE;
    open $QUERYFILE, "<", $TMPDIR . "/query" . $mascot_query;
    while (<$QUERYFILE>) {   #Loop through query section files
        s/\r//g;
        s/\n//g;
        if (/^title=/) {
        if (/min$/) {
            consume_title_esquire($_);  # Catch elution info case 2
        } else {
            consume_title_qstar($_);}   #Call elutio-info case 1
        last};}
    ## Produce csv line
    print $CSV $mascot_query . ",";
    print $CSV $mascot_score . ",";
    print $CSV $pep_mass . ",";
    print $CSV $pep_mz . ",";
    print $CSV $pep_charge . ",";
    print $CSV $pep_qmatch . ",";
    print $CSV $pep_qplughole . ",";
    print $CSV $pep_calc_mass . ",";
    print $CSV $pep_mass_diff . ",";
    print $CSV $pep_sequence . ",";
    print $CSV $elution_start . ",";
    print $CSV $elution_end . ",";
    print $CSV $peaksfrom1 . ",";
    print $CSV $peaksfrom2 . ",";
    print $CSV $peaksfrom3 . ",";
    print $CSV $ms_period, ",";
    print $CSV $ms_cycle_start, ",";
    print $CSV $ms_cycle_end, ",";
    print $CSV $ms_experiment, ",";
    print $CSV join(":", @proteins);
    print $CSV "\n";}

remove_temp();} 	# Remove temporary directory

sub info() {     
    sub consume_param_com($) {      # Catch title of search
        s/^COM=//;
    $EXP_TITLE = $_ . "  [" . basename($DATFILE) . "]";	}

    sub consume_param_db($) { # Catch database
    s/^DB=//;
    $EXP_DB = $_;}

    sub consume_param_taxonomy($) {  # Catch taxonomy
    s/^TAXONOMY=//;
    $EXP_TAXONOMY = $_;	}

    sub consume_param_release($) {  #Catch fasta file version
    s/^release=//;
    $EXP_RELEASE = $_;}
    my $PARAMFILE;
    open $PARAMFILE, "<", $TMPDIR . "/parameters";   
    while (<$PARAMFILE>) {      #Loop through parameter section file
    s/\r//g;
    s/\n//g;
    if (/^COM=/) {consume_param_com($_); next};
    if (/^DB=/) {consume_param_db($_); next};
    if (/^TAXONOMY=/) {consume_param_taxonomy($_); next};}
    close $PARAMFILE;

    my $HEADERFILE;     
    open $HEADERFILE, "<", $TMPDIR . "/header";
    while (<$HEADERFILE>) {   #Loop through header section file
    s/\r//g;
    s/\n//g;
    if (/^release=/) {consume_param_release($_); next};	}
    close $HEADERFILE;}

sub create_chart() { #write the infile for R
    $|=1;
    my $R_FILE = "D:/inetpub/mascot/peprecon2/csv/".basename($DATFILE, '.dat')."/plot.R";
    my $R;
    open($R, ">", $R_FILE) or die $!;
    print $R 'source("D:/inetpub/mascot/bin/ms_compositeplot.R")' . "\n"; #Path to source R script
    print $R 'csvfile <- "' . $CSV_FILE . "\"\n";
    print $R 'pngfile <- "' . $PNGFILE_PATH . "\"\n";
    print $R 'exp <- "' . $exp . "\"\n";
    print $R 'regions <- "' . $regions . "\"\n";
    print $R 'grey_percent <- "' . $grey_percent . "\"\n";
    print $R 'mz.binsize <- "' . $mz_binsize . "\"\n";
    print $R 'el.binsize <- "' . $el_binsize . "\"\n";
    print $R 'do_composite_plot(csvfile, pngfile,exp,regions,grey_percent,mz.binsize,el.binsize)' . "\n";
    close($R);
`C:/Progra~1/R/R-2.11.0-x64/bin/R CMD BATCH $R_FILE`;}   #Execute R and feed the infile

sub remove_temp() {   # Remove all splitted files
    my $fd_DIR;
    opendir $fd_DIR, $TMPDIR;
    my @files = readdir $fd_DIR;
    closedir $fd_DIR;
    foreach (@files) {
        my $file = $_;
        unlink($TMPDIR . "/" . $file);}
    rmdir($TMPDIR);}
sub debug($) {
    my $msg = shift;
    print header;
    print "ERROR: " . $msg . "<br>\n";
    exit;}
### Remove CSV and Images If anything goes wrong ####
print " Sorry ! Something went wrong!! ";
unlink($CSV_DIR."/data.csv");
rmdir($CSV_DIR);
my $fd_DIR;
opendir $fd_DIR, $IMAGEDIR;
my @files = readdir $fd_DIR;
closedir $fd_DIR;
foreach (@files) {
    my $file = $_;
    unlink($IMAGEDIR . "/" . $file);}
rmdir($TMPDIR);
exit;
