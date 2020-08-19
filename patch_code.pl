######### PepRecon2.0 patch start #############

print '<H3>PepRecon 2.0 evaluation tool</H3>';
my $DATDIR='D:/inetpub/mascot/data/';     #DAT files folder
my $DATFILE;
open($DATFILE, $DATDIR . $fileIn)      # DAT file handle   
or die 'Something bad happened...';
my $inQuery = 0;
my $hasElutionInfo = 0;
while (<$DATFILE>) {	# Loop through DAT file		
    if ($_ =~ /^Content-Type\: application\/x-Mascot; name=\"query1\"/) {
    $inQuery = 1;     # DAT file has query1
    }
    if ($_ =~ /^title=.*Elution.*Period.*/) {     # Check for elution-time case 1
    $hasElutionInfo = 1;
    last;
    }
    if ($_ =~ /^title=.*min$/) {   # Check for elution-time case 2
    $hasElutionInfo = 1;
    last;
    }
    if ($_ =~ /^Content-Type: application\/x-Mascot; name=\"query[0-9]*\"/) {  
    $inQuery = 0;    
    }
}
if ($hasElutionInfo) {   # Hyper-link for PepRecon
        print "<P><A HREF=\"ms_peprecon.pl?file=$fileIn\" TARGET=\"_blank\" >Start PepRecon 2.0</A>";
    }
    else  {   # No Hyper-link
    print "Mascot results (DAT-file) don't contain any information on Elution-time.";
}
######### PepRecon2.0 patch end #############  

