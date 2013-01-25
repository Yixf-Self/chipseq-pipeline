#!/usr/bin/perl -w

$ENV{'PERL5LIB'} = '/home/mib-cri/svn_checkouts/solexa/solexa-limsapi/modules/:/home/mib-cri/svn_checkouts/solexa/core-utils/modules/:/home/mib-cri/svn_checkouts/cri-lims/cri-lims-business/perl-soap-clients/:$PERL5LIB';
use strict;
use CRI::FileLocation;
use CRI::SOAP;
use constant {TRUE => 1, FALSE => 0};
use File::Basename; 
use Cwd;
use File::Spec;

my ($exportService);
my $host = 'uk-cri-ldmz02.crnet.org';
my $port = 80;
 
 
eval {
                #CRI::SOAP->debug();
                $exportService = CRI::SOAP->new('http', $host, $port, '/solexa-ws/SolexaExportBeanWS', 'http://solexa.webservice.melab.cruk.org/');
};
if($@) {
                #print $@;
}



my $TempDir = $ARGV[0];
#print $dir;
#my $FileOut=join($dir."ActualLocations.txt");
open OUT, ">".$TempDir."/SLXIDs_ActualLocations.txt";
if (-e $TempDir."/Samples_SLXIDs.txt"){
open IN,"<".$TempDir."/Samples_SLXIDs.txt";
my $line;
foreach $line (<IN>){
chomp($line);
#print $line;
my $lanes = $exportService->call('getAllSampleLanesForId',$line);
#print OUT " ID => $line\n";
foreach my $lane (@$lanes) {
		#print $lane;
                my $locations = $exportService->call('getFileLocations', $lane->{'sampleProcess_id'}, 'HTTP', 'BAM');
		foreach my $location (@$locations) {
                                printHash($location);
                }

}
#close OUT; 
 
sub printHash {
                my ($hash) = @_;
                while ( (my $k,my $v) = each %$hash) {
                                print OUT " $k => $v\n";
					#print  " $k => $v\n";
                }
                print OUT "\n";
}
}
close IN;
}
close OUT;

 