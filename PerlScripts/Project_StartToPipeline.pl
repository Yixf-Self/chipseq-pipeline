#!/usr/bin/perl


##Perl Modules Used##
use strict;
use warnings;
use File::Temp;
use Getopt::Long;
use File::Basename;
use Pod::Usage;
use File::Copy;
#####################

#Needed for querying the limms
$ENV{'PERL5LIB'} = '/home/mib-cri/svn_checkouts/solexa/solexa-limsapi/modules/:/home/mib-cri/svn_checkouts/solexa/core-utils/modules/:$PERL5LIB';
##########
use strict;
use Getopt::Long;

use CRI::Solexa::Utils::General;

my $dburi = "http:::uk-cri-ldmz02.crnet.org:80:/solexa-ws/SolexaExportBeanWS";

&GetOptions('trackingdb=s' => \$dburi);

my $db = connect_to_database(split(/:/, $dburi));


my $TempDir = $ARGV[0];
#print $dir;
#my $FileOut=join($dir."ActualLocations.txt");
open OUT, ">".$TempDir."/Projects_LimmsInfo.txt";
if (-e $TempDir."/Projects.txt"){
open IN,"<".$TempDir."/Projects.txt";
my $proj;
foreach my $proj (<IN>) {
  chomp($proj);
  my $lanes = $db->get_SampleLaneAdaptor->fetch_all_by_annotation('Project', $proj);
  foreach my $lane (@$lanes) {
    my ($dir) = @{$db->get_FileLocationAdaptor->fetch_all_by_Run($lane->run, 'SOLEXA_RUN_ROOT', 'FILE')};
    next unless $lane->run->run_status eq "COMPLETE";
    printf OUT ("%s.%d.s_%d\t%s\tRun-%d\tLane-%d\t%s:%s/full_Data_CRI/%s.%d.s_%d\n",
           $lane->internal_sample_name,
           $lane->run->run_number,
           $lane->lane_number,
           $lane->user_sample_name,
           $lane->run->run_number,
           $lane->lane_number,
           $dir->host,
           $dir->directory,
           $lane->internal_sample_name,
           $lane->run->run_number,
           $lane->lane_number
          );
    #    print OUT $lane->run->run_status;print "\n";
  }
}
}
