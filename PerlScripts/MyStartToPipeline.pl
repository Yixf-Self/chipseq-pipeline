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

die "Can not find limms API script\n" unless -f "/home/mib-cri/bin/get_lanes_for_chIPseq_project.pl"; # Check is the perl script is there
print  "     Getting information from LIMMS..."; 
`/home/mib-cri/bin/get_lanes_for_chIPseq_project.pl \$(basename \$(pwd)) >Lims_Info.txt`;  
print  "DONE\n";

