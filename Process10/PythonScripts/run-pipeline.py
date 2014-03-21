#!/usr/bin/env python
# encoding: utf-8
"""
run-pipeline.py

Created by Anne Pajon on 2014-03-21 
to replace Kick.r based on ChIPSeqPipeline.py.
"""

################################################################################
# IMPORTS
################################################################################
import sys
import os
import argparse
import ConfigParser

################################################################################
# MAIN
################################################################################

def main():
    # get the options
    parser = argparse.ArgumentParser(description='ChIP-Seq Analysis Pipeline Written by Tom Carroll, Suraj Menon and Rory Stark CRUK-CI 2012')
    #prog='ChIPSeqPipeline',
	#formatter_class=argparser.ArgumentDefaultsHelpFormatter,
	#description='\nChIP-Seq Analysis Pipeline\nWritten by Tom Carroll,Suraj Menon and Rory Stark\nCRUK, CRI\n2012\n')
	
    #parser.add_argument("--basedir", dest="basedir", action="store", help="lustre base directory e.g. '/lustre/mib-cri/solexa/Runs'", required=True)
    
    config_file = os.path.join(os.path.dirname(__file__), '..', 'Config', 'config.ini')
    config_parser = ConfigParser.SafeConfigParser()
    
    parser.add_argument("-c","--config", dest="config", action="store", help="The pipeline config file", default=config_file)
    parser.add_argument("-g","--genome", dest="genome", action="store", help="The desired genome to be used", required=True)

    parser.add_argument("--workingDirectory", dest="working_dir", action="store", help="Directory for project")
    parser.add_argument("--bamDirectory", dest="bam_dir", action="store", help="Directory for bam files")
    parser.add_argument("--fastqDirectory", dest="fastq_dir", action="store", help="Directory for fastq files")

    parser.add_argument("--addSLXIDs", dest="slxids", action="store", help="SLXID/s to be added to the current project")
    parser.add_argument("--addSampleNames", dest="samples", action="store", help="???")
    parser.add_argument("--addProjects", dest="projects", action="store", help="Project/s to be merged with the current project")

    parser.add_argument("--callMacsAll", dest="macs_all", action="store", help="Call, QC and find motifs in Macs Peaks")
    parser.add_argument("--callMacsPeaks", dest="macs_peaks", action="store", help="Call Macs Peaks")
    parser.add_argument("--callMacsMotifs", dest="macs_motifs", action="store", help="Call Macs Motif")
    parser.add_argument("--callMacsPeakProfile", dest="macs_peakprofile", action="store", help="Perform QC on Macs Peaks")
    parser.add_argument("--callMacsBetweenPeaks", dest="macs_betweenpeaks", action="store", help="Perform QC between Macs Peaks")

    parser.add_argument("--callSicerAll", dest="sicer_all", action="store", help="Call, QC and find motifs in Sicer Peaks")	
    parser.add_argument("--callSicerPeaks", dest="sicer_peaks", action="store", help="Call Sicer Peaks")
    parser.add_argument("--callSicerMotifs", dest="sicer_motifs", action="store", help="Call Sicer Motif")
    parser.add_argument("--callSicerPeakProfile", dest="sicer_peakprofile", action="store", help="Perform QC on Sicer Peaks")
    parser.add_argument("--callSicerBetweenPeaks", dest="sicer_betweenpeaks", action="store", help="Perform QC between Sicer Peaks")

    parser.add_argument("--callTPICsAll", dest="tpi_all", action="store", help="Call, QC and find motifs in TPICs Peaks")	
    parser.add_argument("--callTPICsPeaks", dest="tpi_peaks", action="store", help="Call TPICs Peaks")
    parser.add_argument("--callTPICsMotifs", dest="tpi_motifs", action="store", help="Call TPICs Motif")
    parser.add_argument("--callTPICsPeakProfile", dest="tpi_peakprofile", action="store", help="Perform QC on TPICs Peaks")
    parser.add_argument("--callTPICsBetweenPeaks", dest="tpi_betweenpeaks", action="store", help="Perform QC between TPICs Peaks")

    parser.add_argument("--AutoMerging", dest="auto_merging", action="store", help="Merge samples by samplename")	
    parser.add_argument("--JustDummy", dest="just_dummy", action="store", help="Makes dummy samplesheet, directory structure and config")

    options = parser.parse_args()
    
    # read config file
    if os.path.exists(options.config):
        config.read(options.config)
        inifile = open(options.config, 'w')
        # do modify config file based on command line arguments
        config.write(inifile)
        inifile.close()

if __name__ == '__main__':
    main()

