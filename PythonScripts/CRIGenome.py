import pysam
import re
import sys
import Pyrex
from datetime import *
import os
from operator import itemgetter


def GetHeaderInfo(BamFile):
	samfile = pysam.Samfile(BamFile,"rb")
	header = samfile.header
	HeaderTags = header.keys()
	if "HD" in HeaderTags:
		HDs = header["HD"]
		HDSub = HDs.keys()
		if "SO" in HDSub:
			SortOrder = HDs["SO"]
		else:
			SortOrder = "unknown"
		if "VN"	in HDSub:
			BamVersionNumber = HDs["VN"]	
		else:
			BamVersionNumber = "1.0"
	else:
		SortOrder = "unknown"
		BamVersionNumber = "1.0"
	if "PG" in HeaderTags:
		PGs = header["PG"]
	else:
		PGs = [{'ID': 'Unknown Aligner', 'VN': 'NA'}]
	if "SQ" in HeaderTags:
		SQs = header["SQ"]
	if "RG" in HeaderTags:
		RGs = header["RG"]
	else:
		RGs = []
	if "CO" in HeaderTags:
		COs = header["CO"]
	else:
		COs = []
	HeaderInfo = {"SortOrder":SortOrder,"SamVersion":BamVersionNumber,"ProgramInfo":PGs,"Chromosomes":SQs,"ReadGroups":RGs,"Comments":COs}
	return HeaderInfo


def GetGenome(BamFile):
	genome = "HG18"
	TempHead = GetHeaderInfo(BamFile)
	TempComments = TempHead["Comments"]
	for comments in TempComments:
		if comments.split(":")[0] == "GenomeVersion":
			genome = comments.split(":")[1]
	return(genome)

def GetDirGetName(BamFile):
	fileSimple = re.split(".bam",re.split("/.*/",BamFile)[1])[0]
	dir = re.findall("/.*/",BamFile)[0]
	return [dir,fileSimple]



BamFile = sys.argv[1]
TargetGenome = sys.argv[2]

TempNames = GetDirGetName(BamFile)

genome = GetGenome(BamFile)
print(genome)
if genome != TargetGenome:
	ReAlignLog = open(TempNames[0]+TempNames[1]+".AlignMe","a")
	ReAlignLog.write("\n")
	ReAlignLog.close()



