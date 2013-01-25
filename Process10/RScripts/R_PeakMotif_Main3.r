###This subscript deals with the samplesheet and processing Bamfiles and finding predicted fragment lengths.

##Get Arguments
Args <- commandArgs(trailingOnly = TRUE)

## Get the Working directory from the supplied argument
WkgDir <- getwd()
source("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/Workflow_Functions3.r")
#
#WkgDir <- Args[2]
#JobString <- "toysd2"
JobString <- Args[1]
PeakCaller <- Args[3]
## Parse from config important locations
#PeakCaller <- "Macs"

PipeLineLocations <- GetImportantLocations(WkgDir,"Config")

SampleSheet <- read.delim("SampleSheet.csv",sep=",",header=T)

SampleSheet2 <- RunPeakMotifingPipeline(SampleSheet,WkgDir,JobString,MaxJobs=75,PipeLineLocations,"Config",Caller=PeakCaller,NPeaks=1000)

#write.table(SampleSheet,"SampleSheet.csv",sep=",",row.names=F,quote=F)

write.table("Complete",file.path(PipeLineLocations@WorkFlowDir,paste(JobString,"_",PeakCaller,"_PeakMotif_Main_3.txt",sep="")),col.names=T,row.names=F,sep=",",quote=F)
