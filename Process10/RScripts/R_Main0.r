###This subscript deals with the samplesheet and finding locations.

##Get Arguments
Args <- commandArgs(trailingOnly = TRUE)

## Get the Working directory from the supplied argument
WkgDir <- getwd()
#WkgDir <- Args[2]
getRandString<-function(len=12) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse=''))
JobString <- getRandString()
#JobString <- Args[3]
source("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/Workflow_Functions3.r")
## Parse from config important locations

PipeLineLocations <- GetImportantLocations(WkgDir,"Config")


## Create the directory structure based on config
CreateDirStruct(WkgDir,"Config",PipeLineLocations)

## Make sure samplesheet is unlocked before starting.
UnlockSampleSheet(WkgDir)

RunMainPipeline(WkgDir,JobString,MaxJobs=75,PipeLineLocations,"Config")

write.table("Complete",file.path(PipeLineLocations@WorkFlowDir,paste(JobString,"_Main_0.txt",sep="")),col.names=T,row.names=F,sep=",",quote=F)

