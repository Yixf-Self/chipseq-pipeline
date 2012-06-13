getRandString<-function(len=12) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse=''))
Args <- commandArgs(trailingOnly = TRUE)
library(raster)


ConfigFile <- readIniFile(file.path(getwd(),"Temp","config.ini"))


LocationsDir <- ConfigFile[ConfigFile[,2] %in% "tempdirectory",3]
WkgDir<- ConfigFile[ConfigFile[,2] %in% "workingdirectory",3]
BamDir<- ConfigFile[ConfigFile[,2] %in% "bamdirectory",3]
FQDir<- ConfigFile[ConfigFile[,2] %in% "fastqdirectory",3]
Genome <- ConfigFile[ConfigFile[,2] %in% "genome",3]
GenomeFileOptions <-  ConfigFile[ConfigFile[,1] %in% "Genomes",]
GenomeFile <- GenomeFileOptions[GenomeFileOptions[,2] %in% tolower(Genome),3]

sampleSheet <- read.delim(file.path(WkgDir,"SampleSheet.csv"),sep=",")
SamplesAndInputs <- matrix(data=c(gsub("_Processed\\.bam","",sampleSheet[,"Processed_bamFileName"]),gsub("_Processed\\.bam","",sampleSheet[match(sampleSheet[,"InputToUse"],sampleSheet[,"SampleName"],incomparable=NA),"Processed_bamFileName"])),ncol=2,byrow=F,dimnames=list(NULL,c("Samples","Inputs")))
SamplesAndInputs <- SamplesAndInputs[!is.na(SamplesAndInputs[,"Inputs"]) & ! SamplesAndInputs[,"Samples"] %in% "No_Processed_Bam",]

Config <- read.delim("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/Config/Config.txt",sep="\t",header=F)

if(nrow(SamplesAndInputs) != 0){
MacsGenomes <- matrix(c("HG18","hs","GRCh37","hs","MM9","mm"),ncol=2,byrow=T)
SicerGenomes <- matrix(c("HG18","hg18","GRCh37","hg19","MM9","mm9"),ncol=2,byrow=T)
TPICsGenomes <- matrix(c("HG18","hg18","GRCh37","GRCh37","MM9","mm9"),ncol=2,byrow=T)


MFOLD_PARAMETER <- as.vector(Config[Config[,1] == "Macs_mFold",2])
FRAGMENTLENGTH_PARAMETER <- as.numeric(as.vector(Config[Config[,1] == "Fragment_Length",2]))
SHIFTSIZE_PARAMETER <- round(FRAGMENTLENGTH_PARAMETER/2)
GENOME_PARAMETER <- as.vector(Config[Config[,1] == "Genome",2])
MACSGENOME_PARAMETER <- as.vector(MacsGenomes[MacsGenomes[,1] == GENOME_PARAMETER,2]) 
SICERGENOME_PARAMETER <- as.vector(SicerGenomes[SicerGenomes[,1] == GENOME_PARAMETER,2]) 
TPICSGENOME_PARAMETER <- as.vector(TPICsGenomes[TPICsGenomes[,1] == GENOME_PARAMETER,2]) 


if(GENOME_PARAMETER %in% "GRCh37"){
	GENOME_LENGTHS <- "/lustre/mib-cri/carrol09/MyPipe/bedFiles/hg19.txt"
}
if(GENOME_PARAMETER %in% "HG18"){
	GENOME_LENGTHS <- "/lustre/mib-cri/carrol09/MyPipe/bedFiles/hg18.txt"
}


TempFull <- read.delim("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/MultiMacs.xml",header=F,stringsAsFactors=F,quote="")
TempFull[grep("RunDirectory_ToChange",TempFull[,1]),1] <- gsub("RunDirectory_ToChange",getwd(),TempFull[grep("RunDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("BamDirectory_ToChange",TempFull[,1]),1] <- gsub("BamDirectory_ToChange",file.path(getwd(),"bamFiles"),TempFull[grep("BamDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("WorkingDirectory_ToChange",TempFull[,1]),1] <- gsub("WorkingDirectory_ToChange",getwd(),TempFull[grep("WorkingDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("GenomeLengths_ToChange",TempFull[,1]),1] <- gsub("GenomeLengths_ToChange",GENOME_LENGTHS,TempFull[grep("GenomeLengths_ToChange",TempFull[,1]),1])


SpecialisationSet <- TempFull[grep("specialisation identifier",TempFull[,1]):max(grep("specialisation>",TempFull[,1])),1]
AllSets <- rep(list(SpecialisationSet),nrow(SamplesAndInputs))
BigSet <- vector("character")
for(i in 1:length(AllSets)){
  AllSets[[i]][grep("Indentifier_ToChange",AllSets[[i]])] <- gsub("Indentifier_ToChange",getRandString(len=6),AllSets[[i]][grep("Indentifier_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("Test_ToChange",AllSets[[i]])] <- gsub("Test_ToChange",paste(SamplesAndInputs[i,1],"_Processed",sep=""),AllSets[[i]][grep("Test_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("Control_ToChange",AllSets[[i]])] <- gsub("Control_ToChange",paste(SamplesAndInputs[i,2],"_Processed",sep=""),AllSets[[i]][grep("Control_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("GM_ToChange",AllSets[[i]])] <- gsub("GM_ToChange",MACSGENOME_PARAMETER,AllSets[[i]][grep("GM_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("GS_ToChange",AllSets[[i]])] <- gsub("GS_ToChange",SICERGENOME_PARAMETER,AllSets[[i]][grep("GS_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("GT_ToChange",AllSets[[i]])] <- gsub("GT_ToChange",TPICSGENOME_PARAMETER,AllSets[[i]][grep("GT_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("MF_ToChange",AllSets[[i]])] <- gsub("MF_ToChange",MFOLD_PARAMETER,AllSets[[i]][grep("MF_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("SS_ToChange",AllSets[[i]])] <- gsub("SS_ToChange",SHIFTSIZE_PARAMETER,AllSets[[i]][grep("SS_ToChange",AllSets[[i]])])
  BigSet <- c(BigSet,AllSets[[i]])
}

FirstAllExceptSpecialisations <- TempFull[c(1:grep("specialisations",TempFull[,1])[1]),1]
SecondAllExceptSpecialisations <- TempFull[c(grep("specialisations",TempFull[,1])[2]:nrow(TempFull)),1]
Tommy <- c(FirstAllExceptSpecialisations,BigSet,SecondAllExceptSpecialisations)
write.table(Tommy,file=file.path(LocationsDir,"TrialMACS.xml"),qmethod="escape",quote=F,sep="\t",col.names=F,row.names=F)
cat("Submitting jobs!............")
system(paste("java -jar /home/mib-cri/svn_checkouts/workflow/1.2/uberjar/target/workflow-all-1.2-SNAPSHOT.jar --mode=lsf ",file.path(LocationsDir,"TrialMACS.xml"),sep=""),wait=TRUE,intern=FALSE)
cat("Jobs Submitted!\n")
}else{cat("No Peaks To Call")}

MacsFiles <- dir(path=file.path(WkgDir,"Peaks","Macs_Peaks"),pattern="*peaks.xls$")
PositivePeaks <- MacsFiles[-grep("negative",MacsFiles)]

sampleSheet <- read.delim(file.path(WkgDir,"SampleSheet.csv"),sep=",")

for(i in 1:nrow(sampleSheet)){
	if(gsub("_Processed.bam","",sampleSheet[i,"Processed_bamFileName"]) %in% gsub("_Processed_peaks.xls","",PositivePeaks)){
		MacsToRead <- file.path(WkgDir,"Peaks","Macs_Peaks",PositivePeaks[gsub("_Processed_peaks.xls","",PositivePeaks) %in% gsub("_Processed.bam","",sampleSheet[i,"Processed_bamFileName"])])
		print(MacsToRead)
		DataIn <- read.delim(MacsToRead,sep="\t",comment.char="#")
		sampleSheet[i,"MacsPeaks"] <- nrow(DataIn)		
		sampleSheet[i,"Macs_name"] <- MacsToRead
		print(nrow(DataIn))
	}
}

write.table(sampleSheet,file=file.path(WkgDir,"SampleSheet.csv"),row.names=F,sep=",")


write.table("Complete",file.path(LocationsDir,paste(Args[1],"_MainMacsProcess.txt",sep="")),col.names=T,row.names=F,sep=",",quote=F)


