getRandString<-function(len=12) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse=''))

Args <- commandArgs(trailingOnly = TRUE)
library(raster)
ConfigFile <- readIniFile(file.path(Args[2],"config.ini"))
#ConfigFile <- readIniFile(file.path(getwd(),"Temp","config.ini"))
LocationsDir <- ConfigFile[ConfigFile[,2] %in% "tempdirectory",3]
WkgDir<- ConfigFile[ConfigFile[,2] %in% "workingdirectory",3]
BamDir<- ConfigFile[ConfigFile[,2] %in% "bamdirectory",3]
FQDir<- ConfigFile[ConfigFile[,2] %in% "fastqdirectory",3]
Genome <- ConfigFile[ConfigFile[,2] %in% "genome",3]
GenomeFileOptions <-  ConfigFile[ConfigFile[,1] %in% "Genomes",]
GenomeFile <- GenomeFileOptions[GenomeFileOptions[,2] %in% tolower(Genome),3]


if(file.exists(file.path(LocationsDir,"ActualFQLocations.txt")) & file.info(file.path(LocationsDir,"ActualFQLocations.txt"))$size > 0){
	MinoMatFinal <- read.delim(file.path(LocationsDir,"FastQPositions.txt"),sep="\t",header=T,stringsAsFactors = F)


sampleSheet <- as.matrix(read.delim(file.path(WkgDir,"SampleSheet.csv"),sep=","))



TempFull <- read.delim("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/AlignMeta.xml",header=F,stringsAsFactors=F,quote="")
TempFull[grep("WorkingDirectory_ToChange",TempFull[,1]),1] <- gsub("WorkingDirectory_ToChange",WkgDir,TempFull[grep("WorkingDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("TempDirectory_ToChange",TempFull[,1]),1] <- gsub("TempDirectory_ToChange",LocationsDir,TempFull[grep("TempDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("FQDirectory_ToChange",TempFull[,1]),1] <- gsub("FQDirectory_ToChange",FQDir,TempFull[grep("FQDirectory_ToChange",TempFull[,1]),1])


SpecialisationSet <- TempFull[grep("specialisation identifier",TempFull[,1]):max(grep("specialisation>",TempFull[,1])),1]
AllSets <- rep(list(SpecialisationSet),nrow(MinoMatFinal))

BigSet <- vector("character")
for(i in 1:length(AllSets)){
  AllSets[[i]][grep("Input_To_Fill",AllSets[[i]])] <- gsub("Input_To_Fill",file.path(FQDir,MinoMatFinal[i,1],strsplit(MinoMatFinal[i,2],"/")[[1]][length(strsplit(MinoMatFinal[i,2],"/")[[1]])]),AllSets[[i]][grep("Input_To_Fill",AllSets[[i]])])
  AllSets[[i]][grep("Output_To_Fill",AllSets[[i]])] <- gsub("Output_To_Fill",MinoMatFinal[i,1],AllSets[[i]][grep("Output_To_Fill",AllSets[[i]])])
  AllSets[[i]][grep("Genome_To_Fill",AllSets[[i]])] <- gsub("Genome_To_Fill",GenomeFile,AllSets[[i]][grep("Genome_To_Fill",AllSets[[i]])])
  AllSets[[i]][grep("GenomeBuild_To_Fill",AllSets[[i]])] <- gsub("GenomeBuild_To_Fill",Genome,AllSets[[i]][grep("GenomeBuild_To_Fill",AllSets[[i]])])
  BigSet <- c(BigSet,AllSets[[i]])
}

FirstAllExceptSpecialisations <- TempFull[c(1:grep("specialisations",TempFull[,1])[1]),1]
SecondAllExceptSpecialisations <- TempFull[c(grep("specialisations",TempFull[,1])[2]:nrow(TempFull)),1]
Tommy <- c(FirstAllExceptSpecialisations,BigSet,SecondAllExceptSpecialisations)
write.table(Tommy,file=file.path(LocationsDir,"TrialFQAlign.xml"),qmethod="escape",quote=F,sep="\t",col.names=F,row.names=F)
cat("Submitting jobs!............")
system(paste("java -jar /lustre/mib-cri/carrol09/MyPipe/workflow-all-1.2-SNAPSHOT.jar --mode=lsf ",file.path(LocationsDir,"TrialFQAlign.xml"),sep=""),wait=TRUE,intern=FALSE)
cat("Jobs Submitted!\n")

sampleSheet <- as.matrix(read.delim(file.path(WkgDir,"SampleSheet.csv"),sep=","))
files <- dir(path=BamDir,pattern=paste("*.Realigned",Genome,".bam$",sep=""))


SLXids <- strsplit(files,"\\.bwa")
FileMat <- matrix(nrow=length(files),ncol=2)
FileMat[,1] <- matrix(unlist(SLXids),ncol=2,byrow=T)[,1]
FileMat[,2] <- files

for(i in 1:nrow(sampleSheet)){
	#ToCompare <- strsplit(as.vector(sampleSheet[i,1]),"\\.")[[1]][1]
	if(sampleSheet[i,1] %in% FileMat[,1]){
	sampleSheet[i,"bamFileName"] <- FileMat[FileMat[,1] %in% sampleSheet[i,1],2]
	}	
}
write.table(sampleSheet,file.path(WkgDir,"SampleSheet.csv"),col.names=T,row.names=F,sep=",",quote=F)
}
write.table("Complete",file.path(LocationsDir,paste(Args[1],"_MainFQRealignProcess.txt",sep="")),col.names=T,row.names=F,sep=",",quote=F)


