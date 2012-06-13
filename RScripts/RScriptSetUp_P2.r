getRandString<-function(len=12) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse=''))

Args <- commandArgs(trailingOnly = TRUE)
library(raster)
#ConfigFile <- readIniFile(file.path(Args[2],"config.ini"))
ConfigFile <- readIniFile(file.path(getwd(),"Temp","config.ini"))
LocationsDir <- ConfigFile[ConfigFile[,2] %in% "tempdirectory",3]
WkgDir<- ConfigFile[ConfigFile[,2] %in% "workingdirectory",3]
BamDir<- ConfigFile[ConfigFile[,2] %in% "bamdirectory",3]
 

sampleSheet <- read.delim(file.path(WkgDir,"SampleSheet.csv"),sep=",")
Samples <- sampleSheet[!is.na(sampleSheet[,"Processed_bamFileName"]) & ! sampleSheet[,"Processed_bamFileName"] %in% "No_Processed_Bam","Processed_bamFileName"]


Config <- read.delim("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/Config/Config.txt",sep="\t",header=F)

GENOME_PARAMETER <- as.vector(Config[Config[,1] == "Genome",2])

if(GENOME_PARAMETER %in% "GRCh37"){
	GENOME_LENGTHS <- "/lustre/mib-cri/carrol09/MyPipe/bedFiles/hg19.txt"
}
if(GENOME_PARAMETER %in% "HG18"){
	GENOME_LENGTHS <- "/lustre/mib-cri/carrol09/MyPipe/bedFiles/hg18.txt"
}


TempFull <- read.delim("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/MultiBamMeta_P2.xml",header=F,stringsAsFactors=F,quote="")
TempFull[grep("RunDirectory_ToChange",TempFull[,1]),1] <- gsub("RunDirectory_ToChange",getwd(),TempFull[grep("RunDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("BamDirectory_ToChange",TempFull[,1]),1] <- gsub("BamDirectory_ToChange",BamDir,TempFull[grep("BamDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("WorkingDirectory_ToChange",TempFull[,1]),1] <- gsub("WorkingDirectory_ToChange",WkgDir,TempFull[grep("WorkingDirectory_ToChange",TempFull[,1]),1])
TempFull[grep("GenomeLengths_ToChange",TempFull[,1]),1] <- gsub("GenomeLengths_ToChange",GENOME_LENGTHS,TempFull[grep("GenomeLengths_ToChange",TempFull[,1]),1])
TempFull[grep("TempDirectory_ToChange",TempFull[,1]),1] <- gsub("TempDirectory_ToChange",LocationsDir,TempFull[grep("TempDirectory_ToChange",TempFull[,1]),1])


SpecialisationSet <- TempFull[grep("specialisation identifier",TempFull[,1]):max(grep("specialisation>",TempFull[,1])),1]
AllSets <- rep(list(SpecialisationSet),length(Samples))
BigSet <- vector("character")
for(i in 1:length(AllSets)){
  AllSets[[i]][grep("Indentifier_ToChange",AllSets[[i]])] <- gsub("Indentifier_ToChange",getRandString(len=6),AllSets[[i]][grep("Indentifier_ToChange",AllSets[[i]])])
  AllSets[[i]][grep("Test_ToChange",AllSets[[i]])] <- gsub("Test_ToChange",paste(gsub(".bam","",Samples[i]),sep=""),AllSets[[i]][grep("Test_ToChange",AllSets[[i]])])
  BigSet <- c(BigSet,AllSets[[i]])
}

FirstAllExceptSpecialisations <- TempFull[c(1:grep("specialisations",TempFull[,1])[1]),1]
SecondAllExceptSpecialisations <- TempFull[c(grep("specialisations",TempFull[,1])[2]:nrow(TempFull)),1]
Tommy <- c(FirstAllExceptSpecialisations,BigSet,SecondAllExceptSpecialisations)
write.table(Tommy,file=file.path(LocationsDir,"TrialP2.xml"),qmethod="escape",quote=F,sep="\t",col.names=F,row.names=F)
cat("Submitting jobs!............")
system(paste("java -jar /home/mib-cri/svn_checkouts/workflow/1.2/uberjar/target/workflow-all-1.2-SNAPSHOT.jar --mode=lsf ",file.path(LocationsDir,"TrialP2.xml"),sep=""),wait=TRUE,intern=FALSE)

#WigMat <- matrix(nrow=nrow(sampleSheet),ncol=2)
#colnames(WigMat) <- c("BedGraph_Files","BigWig_Files")
#sampleSheet <- cbind(sampleSheet,WigMat)
#files <- dir(path=file.path(WkgDir,"Coverage"),pattern="*.bedgraph")
#WigIDs <- gsub(".wig","",files)
#sampleSheet[unique(na.omit(match(as.vector(WigIDs),gsub(".bam","",as.vector(sampleSheet[,"Processed_bamFileName"]))))),"Wig_Files"] <- files
#sampleSheet[is.na(sampleSheet[,"Wig_Files"]),"Wig_Files"] <- "No_BedGraph_File_Available"



sampleSheet <- read.delim(file.path(WkgDir,"SampleSheet.csv"),sep=",")

for(i in 1:nrow(sampleSheet)){
	SampleToLookFor <- gsub(".bam","",sampleSheet[i,"Processed_bamFileName"])
	BedGraphFile <- dir(path=file.path(WkgDir,"Coverage"),pattern=paste(SampleToLookFor,".bedgraph",sep=""),full.names=T)
	if(length(BedGraphFile) > 0){
		sampleSheet[i,"BedGraph_Files"] <- BedGraphFile 
	}

	BigWigFile <- dir(path=file.path(WkgDir,"Coverage"),pattern=paste(SampleToLookFor,".bw",sep=""),full.names=T)
	if(length(BigWigFile) > 0){
		sampleSheet[i,"BigWig_Files"] <- BigWigFile
	}

}	
	
#write.table(sampleSheet,file=file.path(WkgDir,"SampleSheet.csv"),row.names=F,sep=",")



#files <- dir(path=file.path(WkgDir,"Coverage"),pattern="*.bw$")
#BigWigIDs <- gsub(".bw$","",files)
#sampleSheet[unique(na.omit(match(as.vector(BigWigIDs),gsub(".bam","",as.vector(sampleSheet[,"Processed_bamFileName"]))))),"BigWig_Files"] <- files
#sampleSheet[is.na(sampleSheet[,"BigWig_Files"]),"BigWig_Files"] <- "No_BigWig_File_Available"

write.table(sampleSheet,file.path(WkgDir,"SampleSheet.csv"),sep=",",row.names=F,quote=F)
cat("Jobs Submitted!\n")
write.table("Complete",file.path(LocationsDir,paste(Args[1],"_MainWigProcess.txt",sep="")),col.names=T,row.names=F,sep=",",quote=F)
