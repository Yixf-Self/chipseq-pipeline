library(RJSONIO)
library(hwriter)

LinkMyGoogle <- function(ss,Charts,Table,chartOptions,chartTypes,TableName){

getRandVariable<-function(len=12) return(paste(sample(c(LETTERS,letters),len,replace=TRUE),collapse=''))
TempString <- getRandVariable()
ggoogleData <-  paste("googleData",TempString,sep="")
ssSortData <- paste("sortData",TempString,sep="")


GetMeTable <- which(colnames(ss) %in% Table)
for(i in 1:length(Charts)){
  if(i < 2){
    GetMeCharts <-  which(colnames(ss) %in% Charts[[i]])
  }else{
    GetMeCharts <-  c(GetMeCharts,which(colnames(ss) %in% Charts[[i]]))
  }
}
GetMeCharts <- unique(GetMeCharts[!GetMeCharts %in% GetMeTable])
ss <-  ss[,c(GetMeTable,GetMeCharts)]

colList <- vector("list",ncol(ss))
for(i in 1:ncol(ss)){
Temp <- c(colnames(ss)[i],colnames(ss)[i],class(ss[,i]))
if(Temp[3] == "character") {Temp[3] <- "string"}
if(Temp[3] == "numeric") {Temp[3] <- "number"}
if(Temp[3] == "integer") {Temp[3] <- "number"}

names(Temp) <- c("id","label","type")
colList[[i]] <- Temp
}
cols <- colList
#names(cols) <- "cols"

rows <- vector("list",length=nrow(ss))
for(i in 1:nrow(ss)){
      AnotherTempList <- vector("list",length=ncol(ss))
     for(j in 1:ncol(ss)){
          if(class(ss[i,j]) == "integer"){
            value <- as.numeric(ss[i,j])
          }else{
            value <- as.vector(ss[i,j])
          }
          names(value) <- "v"
         # print(value)
          AnotherTempList[[j]] <- value
     }
     Temp <- list(AnotherTempList)
     names(Temp) <- "c"
     rows[[i]] <-  Temp
}
JSON_Obj <- list(cols,rows)
names(JSON_Obj) <- c("cols","rows")



GoogleLoadCommand <- "google.load(\'visualization\', \'1\', {packages: [\'corechart\', \'table\']});\n"

TopScriptTage <- hmakeTag('script',"type=\"text/javascript\" src=\"http://www.google.com/jsapi\"",data="",newline=T)
MiddleScriptTag <- hmakeTag('script',"type=\"text/javascript\"",data=GoogleLoadCommand,newline=T)
visualization =     "var visualization;\n"
theOneWithTheWorkingDirectory ="var myloc = window.location.pathname;\nvar IGV = \"\";\nvar tom = myloc.split(\"/\");\ntom.splice(tom.length-1,1);\ntom2 = tom.join(\"/\");\n"
googleData <- gsub("googleData",ggoogleData,paste(asJSVars(googleData=JSON_Obj,qualifier="var"),"\n"))
sortData <- paste("var ",ssSortData," = new google.visualization.DataTable(",ggoogleData,");\n",sep="")
sortData2 <- paste("var ",ssSortData,"2 = new google.visualization.DataView(",ssSortData,");\n",sep="")
StartVisualisation <- "function drawVisualization() {\n"
MakeTable <- paste("var ",TableName," = new google.visualization.Table(document.getElementById(\"",TableName,"\"));\n",TableName,".draw(",ssSortData,"2, {allowHtml:true});\n",sep="")
TableIndexInMain <-  which(colnames(ss) %in% Table)-1
SetTable <- paste(ssSortData,"2.setColumns(",toJSON(TableIndexInMain),");\n",sep="")
ChartList <- vector("list",length=length(Charts))
for(i in 1:length(Charts)){
    IndexesInMain <-  which(colnames(ss) %in% Charts[[i]])-1
    ChartList[[i]] <- paste("var ",names(Charts)[i]," = new google.visualization.DataView(",ssSortData,");\n",names(Charts)[i],".setColumns(",toJSON(IndexesInMain),");\n
var ",paste(names(Charts)[i],"options",sep="")," = ",chartOptions[[i]],";\n
var ",names(Charts)[i],"chart = new google.visualization.",chartTypes[i],"(document.getElementById(\"",names(Charts)[i],"\"));\n
",names(Charts)[i],"chart.draw(",names(Charts)[i],", ",paste(names(Charts)[i],"options",sep="")," );\n",sep="")
}

DrawOnCallBack <- "google.setOnLoadCallback(drawVisualization);\n"


TopOfSortEvent <- paste("google.visualization.events.addListener(",TableName,", \'sort\',\nfunction(event) {\n",ssSortData,".sort([{column: event.column, desc: !event.ascending}]);\n",sep="")

        
CodeScriptTag <- hmakeTag('script',"type=\"text/javascript\"",data=paste(visualization,theOneWithTheWorkingDirectory,googleData,sortData,sortData2,SetTable,StartVisualisation,MakeTable,paste(unlist(ChartList),collapse="\n"),TopOfSortEvent,paste(unlist(ChartList),collapse="\n"),"})\n}\n",DrawOnCallBack),newline=T)


#cat(TopScriptTage,MiddleScriptTag,CodeScriptTag,file="test2.html")
TableVector <- paste(TopScriptTage,MiddleScriptTag,CodeScriptTag,sep="")
namesOfTable <- c("table",names(Charts))
#TableVector <- gsub("googleData",paste("googleData",TempString,sep=""),TableVector)
#TableVector <- gsub("sortData",paste("sortData",TempString,sep=""),TableVector)
setClass("TommyGoogleVis", representation(jsScript = "character", VisNames = "character"))
myGoogleVis <- new("TommyGoogleVis", jsScript = TableVector, VisNames = namesOfTable)
return(myGoogleVis)
}

GoogleVisMain <- function(myGoogleVis,p){
  TempTag <- paste(hmakeTag("Content",data=myGoogleVis@jsScript))
  hwrite(hmakeTag("Module",data=TempTag),p)
}

addVisElement <- function(GraphName,p){
  hwrite(hmakeTag("div",id=GraphName),p)  
}



closePage2 <- function (page, splash = TRUE)
{

## If above are empty it will try and guess
    title <- basename(getwd())
    title <- gsub("_"," ",title)
    name <- system("finger $(whoami) |perl -ne 'print \"$1\n\" if m/Name: (.*)/;'",intern=TRUE)
    name <- name[length(name)]
    email <- paste(sub(" ",".",name),"@cancer.org.uk",sep="")
    ByMe <- paste(name,"(",email,")",sep="")
    hwriterlink = hwrite("ChIP-seq Pipeline", link = "http://http://www.cancerresearchuk.org/")
    
    if (splash)
        hwrite(paste("\n<br/><br/><font size=\"-2\">(Project report for ",title," generated on ",
            date(), " using the ", hwriterlink, " ", "Version 1", " by ",ByMe,
            ")</font>", sep = ""), page, br = TRUE)
    else hwrite("\n<br/><br/>", page, br = TRUE)
    hwrite("</body></html>", page, br = FALSE)
    close(page)
}


MakeIGVSampleMetadata <- function(SampleSheet,igvdirectory){
write.table("#sampleTable",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,sep="\t")
sampleMetadata <- SampleSheet[,c("SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","Replicate","adjusted_Gini_Of_Coverage","NSC","RSC","NRF")]
colnames(sampleMetadata)[1] <- "Linking_id"
write.table(sampleMetadata,file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=T,quote=F,append=T,sep="\t")
BamMappings <- cbind(paste(SampleSheet[,"SampleName"],"Bam",sep="_"),SampleSheet[,"SampleName"])
BigWigMappings <- cbind(paste(SampleSheet[,"SampleName"],"Bigwig",sep="_"),SampleSheet[,"SampleName"])
MacsMappings <- cbind(paste(SampleSheet[,"SampleName"],"Macs",sep="_"),SampleSheet[,"SampleName"])
TPICSMappings <- cbind(paste(SampleSheet[,"SampleName"],"Tpics",sep="_"),SampleSheet[,"SampleName"])
SicerMappings <- cbind(paste(SampleSheet[,"SampleName"],"Sicer",sep="_"),SampleSheet[,"SampleName"])
write.table("\n#sampleMapping",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table("#Bams",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table(BamMappings,file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table("\n#BigWigs",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table(BigWigMappings,file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table("\n#Macs",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table(MacsMappings,file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table("\n#TPICS",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table(TPICSMappings,file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table("\n#Sicer",file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
write.table(SicerMappings,file.path(igvdirectory,"SampleMetadata.txt"),row.names=F,col.names=F,quote=F,append=T,sep="\t")
}


InsertSVG <- function(file,p){
Toms <- readLines(file(file))

cat(paste(Toms[-1]),"\n",file=p)

}


MakeIGVSessionXML <- function(files,fileType,names,igvdirectory,XMLname,genomeName,locusName="All"){
   i <- 1
  require(XML)

  Output <- file.path(igvdirectory,paste(XMLname,".xml",sep=""))
  resources <- vector("list",length=length(files))
  if(fileType == "Bam"){
  NewName <- paste(names[i],"_Bam",sep="")
  }
  if(fileType == "Bigwig"){
  NewName <- paste(names[i],"_Bigwig",sep="")
  }
    if(fileType == "Macs"){
  NewName <- paste(names[i],"_Macs",sep="")
  }
  if(fileType == "MacsDiff"){
  NewName <- paste(names[i],"_MacsDiff",sep="")
  }
    if(fileType == "Tpics"){
  NewName <- paste(names[i],"_Tpics",sep="")
  }
    if(fileType == "Sicer"){
  NewName <- paste(names[i],"_Sicer",sep="")
  }
  GlobalNode <- newXMLNode("Global",attrs=c(genome.value=genomeName,groupTracksBy="Linking_id",locus=locusName,version=3))
  ResourcesNode <- newXMLNode("Resources",parent=GlobalNode)
  for(i in 1:length(resources)){
    resources[[i]] <-  newXMLNode("Resource",parent=ResourcesNode,attrs=c(label=NewName[i],name=NewName[i],path=relativePath(files[i],Output),relativePath=T))
  }
  MetaDataNode <- newXMLNode("Resource",parent=ResourcesNode,attrs=c(name="SampleMetadata",path=relativePath(file.path(igvdirectory,"SampleMetadata.txt"),Output),relativePath=T))
  PanelDataNode <-  newXMLNode("Panel",attrs=c(height="350",name="DataPanel",width="1115"),parent=GlobalNode)
  if(fileType == "Bam"){
   TrackNode <-  newXMLNode("Track",attrs=c(altColor="0,0,178",color="0,0,178",colorOption="UNEXPECTED_PAIR",displayMode="EXPANDED",featureVisibilityWindow="-1",fontSize="10",id=relativePath(files[i],Output),name=NewName[i],showDataRange="true",sortByTag="",visible="true"),parent=PanelDataNode)
  }
    if(fileType == "Bigwig"){
   TrackNode <-  newXMLNode("Track",attrs=c(altColor="0,0,178",autoscale="true",color="0,0,178",displayMode="COLLAPSED",featureVisibilityWindow="-1",fontSize="10",id=relativePath(files[i],Output),name=NewName[i],renderer="BAR_CHART",showDataRange="true",visible="true",windowFunction="mean"),parent=PanelDataNode)
   DisplayRangeNode <-  newXMLNode("DataRange",attrs=c(baseline="0.0",drawBaseline="true",flipAxis="false",maximum="50",minimum="5",type="LINEAR"),parent=TrackNode)
   }
   if(fileType == "Macs" | fileType == "Tpics"| fileType == "Sicer" | fileType == "MacsDiff"){
   TrackNode <-  newXMLNode("Track",attrs=c(altColor="0,0,178",color="0,0,178",displayMode="COLLAPSED",featureVisibilityWindow="-1",fontSize="10",height="45",id=relativePath(files[i],Output),name=NewName[i],renderer="BASIC_FEATURE",showDataRange="true",sortable="false",visible="true",windowFunction="count"),parent=PanelDataNode)
   }
      saveXML(GlobalNode,file=Output)

  return(Output)
}
makeIGVLink <- function(files,fileType,names,igvdirectory,htmldirectory,XMLname,genomeName,locusName="All"){
  fileSessionName <- MakeIGVSessionXML(files,fileType,names,igvdirectory,XMLname,genomeName,locusName="All")
  fileSessionName <- relativePath(fileSessionName,htmldirectory)
  igvlink <- paste("REPBEGIN<a href=http://localhost:60151/load?file=ABS",fileSessionName,"&merge=true>","Click to load to current IGV session","</a>REPEND",sep="")
  return(igvlink)
}

makeIGVLinkFresh <- function(files,fileType,names,igvdirectory,htmldirectory,XMLname,genomeName,locusName="All"){
  fileSessionName <- MakeIGVSessionXML(files,fileType,names,igvdirectory,XMLname,genomeName,locusName="All")
  fileSessionName <- relativePath(fileSessionName,htmldirectory)
  igvlink <- paste("REPBEGIN<a href=http://localhost:60151/load?file=ABS",fileSessionName,">","Click to load to new IGV session","</a>REPEND",sep="")
  return(igvlink)
}


ReformatVisJS <- function(Tom){
Tom$html$chart["jsData"] <- gsub("var data = new google\\.visualization\\.DataTable\\(\\)\\;\n","var data = new google\\.visualization\\.DataTable\\(\\)\\;\nvar myloc = window\\.location\\.pathname\\;\nvar IGV = \"\"\\;\nvar tom = myloc\\.split\\(\"/\"\\)\\;\ntom\\.splice\\(tom\\.length-1,1\\)\\;\ntom2 = tom\\.join\\(\"/\"\\)\\;\n",Tom$html$chart["jsData"])
Tom$html$chart["jsData"] <- gsub("\"REPBEGIN","IGV.concat(\"",Tom$html$chart["jsData"])
Tom$html$chart["jsData"] <- gsub("ABS","\",tom2,\"/",Tom$html$chart["jsData"])
Tom$html$chart["jsData"] <- gsub("REPEND\"","\")",Tom$html$chart["jsData"])
return(Tom)
}


ReformatTommyVis <- function(TommyVis){
TommyVis <- gsub("\"REPBEGIN","IGV.concat(\"",TommyVis)
TommyVis <- gsub("ABS","\",tom2,\"/",TommyVis)
TommyVis <- gsub("REPEND\"","\")",TommyVis)
return(TommyVis)
}


MakeBlankSession <- function(igvdirectory,genomeName,htmldirectory){
	GlobalNode <- newXMLNode("Global",attrs=c(genome=genomeName,locus="All",version=3))
	Output <- file.path(igvdirectory,paste("BlankSession",".xml",sep=""))
  RelativeOut <- relativePath(Output,htmldirectory)
	saveXML(GlobalNode,file=Output)
  return(RelativeOut)		
}



AddSessionLink <- function(igvdirectory,htmldirectory,genome,p){
  Blanky <- MakeBlankSession(igvdirectory,"hg18",htmldirectory)
  JS1 <- hmakeTag('script',
  paste("var myloc = window.location.pathname;\nvar tom = myloc.split(\"/\");\ntom.splice(tom.length-1,1);\ntom2 = tom.join(\"/\");\nvar SessionXML=",Blanky,";\nvar LinkToSessionInfo = tom2.join(SessionXML)",sep="")
  ,"language"="javascript","type"="text/javascript")
  JS2 <- hmakeTag('a','Start New IGV Session',href='http://www.broadinstitute.org/igv/projects/current/igv.php?sessionURL=',onclick="location.href=this.href+'?key='+LinkToSessionInfo;return false;")
  hwrite(JS1,p,br=T)
  hwrite(JS2,p,br=T)
  }




GetGenomicCov <-function(SampleSheet){
Histfiles <- dir(path=file.path(WkgDir,"Coverage"),pattern="*.hist",full.names=T)
forPic <- gsub("_Processed.hist","",dir(path=file.path(WkgDir,"Coverage"),pattern="*.hist"))
for (i in 1:length(Histfiles)){
  if(i == 1){
      TempIn <- read.delim(Histfiles[i],sep="\t",header=F)
      GenomeCov <- TempIn[TempIn[,1] %in% "genome",]
      ToMerge <- cbind(GenomeCov[,2],GenomeCov[,3])
      colnames(ToMerge) <- seq(1,ncol(ToMerge))
  }
  if(i > 1){
      TempIn <- read.delim(Histfiles[i],sep="\t",header=F)
      GenomeCov <- TempIn[TempIn[,1] %in% "genome",]
      ToMerge2 <- cbind(GenomeCov[,2],GenomeCov[,3])
      ToMerge <- merge(ToMerge,ToMerge2,by=1,all=T)
      colnames(ToMerge) <- seq(1,ncol(ToMerge))
  }
}
colnames(ToMerge) <- c("Depth",gsub("\\.bwa.*","",forPic))
PlottingCov <- melt(as.data.frame(ToMerge),id.vars=c("Depth"))
PlottingCov2 <- merge(PlottingCov,NewGroup,by.x=2,by.y=1,all.x=T,all.y=F)
P <- ggplot(PlottingCov2,aes(x=Depth,y=value,col=SampleName))+geom_line()+facet_wrap(~Tissue_And_Factor)+ylab("Log2 Base Pairs")+   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))
ggsave(P,filename=file.path(WkgDir,"HTML_Report","Plots","CoveragePlot.png"))
#ggsave(L,filename=file.path(WkgDir,"HTML_Report","Plots","CoveragePlotForSVG.png"))
L <- ggplot(PlottingCov2,aes(x=Depth,y=value,col=SampleName))+stat_smooth(se = FALSE)+ylab("Log2 Base Pairs")+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))
print(L)
#SVGCov=openPage('test.html')
ForOrder <- levels(factor(PlottingCov2$SampleName))
Temp <- grid.ls()
PolyLinesOfInterest <-  Temp$name[grep("GRID.polyline",Temp$name)] 


grid.garnish(PolyLinesOfInterest[1],onmousemove=paste("showTooltip(evt, '",gsub("\n", " ", ForOrder[1]), "')",sep=""),onmouseout="hideTooltip()")
gridToSVG(file.path(WkgDir,"HTML_Report","Plots","CoveragePlot2.html"))

Start <- paste(hmakeTag('p','Check/Uncheck Sample to change display'),"\n",sep="")
for(i in 1:length(PolyLinesOfInterest)){ 
Temp <- hmakeTag('input', type="checkbox",value="on",checked="checked",id=paste(ForOrder[i],"Check",sep="_"))
Temp2 <-  hmakeTag('label',"for"=paste(ForOrder[i],"Check",sep="_"),paste(Temp,ForOrder[i]))
Start <- paste(Start, Temp2,"\n",sep="")
}
ForInsideScript <- ""
for(i in 1:length(PolyLinesOfInterest)){
ForInsideScript <- paste(ForInsideScript,"var ",paste(ForOrder[i],"Show",sep="_")," = document.getElementById(\"",paste(ForOrder[i],"Check",sep="_"),"\");\n",sep="")
}
Functions <- paste("\n\nfunction togglefn(svgId) {\nreturn function () {\nvar svgEl = document.getElementById(svgId);\nif (this.checked) {\nsvgEl.style.visibility = \"visible\";\n} else {\nsvgEl.style.visibility = \"hidden\";\n}\n};\n}\n\n")
ForInsideScript2 <- ""
for(i in 1:length(PolyLinesOfInterest)){
ForInsideScript2 <- paste(ForInsideScript2,paste(ForOrder[i],"Show",sep="_"),".addEventListener(\'change\', togglefn(\'",PolyLinesOfInterest[i],"\'), false);\n",sep="")
}
End <- hmakeTag('script',type="text/javascript",paste(ForInsideScript,Functions,ForInsideScript2,sep="\n"))
cat(file=file.path(WkgDir,"HTML_Report","Plots","CoveragePlot2.html"),Start,End,append=T)
                                                      
#cat("")

#hwrite(Temp2,SVGCov,br=T)  
}


GetReadDist <-function(SampleSheet,p){
ss <- SampleSheet
ss <- ss[,c("GenomicsID","SampleName","Reads_In_TSS","Reads_In_Promoter_2000Up_500Up","Reads_In_Promoter_5000Up_2000Up","Reads_In_Promoter_10000Up_5000Up","Reads_In_GeneBody_Minus_TSS","Reads_In_Intragenic","Percent_Of_Reads_In_TSS","Percent_Of_Reads_In_Promoter_2000Up_500Up","Percent_Of_Reads_In_Promoter_5000Up_2000Up","Percent_Of_Reads_In_Promoter_10000Up_5000Up","Percent_Of_Reads_In_GeneBody_Minus_TSS","Percent_Of_Reads_In_Intragenic")]

SampleAnno <- merge(ss,NewGroup,by.x=1,by.y=1,all.x=T,all.y=F)
ReadDistAbs <- gvisBarChart(SampleAnno,xvar="SampleName.x",yvar=c("Percent_Of_Reads_In_TSS","Reads_In_Promoter_2000Up_500Up","Reads_In_Promoter_5000Up_2000Up","Reads_In_Promoter_10000Up_5000Up","Reads_In_GeneBody_Minus_TSS","Reads_In_Intragenic"),options = list(isStacked=F))
ReadDistPer <- gvisBarChart(SampleAnno,xvar="SampleName.x",yvar=c("Percent_Of_Reads_In_TSS","Percent_Of_Reads_In_Promoter_2000Up_500Up","Percent_Of_Reads_In_Promoter_5000Up_2000Up","Percent_Of_Reads_In_Promoter_10000Up_5000Up","Percent_Of_Reads_In_GeneBody_Minus_TSS","Percent_Of_Reads_In_Intragenic"),options = list(isStacked=T))

#TG <- gvisMerge(T, G, horizontal = TRUE, tableOptions = "bgcolor=\"#CCCCCC\" cellspacing=10")

MergedPicture <- gvisMerge(ReadDistAbs,ReadDistPer, horizontal = TRUE)
  cat(createGoogleGadget(MergedPicture),file=p)

}




GetCoverageShiftInPeaks <-function(SampleSheet){

CoverageResults <- dir(path=file.path(WkgDir,"Fragment_Lengths"),pattern="*.FragCovLog",full.names=T)
CovNames <- gsub("/.*/","",gsub("_Processed.FragCovLog","",CoverageResults ))
for(i in 1:length(CoverageResults)){
	if(i == 1){
		CovFrame <- read.delim(CoverageResults[i],sep=" ",header=F)
	}
	if(i > 1){
		CovFrameTemp <- read.delim(CoverageResults[i],sep=" ",header=F)
		CovFrame <- merge(CovFrame,CovFrameTemp,by=1,all=T)
		colnames(CovFrame) <- seq(1,ncol(CovFrame))
	}
}
colnames(CovFrame) <- c("Shift_Size",gsub("\\.bwa.*","",CovNames))
temp <- melt(as.data.frame(CovFrame),id.vars=c("Shift_Size"))
tempNew <- merge(temp,NewGroup,by.x=2,by.y=1,all.x=T,all.y=F)
#png("CoverageShiftSizes.png",width=1000,height=1000)
P <- ggplot(tempNew,aes(x=Shift_Size,y=value,col=variable))+geom_line()+xlim(0,300)+ylab("Proportion of Bases Covered")+facet_wrap(~Tissue_And_Factor)

#dev.off()
#Mins <- vector("numeric",length=(ncol(CovFrame)-1))
#for(i in 1:(ncol(CovFrame)-1)){
#	Mins[i] <- CovFrame[which.min(CovFrame[,i+1]),1]
#}
#P = P+geom_vline(xintercept = Mins,colours=Mins)
ggsave(P,filename=file.path(WkgDir,"HTML_Report","Plots","CoverageShiftPlot.png"))
}

GetGiniSDPlots <-function(SampleSheet){
ss <- SampleSheet
ss <- ss[,c("GenomicsID","SampleName","InputToUse","SSD_Of_Coverage","Gini_Of_Coverage","adjusted_Gini_Of_Coverage")]
SampleAnno <- merge(ss,NewGroup,by.x=1,by.y=1,all.x=T,all.y=F)
GGSampleCo <- melt(SampleAnno)

SDsOfInput <- vector("numeric",nrow(SampleAnno))
GinisOfInput <- vector("numeric",nrow(SampleAnno))
AdjGinisOfInput <-  vector("numeric",nrow(SampleAnno))

SampleNames <- SampleAnno[,"SampleName.x"]
SDs <- SampleAnno[,"SSD_Of_Coverage"]
Ginis <- SampleAnno[,"Gini_Of_Coverage"]
AdjGinis <- SampleAnno[,"adjusted_Gini_Of_Coverage"]
groups <- factor(SampleAnno[,"Tissue_And_Factor"])
for(i in 1:nrow(SampleAnno)){
  ReleventInput <- SampleAnno[i,"InputToUse"]
  if(!is.na(ReleventInput) & any(SampleAnno[,"SampleName.x"] %in% ReleventInput)){
  SDsOfInput[i] <- SampleAnno[SampleAnno[,"SampleName.x"] %in% ReleventInput,"SSD_Of_Coverage"]
  GinisOfInput[i] <- SampleAnno[SampleAnno[,"SampleName.x"] %in% ReleventInput,"Gini_Of_Coverage"]
  AdjGinisOfInput[i] <-  SampleAnno[SampleAnno[,"SampleName.x"] %in% ReleventInput,"adjusted_Gini_Of_Coverage"]
  }else if(!is.na(ReleventInput) & any(SampleAnno[,"GenomicsID"] %in% ReleventInput)){
  SDsOfInput[i] <- SampleAnno[SampleAnno[,"GenomicsID"] %in% ReleventInput,"SSD_Of_Coverage"]
  GinisOfInput[i] <- SampleAnno[SampleAnno[,"GenomicsID"] %in% ReleventInput,"Gini_Of_Coverage"]
  AdjGinisOfInput[i] <-  SampleAnno[SampleAnno[,"GenomicsID"] %in% ReleventInput,"adjusted_Gini_Of_Coverage"]
  }else{
  SDsOfInput[i] <- NA
  GinisOfInput[i] <- NA
  AdjGinisOfInput[i] <-  NA
  }
}


png(file.path(WkgDir,"HTML_Report","Plots","SDsPlot.png"))
dotchart2(SDs,SampleNames,groups,col="darkred",dotsize=2,lcolor="orange",lty=2,xlab="SD of Coverage")
dotchart2(SDsOfInput,SampleNames, groups, add=TRUE,col="darkblue",dotsize=2,lcolor="orange",lty=2)
legend("topright",legend=c("Sample","Input_Used"),fill=c("darkred","darkblue"))
dev.off()

png(file.path(WkgDir,"HTML_Report","Plots","GinisPlot.png"))
dotchart2(Ginis,SampleNames,groups,col="darkred",dotsize=2,lcolor="orange",lty=2,xlab="Gini of Coverage")
dotchart2(GinisOfInput, SampleNames, groups, add=TRUE,col="darkblue",dotsize=2,lcolor="orange",lty=2)
legend("topright",legend=c("Sample","Input_Used"),fill=c("darkred","darkblue"))
dev.off()

png(file.path(WkgDir,"HTML_Report","Plots","AdjustedGinisPlot.png"))
dotchart2(AdjGinis,SampleNames,groups,col="darkred",dotsize=2,lcolor="orange",lty=2,xlab="Adjusted Gini of Coverage")
dotchart2(AdjGinisOfInput, SampleNames, groups, add=TRUE,col="darkblue",dotsize=2,lcolor="orange",lty=2)
legend("topright",legend=c("Sample","Input_Used"),fill=c("darkred","darkblue"))
dev.off()

png(file.path(WkgDir,"HTML_Report","Plots","AllCoverageSDGiniPlot.png"),width=2000,height=2000)
par(mfrow=c(3,1))
dotchart2(SDs,SampleNames,groups,col="darkred",dotsize=2,lcolor="orange",lty=2,xlab="SD of Coverage")
dotchart2(SDsOfInput, SampleNames, groups, add=TRUE,col="darkblue",dotsize=2,lcolor="orange",lty=2)
legend("topright",legend=c("Sample","Input_Used"),fill=c("darkred","darkblue"))
dotchart2(Ginis,SampleNames,groups,col="darkred",dotsize=2,lcolor="orange",lty=2,xlab="Gini of Coverage")
dotchart2(GinisOfInput, SampleNames, groups, add=TRUE,col="darkblue",dotsize=2,lcolor="orange",lty=2)
legend("topright",legend=c("Sample","Input_Used"),fill=c("darkred","darkblue"))
dotchart2(AdjGinis,SampleNames,groups,col="darkred",dotsize=2,lcolor="orange",lty=2,xlab="Adjusted Gini of Coverage")
dotchart2(AdjGinisOfInput, SampleNames, groups, add=TRUE,col="darkblue",dotsize=2,lcolor="orange",lty=2)
legend("topright",legend=c("Sample","Input_Used"),fill=c("darkred","darkblue"))
dev.off()
}




GetGCInPeaks <-function(SampleSheet){
ss <- SampleSheet
GCFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*GC.txt$",full.names=T)
CountFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_Counts.bed$",full.names=T)
namesForGraph <- gsub("_GC.txt","",dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*GC.txt$",full.names=F))
GCScores <- vector("list",length=length(namesForGraph))
for(i in 1:length(GCScores)){
	GCData <- read.delim(GCFiles[i],sep="\t",h=T)
	CountFile <- CountFiles[grep(namesForGraph[i],CountFiles)]
	CountData <- read.delim(CountFile,sep="\t",h=F)
	GCScores[[i]] <- GCData[,7]
	TotalGC <- as.numeric(ss[ss[,c("GenomicsID")] %in% namesForGraph,c("Filtered")])
	GCandCounts <- merge(GCData,CountData[,c(4,6)],by.x=4,by.y=1,all=T)
	GCandCounts <- cbind(GCandCounts,(((((GCandCounts[,16])/(GCandCounts[,14]))*1000)/TotalGC)*1000000))
	png(file.path(WkgDir,"HTML_Report","Plots",paste(namesForGraph[i],"RPKM_Of_PeaksVsGCOfPeaks.png",sep="")))
	smoothScatter(GCandCounts[,7]*100,GCandCounts[,17],xlab="GC Content",ylab="RPKM")
	dev.off()
}

#p <- ggplot(GGFrame, aes(factor(SampleName),Ratio))
names(GCScores) <-  paste(namesForGraph,"_W_",sep="")
#boxplot(GCScores)
TempGC <- unlist(GCScores)
TempGCMat <- data.frame(SampleName=gsub("_W_.*","",names(TempGC)),GCContent=as.numeric(TempGC))
TempGCMatNew <- merge(TempGCMat,NewGroup,by.x=1,by.y=1,all.x=T,all.y=F)
colnames(TempGCMatNew) <- c("GenomicsID","GCContent","SampleName","Tissue_And_Factor")
p <- ggplot(TempGCMatNew, aes(SampleName,GCContent,fill=Tissue_And_Factor))+geom_boxplot()+coord_flip()
#p <- ggplot(TempGCMat, aes(GCContent, fill = SampleName)) + geom_density(adjust=2/3,alpha=0.7)+facet_grid(SampleName~.)
ggsave(p,file=file.path(WkgDir,"HTML_Report","Plots","GCcontentInpeaks.png"))
}


GetCoverageProfilePlot <- function(SampleSheet){
ss <- SampleSheet
TSSProfilesFiles <- dir(file.path(WkgDir,"Coverage"),pattern="*Processed.RData",full.names=T)
TSSAverageMat <-  matrix(nrow=length(TSSProfilesFiles),ncol=5001)
namesForMat <- vector("character",length=length(TSSProfilesFiles))
ReadAmountsForMat <- vector("numeric",length=length(TSSProfilesFiles))
FileNames <- gsub(".*TSS_AvCov_","",gsub(".bwa.*","",TSSProfilesFiles))
print(FileNames)
print(TSSProfilesFiles)
for(i in 1:length(TSSProfilesFiles)){
namesForMat[i]  <- ss[ss[,1] %in% FileNames[i],"SampleName"]
ReadAmountsForMat[i]  <- ss[ss[,1] %in% FileNames[i],"Filtered"]
load(TSSProfilesFiles[i])
TSSAverageMat[i,] <- TempColMeans#/as.numeric(ReadAmountsForMat[i])
TSSAverageMat[i,] <- TSSAverageMat[i,]/as.numeric(ReadAmountsForMat[i])
rm(TempColMeans)
}
rownames(TSSAverageMat) <- namesForMat

Position <-  1000-5000:0
BigFrame <- cbind(Position,t(TSSAverageMat))
temp <- melt(as.data.frame(BigFrame),id.vars=c("Position"))
tempBig <- merge(temp,NewGroup,by=2,all.x=T,all.y=F)
p <- ggplot(tempBig,aes(x=Position,y=value,col=variable))+stat_smooth(se = TRUE)+facet_wrap(~Tissue_And_Factor)
#save(p,file=file.path(WkgDir,"Coverage","AverageTSSPlot.RData"))
ggsave(p,file=file.path(WkgDir,"Coverage","AverageTSSPlot.png"),width=20,height=8)
L <- ggplot(tempBig,aes(x=Position,y=value,col=variable))+stat_smooth(se = TRUE)
print(L)
#SVGCov=openPage('test.html')
ForOrder <- levels(factor(tempBig$variable))
Temp <- grid.ls()
PolyLinesOfInterest <-  Temp$name[grep("GRID.polyline",Temp$name)] 


grid.garnish(PolyLinesOfInterest[1],onmousemove=paste("showTooltip(evt, '",gsub("\n", " ", ForOrder[1]), "')",sep=""),onmouseout="hideTooltip()")
gridToSVG(file.path(WkgDir,"HTML_Report","Plots","AverageTSSPlot.html"))

Start <- paste(hmakeTag('p','Check/Uncheck Sample to change display'),"\n",sep="")
for(i in 1:length(PolyLinesOfInterest)){ 
Temp <- hmakeTag('input', type="checkbox",value="on",checked="checked",id=paste(ForOrder[i],"Check2",sep="_"))
Temp2 <-  hmakeTag('label',"for"=paste(ForOrder[i],"Check2",sep="_"),paste(Temp,ForOrder[i]))
Start <- paste(Start, Temp2,"\n",sep="")
}
ForInsideScript <- ""
for(i in 1:length(PolyLinesOfInterest)){
ForInsideScript <- paste(ForInsideScript,"var ",paste(ForOrder[i],"Show2",sep="_")," = document.getElementById(\"",paste(ForOrder[i],"Check2",sep="_"),"\");\n",sep="")
}
Functions <- paste("\n\nfunction togglefn(svgId) {\nreturn function () {\nvar svgEl = document.getElementById(svgId);\nif (this.checked) {\nsvgEl.style.visibility = \"visible\";\n} else {\nsvgEl.style.visibility = \"hidden\";\n}\n};\n}\n\n")
ForInsideScript2 <- ""
for(i in 1:length(PolyLinesOfInterest)){
ForInsideScript2 <- paste(ForInsideScript2,paste(ForOrder[i],"Show2",sep="_"),".addEventListener(\'change\', togglefn(\'",PolyLinesOfInterest[i],"\'), false);\n",sep="")
}
End <- hmakeTag('script',type="text/javascript",paste(ForInsideScript,Functions,ForInsideScript2,sep="\n"))
cat(file=file.path(WkgDir,"HTML_Report","Plots","AverageTSSPlot.html"),Start,End,append=T)
 

}


GetPeakProfilePlot <- function(SampleSheet){
ss <- SampleSheet
TSSProfilesFiles <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_InGenePeakProfile.txt",full.names=T)
TSSProfilesFiles2 <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_InGenePeakProfile.txt",full.names=F)
TSSAverageMat <-  matrix(nrow=length(TSSProfilesFiles),ncol=240)
namesForMat <- vector("character",length=length(TSSProfilesFiles))
ReadAmountsForMat <- vector("numeric",length=length(TSSProfilesFiles))
FileNames <- gsub("_In.*","",TSSProfilesFiles2)
print(FileNames)
print(TSSProfilesFiles)
for(i in 1:length(TSSProfilesFiles)){
namesForMat[i]  <- ss[ss[,1] %in% FileNames[i],"SampleName"]
TSSAverageMat[i,] <- read.delim(TSSProfilesFiles[i],header=F,sep="\t")[,3]
}
rownames(TSSAverageMat) <- namesForMat

Position <-  read.delim(TSSProfilesFiles[i],header=F,sep="\t")[,1]
BigFrame <- cbind(Position,t(TSSAverageMat))
temp <- melt(as.data.frame(BigFrame),id.vars=c("Position"))
tempBig <- merge(temp,NewGroup,by=2,all.x=T,all.y=F)
p <- ggplot(tempBig,aes(x=Position,y=value,col=variable))+stat_smooth(se = FALSE)+facet_wrap(~Tissue_And_Factor)
#save(p,file=file.path(WkgDir,"Coverage","AverageTSSPlot.RData"))
ggsave(p,file=file.path(WkgDir,"Coverage","AveragePeakTSSPlot.png"),width=20,height=8)
}

GetPeakDist <-function(SampleSheet,p){
ss <- SampleSheet
ss <- ss[,c("GenomicsID","SampleName")]


TSSProfilesFiles <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_InGenePeakCounts.txt",full.names=T)
TSSProfilesFiles2 <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_InGenePeakCounts.txt",full.names=F)
TSSCountMat <-  matrix(nrow=length(TSSProfilesFiles),ncol=15)
FileNames <- gsub("_In.*","",TSSProfilesFiles2)
Temp <- vector("character",length(TSSProfilesFiles))
for(i in 1:length(TSSProfilesFiles)){
	Temp[i] <- ss[ss[,1] %in% FileNames[i],"SampleName"]
	FileIn <- read.delim(TSSProfilesFiles[i],sep="\t",header=F)
	TSSCountMat[i,2:8] <- as.numeric(as.vector(FileIn[,2]))
	TSSCountMat[i,9:15] <- as.numeric(as.vector(FileIn[,3]))
}
TSSCountMat <- data.frame(TSSCountMat)
TSSCountMat[,1] <- Temp
colnames(TSSCountMat) <- c("SampleName",paste("Total_Peaks_In",FileIn[1:7,1],sep=""),paste("Percentage_Peaks_In",FileIn[1:7,1],sep=""))
#SampleAnno <- merge(ss,TSSCountMat,by.x=1,by.y=1,all.x=F,all.y=T)

ReadDistAbs <- gvisBarChart(TSSCountMat,xvar="SampleName",yvar=colnames(TSSCountMat)[2:8],options = list(isStacked=F))
ReadDistPer <- gvisBarChart(TSSCountMat,xvar="SampleName",yvar=colnames(TSSCountMat)[9:15],options = list(isStacked=T))

#TG <- gvisMerge(T, G, horizontal = TRUE, tableOptions = "bgcolor=\"#CCCCCC\" cellspacing=10")

MergedPicture <- gvisMerge(ReadDistAbs,ReadDistPer, horizontal = TRUE)
  cat(createGoogleGadget(MergedPicture),file=p)

}
#####
GetPeakToPeak <-function(SampleSheet,p){
ss <- SampleSheet
ss <- ss[,c("SampleName","Tissue","Factor","Condition_1")]

GenoCorr <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_GenoMetriCorr.txt",full.names=T)
GenoCorr2 <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_GenoMetriCorr.txt",full.names=F)
FileOfInterest <- gsub("_GenoMetriCorr.txt","",GenoCorr2)
IagainstJ <- matrix(unlist(strsplit(FileOfInterest,"_Vs_")),byrow=T,ncol=2)
JaccardMatNames <- unique((IagainstJ[,1]))
ResScores <- matrix(nrow=length(JaccardMatNames),ncol=length(JaccardMatNames))
colnames(ResScores) <- JaccardMatNames 
rownames(ResScores) <- JaccardMatNames 
GenoCorrList <- vector("list",length=length(JaccardMatNames))
names(GenoCorrList) <- JaccardMatNames
for(i in 1:length(GenoCorrList)){
GenoCorrList[[i]] <- data.frame(matrix(nrow=14,ncol=length(JaccardMatNames)+1))
GenoCorrList[[i]][,1] <- c("query.population","reference.population","relative.distances.ks.p.value","relative.distances.ecdf.area.correlation",
"query.reference.intersection","query.reference.union","jaccard.measure","projection.test.p.value","projection.test.lower.tail",
"relative.distances.ecdf.deviation.area.p.value","scaled.absolute.min.distance.sum.p.value","scaled.absolute.min.distance.sum.lower.tail",
"jaccard.measure.p.value","jaccard.measure.lower.tail")
colnames(GenoCorrList[[i]]) <- c("GenoMetriCorr_Result",JaccardMatNames)
}
for(i in 1:length(GenoCorr)){
	Temp <- read.delim(GenoCorr[i],sep="\t",header=F)
	GenoCorrList[[IagainstJ[i,1]]][,IagainstJ[i,2]] <- Temp[,2]
	ResScores[IagainstJ[i,1],IagainstJ[i,2]] <- as.numeric(as.vector(Temp[Temp[,1] %in% "jaccard.measure",2]))
}
SampleSheetJM <- merge(ss,ResScores,by.x=1,by.y=0,all.x=F,all.y=T)
namesHTML <- vector("character",length=length(GenoCorrList))
for(i in 1:length(GenoCorrList)){
temp <- openPage(file.path(WkgDir,"HTML_Report","PerSample_Genometricor",paste(names(GenoCorrList)[i],"_GenomMetriCor.html",sep="")))
Gvis_Temp <- gvisTable(GenoCorrList[[i]], options=list(width=1750, height=35*nrow(SampleSheetJM)))
cat(createGoogleGadget(Gvis_Temp),file=temp)
namesHTML[i] <- file.path("PerSample_Genometricor",paste(names(GenoCorrList)[i],"_GenomMetriCor.html",sep=""))
closePage(temp)
}
Links <- paste("<a href=",namesHTML,">","GenoMetriCor_Results","</a>",sep="")
SampleSheetJM <- cbind(SampleSheetJM[,1],Links,SampleSheetJM[,-1])
colnames(SampleSheet)[1:2] <- c("SampleName","GenoMetricCor_Results")
Gvis_JM <- gvisTable(SampleSheetJM, options=list(width=1750, height=35*nrow(SampleSheetJM)))
cat(createGoogleGadget(Gvis_JM),file=p)


}
####




PlotPosAndNegInPeaks <- function(SampleSheet,p){
PosCountFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*CountsByPos.bed$",full.names=T)
NegCountFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*CountsByNeg.bed$",full.names=T)
namesForGraph <- gsub("_CountsByPos.bed","",dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*CountsByPos.bed$",full.names=F))

ratios <- vector("list",length=length(namesForGraph))
TotalPos <- vector("numeric",length(namesForGraph))
TotalNeg <- vector("numeric",length(namesForGraph))
TotalPosPercent <- vector("numeric",length(namesForGraph))
TotalNegPercent <- vector("numeric",length(namesForGraph))
for(i in 1:length(namesForGraph)){
	PosFile <- PosCountFiles[grep(namesForGraph[i],PosCountFiles)]
	NegFile <- NegCountFiles[grep(namesForGraph[i],NegCountFiles)]
	CountForPos <- read.delim(PosFile,sep="\t",h=F)
	CountForNeg <- read.delim(NegFile,sep="\t",h=F)
	Total <- merge(CountForPos,CountForNeg[,c(4,6)],by.x=4,by.y=1,all=T)
	Total <- cbind(Total,log2(Total[,6]+Total[,7]),log2(Total[,6]/Total[,7]))
	TotalPos[i] <- sum(Total[,6])
	TotalNeg[i] <- sum(Total[,7])
	TotalPosPercent[i] <- (TotalPos[i]/(TotalPos[i]+TotalNeg[i]))*100
	TotalNegPercent[i] <- (TotalNeg[i]/(TotalPos[i]+TotalNeg[i]))*100
	ratios[[i]] <- Total[,9]
	png(file.path(WkgDir,"HTML_Report","Plots",paste(namesForGraph[i],"Ratio_Of_PerStrand_CountsVsTotalCounts.png",sep="")))
	plot(Total[,8],Total[,9],ylab="Log2 Ratio Of Reads from Strands",xlab="Log2 Total Reads")
	dev.off()
}
#png(file.path(WkgDir,"HTML_Report","Plots","BoxplotOfRatios.png"))
#vioplot(ratios,names=namesForGraph,)
#dev.off()
#names(ratios) <- namesForGraph
#ForGGPlot <- unlist(ratios)
#GGFrame <- data.frame(SampleName=names(ForGGPlot),Ratio=as.numeric(ForGGPlot))
#p <- ggplot(GGFrame, aes(factor(SampleName),Ratio))
#p <- p + geom_boxplot()
#ggsave(file=file.path(WkgDir,"HTML_Report","Plots","PosAndNegRatioCoverage.png"))
  longFrame <- data.frame(SampleName=as.character(namesForGraph),Positive_Strand_Reads=as.numeric(TotalPos),Negative_Strand_Reads=as.numeric(TotalNeg),Positive_Strand_Percentage=as.numeric(TotalPosPercent),Negative_Strand_Percentage=as.numeric(TotalNegPercent))
  OnOffAbs <- gvisBarChart(longFrame,xvar="SampleName",yvar=c("Positive_Strand_Reads","Negative_Strand_Reads"),options = list(isStacked=F))
  OnOffPercentage <- gvisBarChart(longFrame,xvar="SampleName",yvar=c("Positive_Strand_Percentage","Negative_Strand_Percentage"),options = list(isStacked=T))
  MergedPicture2 <- gvisMerge(OnOffAbs,OnOffPercentage, horizontal = TRUE)
  cat(createGoogleGadget(MergedPicture2),file=p)
}


PlotOnOffCounts <- function(SampleSheet,p){

#ss <- read.delim("SampleSheet.csv",sep=",",header=T,stringsAsFactors=F)
ss <- SampleSheet
Tempss <- ss[,c("GenomicsID","Filtered")]

OnHistFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*MergedCounts.bed$",full.names=T)
namesForGraph <- gsub("_MergedCounts.bed","",dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*MergedCounts.bed$",full.names=F))

TotalRatio  <- vector("numeric",length=length(OnHistFiles))
Total  <- vector("numeric",length=length(OnHistFiles))
TotalOn  <- vector("numeric",length=length(OnHistFiles))
TotalOff <-  vector("numeric",length=length(OnHistFiles))
TotalOnPercent  <- vector("numeric",length=length(OnHistFiles))
TotalOffPercent <- vector("numeric",length=length(OnHistFiles))

for(i in 1:length(OnHistFiles)){
	TempCount <- read.delim(OnHistFiles[i],sep="\t",header=F)
	Total[i] <- as.numeric(ss[ss[,c("GenomicsID")] %in% namesForGraph[i],c("Filtered")])
	TotalOn[i] <- sum(TempCount[,4])
	TotalOff[i] <- Total[i]-TotalOn[i]
	TotalRatio[i] <- TotalOn[i]/TotalOff[i]
	TotalOnPercent[i] <- TotalOn[i]/Total[i]
	TotalOffPercent[i] <- TotalOff[i]/Total[i]
}



names(TotalRatio) <- namesForGraph
OnOffReadCounts <- data.frame(names(TotalRatio),as.numeric(Total),as.numeric(TotalOn),as.numeric(TotalOff),as.numeric(TotalRatio),as.numeric(TotalOnPercent),as.numeric(TotalOffPercent))
longFrame <- merge(OnOffReadCounts,NewGroup,by.x=1,by.y=1,all.x=T,all.y=F)
colnames(longFrame) <- c("GenomicsID","Total","Total_In_Peaks","Total_Outside_Peaks","Total_OnVsOff_Ratio","Total_In_Percent","Total_Off_Percent","SampleName","Tissue_And_Factor")
OnOffAbs <- gvisBarChart(longFrame,xvar="SampleName",yvar=c("Total_In_Peaks","Total_Outside_Peaks"),options = list(isStacked=F))
OnOffPercentage <- gvisBarChart(longFrame,xvar="SampleName",yvar=c("Total_In_Percent","Total_Off_Percent"),options = list(isStacked=T))

#TG <- gvisMerge(T, G, horizontal = TRUE, tableOptions = "bgcolor=\"#CCCCCC\" cellspacing=10")

MergedPicture <- gvisMerge(OnOffAbs,OnOffPercentage, horizontal = TRUE)

P <- qplot(Tissue_And_Factor, Total_OnVsOff_Ratio, data = longFrame, geom = "boxplot")
#ggplot(longFrame,aes(x=Depth,y=value))+geom_bar()+ylab("Log2 Base-Pairs")+facet_wrap(~SampleName)
ggsave(file=file.path(WkgDir,"HTML_Report","Plots","OnAndOffRatioCoverage.png"))
cat(createGoogleGadget(MergedPicture),file=p)
}


MakeOnOffPeakPics <- function(){

OffHistFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*AllOutSidePeaks.hist$",full.names=T)
namesForGraph <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*AllOutSidePeaks.hist$",full.names=F)


for(i in 1:length(OffHistFiles)){
   DataIn <- read.delim(OffHistFiles[i],sep="\t")[,c(2,3)]
   print(paste("Merging hist from ",OffHistFiles[i],sep=""))
   if(i == 1){
   BigFrame <- DataIn
   }else{
   BigFrame <- merge(BigFrame,DataIn,by=1,all=T)
   }
}

OnHistFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*AllInPeaks.hist$",full.names=T)
namesForGraph2 <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*AllInPeaks.hist$",full.names=F)

for(i in 1:length(OnHistFiles)){
   DataIn <- read.delim(OnHistFiles[i],sep="\t")[,c(2,3)]
   print(paste("Merging hist from ",OnHistFiles[i],sep=""))
   if(i == 1){
   BigFrame2 <- DataIn
   }else{
   BigFrame2 <- merge(BigFrame2,DataIn,by=1,all=T)
   }
}



colnames(BigFrame) <-  c("Depth",namesForGraph)
colnames(BigFrame2) <-  c("Depth",namesForGraph2)


temp <- melt(BigFrame,id="Depth")
temp2 <- melt(BigFrame2,id="Depth")
temp <- cbind(temp,"Outside_peaks")
temp2 <- cbind(temp2,"In_peaks")
colnames(temp)[4] <- "Inside_Outside_Peaks"
colnames(temp2)[4] <- "Inside_Outside_Peaks"

longFrame <- rbind(temp,temp2)
longFrame[,3] <- longFrame[,3]
longFrame[,2] <- gsub("_G.*","",longFrame[,2])

longFrame <- merge(longFrame,NewGroup,by.x=2,by.y=1,all.x=T,all.y=F)
ggplot(longFrame,aes(x=Depth,y=value,col=Tissue_And_Factor,linetype=Inside_Outside_Peaks))+geom_smooth()+ylab("Log10 Base-Pairs")+facet_wrap(~SampleName)+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))
ggsave(file=file.path(WkgDir,"HTML_Report","Plots","OnAndOffPeakCoverage.png"))
}



MakeSSRS <- function(SampleSheet,genome,chartOptions){

   Temp <- SampleSheet[,c("SampleName","Original","delRand","Excluded","Filtered","Unique")]
#   New <- data.frame("Sample_Name"=as.character(Temp[,1]),Temp[,"Filtered"]-Temp[,"Unique"],Temp[,"Excluded"]-Temp[,"Filtered"],Temp[,"delRand"]-Temp[,"Excluded"],Temp[,"Original"]- Temp[,"delRand"]))
   Temp <- Temp[!Temp[,"Original"] %in% "No_Information_Available",]
   New=data.frame(Sample_Name=as.character(Temp[,1]),Duplicates=as.numeric(Temp[,"Filtered"])-as.numeric(Temp[,"Unique"]),QC_Filtered=as.numeric(Temp[,"Excluded"])-as.numeric(Temp[,"Filtered"]),BlackListed=as.numeric(Temp[,"delRand"])-as.numeric(Temp[,"Excluded"]),Random_Contigs=as.numeric(Temp[,"Original"])-as.numeric(Temp[,"delRand"]))
#   colnames(New) <- c("Sample_Name","Unique","MAPQC > 15","Outside Duke Excluded","Random Chromosomes")

Bams <- vector("character",length=nrow(SampleSheet))
BamIGVLinks <-   vector("character",length=nrow(SampleSheet))
BamIGVLinksFresh <-  vector("character",length=nrow(SampleSheet))
BamLocations <- dir(pattern="*_Processed.bam$",path=file.path(WkgDir,"bamFiles"),full.names=T)
for(i in 1:nrow(SampleSheet)){
   if(length(grep(SampleSheet[i,1],BamLocations)) > 0){
     Bams[i] <-  BamLocations[grep(SampleSheet[i,"GenomicsID"],BamLocations)]
   }else{
     Bams[i] <- ""
   }
 }


  
for(i in 1:nrow(SampleSheet)){
BamIGVLinks[i] <- makeIGVLink(Bams[i],"Bam",SampleSheet[i,"SampleName"],file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","HTMLReport.html"),paste("Bams",gsub("_Processed.bam$","",basename(Bams[i])),sep=""),genomeName=genome,locusName="All") 
BamIGVLinksFresh[i] <- makeIGVLinkFresh(Bams[i],"Bam",SampleSheet[i,"SampleName"],file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","HTMLReport.html"),paste("Bams",gsub("_Processed.bam$","",basename(Bams[i])),sep=""),genomeName=genome,locusName="All")
}



SampleSheet <- read.delim("SampleSheet.csv",sep=",",stringsAsFactors=F)
ss <- cbind(SampleSheet,New,BamIGVLinks,BamIGVLinksFresh)
colnames(ss)[(ncol(ss)-1):ncol(ss)] <- c("New_IGV_Session","Current_IGV_Session")
ss <- ss[,c("GenomicsID","SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","Replicate","Original","delRand","Excluded","Filtered","Unique","DuplicationRate","Reads_In_TSS","Reads_In_Promoter_2000Up_500Up","Reads_In_Promoter_5000Up_2000Up","Reads_In_Promoter_10000Up_5000Up","Reads_In_GeneBody_Minus_TSS","Reads_In_Intragenic","Percent_Of_Reads_In_TSS","Percent_Of_Reads_In_Promoter_2000Up_500Up","Percent_Of_Reads_In_Promoter_5000Up_2000Up","Percent_Of_Reads_In_Promoter_10000Up_5000Up","Percent_Of_Reads_In_GeneBody_Minus_TSS","Percent_Of_Reads_In_Intragenic","New_IGV_Session","Current_IGV_Session","Duplicates","QC_Filtered","BlackListed","Random_Contigs")]

Chart1 <- c("SampleName","Percent_Of_Reads_In_TSS","Percent_Of_Reads_In_Promoter_2000Up_500Up","Percent_Of_Reads_In_Promoter_5000Up_2000Up","Percent_Of_Reads_In_Promoter_10000Up_5000Up","Percent_Of_Reads_In_GeneBody_Minus_TSS","Percent_Of_Reads_In_Intragenic")
Chart2 <- c("SampleName","Reads_In_TSS","Reads_In_Promoter_2000Up_500Up","Reads_In_Promoter_5000Up_2000Up","Reads_In_Promoter_10000Up_5000Up","Reads_In_GeneBody_Minus_TSS","Reads_In_Intragenic")
Chart3 <- c("SampleName","Random_Contigs","BlackListed","QC_Filtered","Duplicates","Unique")
Charts <-  list(Chart3,Chart1,Chart2)
names(Charts) <- c("QCChart","BarplotOfReads","BarplotOfAbsolute")
chartTypes <- c("ColumnChart","BarChart","BarChart")
#chartOptions <- list(OptionsChart3,OptionsChart1,OptionsChart2)
Table <-  c("SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","Replicate","Original","delRand","Excluded","Filtered","Unique","DuplicationRate","New_IGV_Session","Current_IGV_Session")
googleStuff <- LinkMyGoogle(ss,Charts,Table,chartOptions,chartTypes,"table")
googleStuff@jsScript <- ReformatTommyVis(googleStuff@jsScript)
return(googleStuff)
}


MakeSSCT <- function(SampleSheet,genome,p){

GiniCovPics <- vector("character",length=nrow(SampleSheet))
BigWigs <- vector("character",length=nrow(SampleSheet))
BWIGVLinks <- vector("character",length=nrow(SampleSheet))
BWIGVLinksFresh <- vector("character",length=nrow(SampleSheet))
GiniFiles <- dir(pattern="*_GiniCoverage.png",path=file.path(WkgDir,"Coverage"),full.names=F)
GiniFileLocations <- dir(pattern="*_GiniCoverage.png",path=file.path(WkgDir,"Coverage"),full.names=T)
BigWigLocations <- dir(pattern="*.bw$",path=file.path(WkgDir,"Coverage"),full.names=T)
for(i in 1:nrow(SampleSheet)){
if(length(grep(SampleSheet[i,1],GiniFiles)) > 0){
GiniCovPics[i] <- GiniFiles[grep(SampleSheet[i,1],GiniFiles)]

system(paste("cp ",GiniFileLocations[grep(SampleSheet[i,1],GiniFileLocations)],file.path(WkgDir,"HTML_Report","Plots")))
}else{
GiniCovPics[i] <- ""
}

if(length(grep(SampleSheet[i,1],BigWigLocations)) > 0){
BigWigs[i] <- BigWigLocations[grep(SampleSheet[i,1],BigWigLocations)]

}else{
BigWigs[i] <- ""
}

}

for(i in 1:nrow(SampleSheet)){

BWIGVLinks[i] <- makeIGVLink(BigWigs[i],"Bigwig",SampleSheet[i,"SampleName"],file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","HTMLReport.html"),paste("BigWig_",gsub(".bw$","",basename(BigWigs[i])),sep=""),genomeName=genome,locusName="All") 
BWIGVLinksFresh[i] <- makeIGVLinkFresh(BigWigs[i],"Bigwig",SampleSheet[i,"SampleName"],file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","HTMLReport.html"),paste("BigWig_",gsub(".bw$","",basename(BigWigs[i])),sep=""),genomeName=genome,locusName="All") 

}


GiniCovWithLinks <- paste("<a href=",file.path("Plots",GiniCovPics),">",SampleSheet[,"Gini_Of_Coverage"],"</a>",sep="")

SSCT <- SampleSheet[,c("GenomicsID","SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1",
"Condition_2","SSD_Of_Coverage","adjusted_Gini_Of_Coverage","NSC","RSC","BedGraph_Files","BigWig_Files")]

SSCT[,"Gini_Of_Coverage"] <- GiniCovWithLinks
SSCT[,"BigWig_Files"] <- BWIGVLinks
SSCT[,"BedGraph_Files"] <- BWIGVLinksFresh
colnames(SSCT) <- c("GenomicsID","SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1",
"Condition_2","SSD_Of_Coverage","adjusted_Gini_Of_Coverage","NSC","RSC","BigWig_To_New_IGV","BigWig_To_IGV_Session")
SSCT <- SSCT[,c("SampleName","Tissue","Factor","Condition_1",
"SSD_Of_Coverage","adjusted_Gini_Of_Coverage","NSC","RSC","BigWig_To_New_IGV","BigWig_To_IGV_Session")]


Gvis_CT <- gvisTable(SSCT, options=list(width=1750, height=35*nrow(SampleSheet)))
Gvis_CT <- ReformatVisJS(Gvis_CT)
cat(createGoogleGadget(Gvis_CT),file=p)
}




MakeSSAcrossPeaks <- function(SampleSheet,p){
  SSAP <- SampleSheet[,c("GenomicsID","SampleName","Macs_In_TPICs","TPICs_In_Macs","Macs_In_Sicer","Sicer_In_Macs","TPICs_In_Sicer","Sicer_In_TPICs","JacardIndex_TPICS_And_Macs","JacardIndex_Sicer_And_Macs","JacardIndex_TPICs_And_Sicer")]
  colnames(SSAP) <- c("GenomicsID","SampleName","Macs_In_TPICs","TPICs_In_Macs","Macs_In_Sicer","Sicer_In_Macs","TPICs_In_Sicer","Sicer_In_TPICs","JacardIndex_TPICS_And_Macs","JacardIndex_Sicer_And_Macs","JacardIndex_TPICs_And_Sicer")
  Gvis_AP <- gvisTable(SSAP, options=list(width=1750, height=35*nrow(SampleSheet)))
  cat(createGoogleGadget(Gvis_AP),file=p)
}






MakePerSampleMacsPeaks <- function(HTMLreport,Name,MacsFile){
if(MacsFile != "" & file.exists(MacsFile)){
  system(paste("mkdir -p ",file.path(WkgDir,"HTML_Report","PerSamplePeakHTML")))
  peakHTML <- file.path(WkgDir,"HTML_Report","PerSamplePeakHTML",paste(Name,".html",sep=""))  
  PeaksFile <- read.delim(MacsFile,comment="#",sep="\t",header=F)
  PeaksFile <- PeaksFile[order(PeaksFile[,5],decreasing=T),]
  GotoLinks <- paste("http://localhost:60151/goto?locus=",PeaksFile[,1],":",PeaksFile[,2],"-",PeaksFile[,3],sep="")
  HTMLGotoLinks <- paste("<a href=",GotoLinks,">",PeaksFile[,4],"</a>",sep="")
  NewPeakTable <- cbind(PeaksFile[,1:3],HTMLGotoLinks,PeaksFile[,5]) 
  colnames(NewPeakTable) <- c("Chromosome","Start","End","Peak_ID","Score")
  Gvis_PerSampleMacs <- gvisTable(NewPeakTable, options=list(width=1750, height=32*nrow(NewPeakTable)))  
  pagePeak <- openPage(peakHTML)
  cat(createGoogleGadget(Gvis_PerSampleMacs),file=pagePeak) 
  closePage(pagePeak)
  relativePos <- relativePath(peakHTML,HTMLreport)
#  HTMLLinkToSamplePeaks <- paste("<a href=",relativePos,">","Link_To_Peak_Table","</a>",sep="")
  
}else{
      relativePos <- ""
}
  return(relativePos) 
}


MakeSSMP <- function(SampleSheet,genome,chartOptions){

ss <- SampleSheet
ss <- ss[,c("GenomicsID","SampleName")]


TSSProfilesFiles <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_InGenePeakCounts.txt",full.names=T)
TSSProfilesFiles2 <- dir(file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*_InGenePeakCounts.txt",full.names=F)
TSSCountMat <-  matrix(nrow=length(TSSProfilesFiles),ncol=15)
FileNames <- gsub("_In.*","",TSSProfilesFiles2)
Temp <- vector("character",length(TSSProfilesFiles))
for(i in 1:length(TSSProfilesFiles)){
	Temp[i] <- ss[ss[,1] %in% FileNames[i],"GenomicsID"]
	FileIn <- read.delim(TSSProfilesFiles[i],sep="\t",header=F)
	TSSCountMat[i,2:8] <- as.numeric(as.vector(FileIn[,2]))
	TSSCountMat[i,9:15] <- as.numeric(as.vector(FileIn[,3]))
}
TSSCountMat <- data.frame(TSSCountMat)
TSSCountMat[,1] <- Temp
colnames(TSSCountMat) <- c("GenomicsID",paste("Total_Peaks_In",FileIn[1:7,1],sep=""),paste("Percentage_Peaks_In",FileIn[1:7,1],sep=""))
#SampleAnno <- merge(ss,TSSCountMat,by.x=1,by.y=1,all.x=F,all.y=T)

#ReadDistAbs <- gvisBarChart(TSSCountMat,xvar="SampleName",yvar=colnames(TSSCountMat)[2:8],options = list(isStacked=F))
#ReadDistPer <- gvisBarChart(TSSCountMat,xvar="SampleName",yvar=colnames(TSSCountMat)[9:15],options = list(isStacked=T))


#ss <- read.delim("SampleSheet.csv",sep=",",header=T,stringsAsFactors=F)
ss <- SampleSheet
Tempss <- ss[,c("GenomicsID","Filtered")]

OnHistFiles <- dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*MergedCounts.bed$",full.names=T)
namesForGraph <- gsub("_MergedCounts.bed","",dir(path=file.path(WkgDir,"Peaks","PeakProfiles"),pattern="*MergedCounts.bed$",full.names=F))

TotalRatio  <- vector("numeric",length=length(OnHistFiles))
Total  <- vector("numeric",length=length(OnHistFiles))
TotalOn  <- vector("numeric",length=length(OnHistFiles))
TotalOff <-  vector("numeric",length=length(OnHistFiles))
TotalOnPercent  <- vector("numeric",length=length(OnHistFiles))
TotalOffPercent <- vector("numeric",length=length(OnHistFiles))

for(i in 1:length(OnHistFiles)){
	TempCount <- read.delim(OnHistFiles[i],sep="\t",header=F)
	Total[i] <- as.numeric(ss[ss[,c("GenomicsID")] %in% namesForGraph[i],c("Filtered")])
	TotalOn[i] <- sum(TempCount[,4])
	TotalOff[i] <- Total[i]-TotalOn[i]
	TotalRatio[i] <- TotalOn[i]/TotalOff[i]
	TotalOnPercent[i] <- TotalOn[i]/Total[i]
	TotalOffPercent[i] <- TotalOff[i]/Total[i]
}



names(TotalRatio) <- namesForGraph
OnOffReadCounts <- data.frame(names(TotalRatio),as.numeric(Total),as.numeric(TotalOn),as.numeric(TotalOff),as.numeric(TotalRatio),as.numeric(TotalOnPercent),as.numeric(TotalOffPercent))
longFrame <- merge(OnOffReadCounts,NewGroup,by.x=1,by.y=1,all.x=T,all.y=F)
colnames(longFrame) <- c("GenomicsID","Total","Total_In_Peaks","Total_Outside_Peaks","Total_OnVsOff_Ratio","Total_In_Percent","Total_Off_Percent","SampleName","Tissue_And_Factor")


Macs <- vector("character",length=nrow(SampleSheet))
MacsIGVLinks <-   vector("character",length=nrow(SampleSheet))
MacsIGVLinksFresh <-  vector("character",length=nrow(SampleSheet))
PerSampleReports <- vector("character",length=nrow(SampleSheet))
MacsLocations <- dir(pattern="*peaks.bed$",path=file.path(WkgDir,"Peaks","Macs_Peaks"),full.names=T)
for(i in 1:nrow(SampleSheet)){
   if(length(grep(SampleSheet[i,1],MacsLocations)) > 0){
     Macs[i] <-  MacsLocations[grep(SampleSheet[i,"GenomicsID"],MacsLocations)]
   }else{
     Macs[i] <- ""
   }
 }


for(i in 1:nrow(SampleSheet)){


MacsIGVLinks[i] <- makeIGVLink(Macs[i],"Macs",SampleSheet[i,"SampleName"],file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","HTMLReport.html"),paste("Macs",gsub("_Processed_peaks.bed$","",basename(Macs[i])),sep=""),genomeName=genome,locusName="All")
MacsIGVLinksFresh[i] <- makeIGVLinkFresh(Macs[i],"Macs",SampleSheet[i,"SampleName"],file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","HTMLReport.html"),paste("Macs",gsub("_Processed_peaks.bed$","",basename(Macs[i])),sep=""),genomeName=genome,locusName="All")

}

#for(i in 1:nrow(SampleSheet)){
#  PerSampleReports[i] <- MakePerSampleMacsPeaks(file.path(WkgDir,"HTML_Report","SampleSummary.html"),SampleSheet[i,"GenomicsID"],Macs[i])
 # print(i)
#}

#  PeakTableLists <- paste("<a href=",PerSampleReports,">",SampleSheet[,"MacsPeaks"],"</a>",sep="")
  PeakTableLists <-  vector("character",length=nrow(SampleSheet))
  SSMP1 <- SampleSheet[,c("GenomicsID","SampleName")]
  SSMP2 <- cbind(MacsIGVLinks,MacsIGVLinksFresh)
  SSMP3 <- SampleSheet[,c("Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2")]
  SSMP4 <- SampleSheet[,c("Macs_Genes_In_Peaks","Macs_Peaks_In_Genes","Macs_Genes_Annotation","Macs_GO_Terms","Macs_GO_Table")]
  SSMP <- cbind(SSMP1,SSMP2,SSMP3,PeakTableLists,SSMP4)
  
  
#  SSMP <- cbind(SSMP1,SSMP2,SSMP3,PeakTableLists)

#  SSMP <- cbind(SSRS1,SSRS2,SSRS3)

#  colnames(SSMP) <- c("GenomicsID","SampleName","IGV_Link_Peaks","Fresh_IGV_Link_Peaks","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","MacsPeaks")
#  colnames(SSMP) <- c("GenomicsID","SampleName","IGV_Link_Peaks","Fresh_IGV_Link_Peaks","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2")

if(ConfigFile[ConfigFile[,2] %in% "callmacsmotifs",3] %in% "Yes"){
ForSummaryDenovoMotifFile <- vector("character",nrow(SampleSheet))
ForSummaryKnownMotifFile <- vector("character",nrow(SampleSheet))
ForSummaryDremeMotifFile <- vector("character",nrow(SampleSheet))
ForSummaryMemeMotifFile <- vector("character",nrow(SampleSheet))
for(i in 1:nrow(SampleSheet)){

if(length(grep("index.html",dir(path=file.path(WkgDir,"Motif",SampleSheet[i,1],"Denovo")))) > 0){
ForSummaryDenovoMotifFile[i] <- file.path("..","Motif",SampleSheet[i,1],"Denovo","index.html")
}
if(length(grep("ame.html",dir(path=file.path(WkgDir,"Motif",SampleSheet[i,1],"Known")))) > 0){
ForSummaryKnownMotifFile[i] <- file.path("..","Motif",SampleSheet[i,1],"Known","ame.html")
}
if(length(grep("dreme.html",dir(path=file.path(WkgDir,"Motif",SampleSheet[i,1],"Denovo","dreme_out")))) > 0){
ForSummaryDremeMotifFile[i] <- file.path("..","Motif",SampleSheet[i,1],"Denovo","index.html")
}
if(length(grep("meme.html",dir(path=file.path(WkgDir,"Motif",SampleSheet[i,1],"Denovo","meme_out")))) > 0){
ForSummaryMemeMotifFile[i] <- file.path("..","Motif",SampleSheet[i,1],"Denovo","index.html")
}
}
#ForSummaryDenovoMotifFile <- gsub("/dreme_out/dreme.html","/index.html",SampleSheet[,"Dreme_Motif_File"])
#ForSummaryKnownMotifFile <- gsub("/Denovo/.*","/Known/ame.txt",SampleSheet[,"Dreme_Motif_File"])

system(paste("convert ","/lustre/mib-cri/carrol09/Work/MyPipe/Process10/images/CRUK_MotifKnown.png -resize 20%",file.path(WkgDir,"HTML_Report","Plots","CRUK_MotifKnownSmall.png")))
system(paste("convert ","/lustre/mib-cri/carrol09/Work/MyPipe/Process10/images/CRUK_MotifDenovo.png -resize 20%",file.path(WkgDir,"HTML_Report","Plots","CRUK_MotifDenovoSmall.png")))



Dreme_Motifs <- paste("<a href=",file.path(ForSummaryDremeMotifFile),">",SampleSheet[,"Dreme_Significant_Motifs"],"</a>",sep="")
Meme_Motifs <- paste("<a href=",file.path(ForSummaryMemeMotifFile),">",SampleSheet[,"meme_Significant_Motifs"],"</a>",sep="")
Known_Motifs <- paste("<a href=",file.path(ForSummaryKnownMotifFile),">",SampleSheet[,"Known_Significant_Motifs"],"</a>",sep="")
LotsOfPics <- paste("<img src=\"",file.path("..","HTML_Report","Plots","CRUK_MotifKnownSmall.png"),"\">",sep="")
Meme_ChIP <- paste("<a href=",file.path(ForSummaryDenovoMotifFile),">",LotsOfPics,"</a>",sep="")
SSMP <- cbind(SSMP,Known_Motifs,Meme_ChIP,Meme_Motifs,Dreme_Motifs)
colnames(SSMP) <- c("GenomicsID","SampleName","IGV_Link_Peaks","Fresh_IGV_Link_Peaks","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","Macs_Peaks","Macs_Genes_In_Peaks","Macs_Peaks_In_Genes","Macs_Genes_Annotation","Macs_GO_Terms","Macs_GO_Table","Known_Significant_Motifs","Denovo_Meme_ChIP","Meme_Significant_Motifs","Dreme_Significant_Motifs")
#SSMP <- SSMP[,c("SampleName","Tissue","Factor","Antibody","Condition_1","Macs_Peaks","Macs_Genes_In_Peaks","Macs_Peaks_In_Genes","Macs_Genes_Annotation","Macs_GO_Terms","Macs_GO_Table","IGV_Link_Peaks","Fresh_IGV_Link_Peaks","Known_Significant_Motifs","Denovo_Meme_ChIP","Meme_Significant_Motifs","Dreme_Significant_Motifs")]
}else{
colnames(SSMP) <- c("GenomicsID","SampleName","IGV_Link_Peaks","Fresh_IGV_Link_Peaks","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","Macs_Peaks","Macs_Genes_In_Peaks","Macs_Peaks_In_Genes","Macs_Genes_Annotation","Macs_GO_Terms","Macs_GO_Table")
#SSMP <- SSMP[,c("SampleName","Tissue","Factor","Antibody","Condition_1","Macs_Peaks","Macs_Genes_In_Peaks","Macs_Peaks_In_Genes","Macs_Genes_Annotation","Macs_GO_Terms","Macs_GO_Table","IGV_Link_Peaks","Fresh_IGV_Link_Peaks")]
}

SSMP2 <- merge(SSMP,TSSCountMat,by=1,all=T)
SSMP3 <- merge(SSMP2,longFrame,by=1,all=T)
SSMP3 <- SSMP3[!is.na(SSMP3[,"Macs_Genes_Annotation"]),]
colnames(SSMP3)[2] <- "SampleName"
Table <- c("SampleName","Tissue","Factor","Antibody","Condition_1","Conditions_2","Macs_Peaks","Macs_Genes_In_Peaks","Macs_Peaks_In_Genes","Macs_Genes_Annotation","Macs_GO_Terms","Macs_GO_Table","IGV_Link_Peaks","Fresh_IGV_Link_Peaks")
SSMP4 <- SSMP3[,c(1:2,5:17,2:3,18:37)]
ss <- SSMP4

Chart1 <- c("SampleName",colnames(TSSCountMat)[2:8])
Chart2 <- c("SampleName",colnames(TSSCountMat)[9:15])
# c("GenomicsID","Total","Total_In_Peaks","Total_Outside_Peaks","Total_OnVsOff_Ratio","Total_In_Percent","Total_Off_Percent","SampleName","Tissue_And_Factor")
Chart3 <- c("SampleName","Total_In_Peaks","Total_Outside_Peaks")
Chart4 <- c("SampleName","Total_In_Percent","Total_Off_Percent")


Charts <-  list(Chart1,Chart2,Chart3,Chart4)
names(Charts) <- c("AbsPeaksIn","PerPeaksIn","AbsPeaksInReads","PercentPeaksInReads")
chartTypes <- c("BarChart","BarChart","BarChart","BarChart")
#chartOptions <- list(OptionsChart1,OptionsChart2,OptionsChart3,OptionsChart4)
googleStuff <- LinkMyGoogle(ss,Charts,Table,chartOptions,chartTypes,"table2")
googleStuff@jsScript <- ReformatTommyVis(googleStuff@jsScript)
return(googleStuff)




#Gvis_MP <- gvisTable(SSMP, options=list(width=1750, height=40*nrow(SampleSheet)))
#Gvis_MP <- ReformatVisJS(Gvis_MP)
#cat(createGoogleGadget(Gvis_MP),file=p)
}

MakeSSSP <- function(SampleSheet,p){
SampleSheetSP <- SampleSheet[,c("GenomicsID","SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","Sicer_Name","SicerPeaks")]
Gvis_SP <- gvisTable(SampleSheetSP, options=list(width=1750, height=35*nrow(SampleSheet)))
cat(createGoogleGadget(Gvis_SP),file=p)

}

MakeSSTP <- function(SampleSheet,p){
SampleSheetTP <- SampleSheet[,c("GenomicsID","SampleName","Run","Lane","Tissue","Factor","Antibody","Condition_1","Condition_2","TPICS_Name","TPICsPeaks")]
Gvis_TP <- gvisTable(SampleSheetTP, options=list(width=1750, height=35*nrow(SampleSheet)))
cat(createGoogleGadget(Gvis_TP),file=p)

}

addImage <- function(Image,ImageLocation,ImageTitle,ImageText,p){
if(!file.exists(file.path(WkgDir,"HTML_Report","Plots",Image))){
  system(paste("cp ",ImageLocation,file.path(WkgDir,"HTML_Report","Plots")))
}
ImageInPlots <- file.path("..","HTML_Report","Plots",Image)
hwrite(ImageTitle, p, br=TRUE)
hwriteImage(ImageInPlots, p, br=TRUE,width=1000,height=1000)
hwrite('',p, br=TRUE)
hwrite(ImageText, p)
}


MakeReadsBarplot <- function(SampleSheet,p){
   Temp <- SampleSheet[,c("SampleName","Original","delRand","Excluded","Filtered","Unique")]
#   New <- data.frame("Sample_Name"=as.character(Temp[,1]),Temp[,"Filtered"]-Temp[,"Unique"],Temp[,"Excluded"]-Temp[,"Filtered"],Temp[,"delRand"]-Temp[,"Excluded"],Temp[,"Original"]- Temp[,"delRand"]))
   Temp <- Temp[!Temp[,"Original"] %in% "No_Information_Available",]
   New=data.frame(Sample_Name=as.character(Temp[,1]),Unique=as.numeric(Temp[,"Unique"]),Duplicated=as.numeric(Temp[,"Filtered"])-as.numeric(Temp[,"Unique"]),QC_Filtered=as.numeric(Temp[,"Excluded"])-as.numeric(Temp[,"Filtered"]),BlackListed=as.numeric(Temp[,"delRand"])-as.numeric(Temp[,"Excluded"]),Random_Contigs=as.numeric(Temp[,"Original"])-as.numeric(Temp[,"delRand"]))
#   colnames(New) <- c("Sample_Name","Unique","MAPQC > 15","Outside Duke Excluded","Random Chromosomes")
   ReadsBarPlot <- gvisColumnChart(New,xvar="Sample_Name",options = list(isStacked=T))
   cat(createGoogleGadget(ReadsBarPlot),file=p)
}

ErrorForHTML <- function(p){
  hwrite('No Information Available for Table/Plot',p)
  simpleError("No Information Available for Table/Plot")
}
ErrorForImageMaking <- function(p){
#  hwrite('No Information Available for Table/Plot',p)
  simpleError("No Image made")
}








