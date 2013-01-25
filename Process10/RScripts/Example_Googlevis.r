###ChIP-seq HTML report

## Get R libraries

library(ggplot2)
library(hwriter)
library(googleVis)
library(reshape2)
library(Hmisc)
library(XML)
library(tractor.base)
library(raster)
library(scales)

source("/lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/HTML_functions.r")




## Get arguments from the command line
getRandString<-function(len=12) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse=''))
Args <- commandArgs(trailingOnly = TRUE)
WkgDir <- Args[1]
WkgDir <- getwd()




ConfigFile <- readIniFile(file.path(getwd(),"Temp","config.ini"))
genome <- ConfigFile[ConfigFile[,2] %in% "genome",3]


system(paste("mkdir -p ",file.path(WkgDir,"HTML_Report","Plots")))
system(paste("mkdir -p ",file.path(WkgDir,"HTML_Report","IGV")))
system(paste("mkdir -p ",file.path(WkgDir,"HTML_Report","PerSample_Genometricor")))



## Read in SampleSheet
SampleSheet <- read.delim(file.path(WkgDir,"SampleSheet.csv"),stringsAsFactors=F,sep=",",h=T)
NewGroup <- cbind(SampleSheet[,c("GenomicsID","SampleName")],paste(SampleSheet[,"Tissue"],SampleSheet[,"Factor"],sep="_"))
colnames(NewGroup) <- c("GenomicsID","SampleName","Tissue_And_Factor")


####Make Images

##On/off Peak image
tryCatch(
MakeOnOffPeakPics()
,error=function(e) ErrorForImageMaking(p),
finally=print("On/Off peak Images done"))

tryCatch(
GetGenomicCov(SampleSheet)
,error=function(e) ErrorForImageMaking(p),
finally=print("On/Off peak Images done"))


tryCatch(
GetCoverageProfilePlot(SampleSheet)
,error=function(e) ErrorForImageMaking(p),
finally=print("CoverageProfilePlots done"))

tryCatch(
GetPeakProfilePlot(SampleSheet)
,error=function(e) ErrorForImageMaking(p),
finally=print("CoverageProfilePlots done"))

tryCatch(
GetGCInPeaks(SampleSheet)
,error=function(e) ErrorForImageMaking(p),
finally=print("GC content in peaks done"))

tryCatch(
GetCoverageShiftInPeaks(SampleSheet)
,error=function(e) ErrorForImageMaking(p),
finally=print("Coverage shift plot done"))

tryCatch(
GetGiniSDPlots(SampleSheet)
,error=function(e) ErrorForImageMaking(p),
finally=print("Coverage shift plot done"))


#tryCatch(
#PlotPosAndNegInPeaks(SampleSheet,p)
#,error=function(e) ErrorForImageMaking(p),
#finally=print("Pos/Neg peak Images done")
#)

### Open HTML page for writing
p <- openPage(file.path(WkgDir,"HTML_Report","SampleSummary.html"))
hwrite("ChIP-Seq HTML Report", heading=1,p,br=T)


hwrite("The Report",p,style='font-weight: bold',br=T)
hwrite("This document gives an overview of analysis performed as part of the CRI ChIP-seq analysis pipeline.",p,br=T)
hwrite("The organisation of this report follows the order of ChIP-seq analysis from generic and ChIP-seq specific QC through to the identification of genomic locations enriched for ChIP-seq signal, association with 
genomic features (i.e.genes,promoters) and any underlying enriched sequence motifs.",br=T,p)
hwrite("",p,br=T)

q <- openPage(file.path(WkgDir,"HTML_Report","Directory_Structure.html"))
OrganisationTable <- matrix(nrow=30,ncol=2)
OrganisationTable[1,1] <- basename(getwd())
OrganisationTable[1,2] <- "The project name"

OrganisationTable[2,1] <- "--HTML_Report"
OrganisationTable[2,2] <- "HTML report directory"

OrganisationTable[3,1] <- "&nbsp;&nbsp;|-SampleSummary.html" 
OrganisationTable[3,2] <- "Main HTML report (you are here!)" 

OrganisationTable[4,1] <- "&nbsp;&nbsp;|-IGV.html" 
OrganisationTable[4,2] <- "IGV session file directory"

OrganisationTable[5,1] <- "--bamFiles" 
OrganisationTable[5,2] <- "bamFile directory"

OrganisationTable[6,1] <- "&nbsp;&nbsp;|-*_Processed.bam" 
OrganisationTable[6,2] <- "Processed BamFiles for each sample"

OrganisationTable[7,1] <- "--Coverage" 
OrganisationTable[7,2] <- "Coverage and Pile-up directory"

OrganisationTable[8,1] <- "&nbsp;&nbsp;|-samplename_Processed.bw" 
OrganisationTable[8,2] <- "Processed BigWig (Coverage file) for each sample"

OrganisationTable[9,1] <- "&nbsp;&nbsp;|-samplename_Processed.bedGraph" 
OrganisationTable[9,2] <- "Processed BedGraph (Coverage file) for each sample" 

OrganisationTable[10,1] <- "--Peaks" 
OrganisationTable[10,2] <- "Main peak directory"

OrganisationTable[11,1] <- "&nbsp;&nbsp;|-Macs_Peaks" 
OrganisationTable[11,2] <- "MACS' peaks directory"

OrganisationTable[12,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-samplename_Processed_peaks.xls" 
OrganisationTable[12,2] <- "MACS output peak file per sample" 

OrganisationTable[13,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-samplename_Processed_peaks_Annotated.xls" 
OrganisationTable[13,2] <- "MACS output peak file  per sample annotated with overlapping and nearest genes"

OrganisationTable[14,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-samplename_Processed_GO_Results" 
OrganisationTable[14,2] <- "Gene Ontology terms enriched for peaks within each sample"

OrganisationTable[15,1] <- "&nbsp;&nbsp;|-Sicer_Peaks" 
OrganisationTable[15,2] <- "Sicer's peaks directory"

OrganisationTable[16,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-samplename" 
OrganisationTable[16,2] <- "Directory per sample containing Sicer results" 

OrganisationTable[17,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|-samplename_Processed-islands-summary-FDR.01" 
OrganisationTable[17,2] <- "Sicer output peak file per sample" 

OrganisationTable[18,1] <- "&nbsp;&nbsp;|-TPICS_Peaks" 
OrganisationTable[18,2] <- "TPICS peaks directory"

OrganisationTable[19,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-samplename" 
OrganisationTable[19,2] <- "Directory per sample containing TPICS results" 

OrganisationTable[20,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|-samplename_TPICS_Peaks.bed" 
OrganisationTable[20,2] <- "TPICS output peak file per sample" 

OrganisationTable[21,1] <- "--Motifs" 
OrganisationTable[21,2] <- "Main Motif directory"

OrganisationTable[22,1] <- "&nbsp;&nbsp;|--samplename" 
OrganisationTable[22,2] <- "Per sample motif directory"

OrganisationTable[23,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-Denovo" 
OrganisationTable[23,2] <- "Directory containin Denovo motif results per sample" 

OrganisationTable[24,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|-index.html" 
OrganisationTable[24,2] <- "HTML file containing links to denvo motif finding results per sample" 

OrganisationTable[25,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;|-Known" 
OrganisationTable[25,2] <- "Directory containin Known motif results per sample" 

OrganisationTable[26,1] <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|-ame.html" 
OrganisationTable[26,2] <- "HTML file containing links to known motif finding results per sample" 

OrganisationTable[27,1] <- "&nbsp;&nbsp;|-Summary_Significance_Table_meme_Denovo_Motifs.txt" 
OrganisationTable[27,2] <- "Summary table of Meme Denovo motifs discovered in all sample" 

OrganisationTable[28,1] <- "&nbsp;&nbsp;|-Summary_Significance_Table_dreme_Denovo_Motifs.txt" 
OrganisationTable[28,2] <- "Summary table of Dreme Denovo motifs discovered in all sample" 

OrganisationTable[29,1] <- "&nbsp;&nbsp;|Summary_Significance_Table_KnownMotifs.txt" 
OrganisationTable[29,2] <- "Summary table of Ame Denovo motifs discovered in all sample" 




hwrite(OrganisationTable[1:29,1:2], q, border=0,style='font-family:monospace')
closePage(q)
hwrite("Directories and Files",p,style='font-weight: bold',br=T)
hwrite("This report contains only an overview of your ChIP-seq results and so much of the information can be found in accompanying files. References to the locations of accompanying files are provided both within the text and tables
describing file contents and a breakdown of relevant files and directory structure seen ",p) 
hwrite('here', p, link="Directory_Structure.html",br=T)
hwrite("",p,br=T)

hwrite("Visualisation of Sequence Files",p,style='font-weight: bold',br=T)
hwrite("Visual inspection of your dataset as a coverage Bedgraph or BigWig using tools such IGV, IGB, Deliance or UCSC allows for a rapid evaluation of known targets of ChIP enrichment and remains a standard for evaluation of ChIP enriched regions.",p,br=T) 

hwrite("This report contains links allowing for your data to be autoloaded in IGV but these require that an IGV session of the relevant genome be running. To use a webstart link to an IGV session for your genome
please use the below link",p,br=T)

#AddSessionLink(file.path(WkgDir,"HTML_Report","IGV"),file.path(WkgDir,"HTML_Report","TrialSampleSummary.html"),genome,p)
if(tolower(genome) == "hg18"){
hwrite(hmakeTag('a','Start New IGV Session for HG18',href='http://www.broadinstitute.org/igv/projects/current/igv.php?genome=hg18'),p,br=T)
}
if(tolower(genome) == "grch37"){
hwrite(hmakeTag('a','Start New IGV Session for GRCh37',href='http://www.broadinstitute.org/igv/projects/current/igv.php?genome=hg19'),p,br=T)
}
hwrite('',br=T)

hwrite("Once a blank session has been started, files may be loaded by simply selected a link for a new session (which will contain only the file selected) or by selected a link to the current session (which will load alongside other files in session)",p,br=T)
hwrite("For a detailed description of IGV genome browser and usage please visit their website ",p) 
hwrite('here', p, link='http://www.broadinstitute.org/igv/')





hwrite("Index", heading=2,p,br=T)
hwrite("Section 1 - Post Alignment QC", heading=3,p,style='font-weight: bold',br=T,link='#s1')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Read Filtering",p,br=T,style='font-weight: italics',link='#Read_Filtering')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Table 1 - Quality Control and Summary of Read Distributions of Samples Within Project",p,br=T,style='font-style: italic',link='#T1')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Read In Genes",p,br=T,style='font-weight: italics',link='#Read_In_Features')
hwrite("Section 2 - Coverage statistics", heading=3,style='font-weight: bold',p,br=T,link='#s2')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Coverage QC Metrics",p,br=T,link='#Coverage_QC')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Table 2 - Assessment of Enrichment Efficiency and Links to Genome Coverage Graphs",p,br=T,style='font-style: italic',link='#T2')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Genomic Depth of Coverage",p,br=T,link='#Depth_QC')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Cross-Coverage and Predicted Fragment Length",p,br=T,link='#FragmentLength_QC')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Inequality of Coverage",p,br=T,link='#Gini_QC')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Average Gene/TSS Coverage",p,br=T,link='#TSS_QC')
hwrite("Section 3 - Peak Calling", heading=3,style='font-weight: bold',p,br=T,link='#s3')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Correspondence Between Peak-Callers",p,br=T,link='#AcossPeakCallers_QC')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Table 3 - Percentage of overlapping peaks and the Jaccard Index score for pairwise comparisons between peak callers",p,br=T,style='font-style:italic',link='#T3')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Macs Peaks",p,br=T,link='#Macs_Peaks')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Calling Peaks With MACS",p,br=T,link='#Macs_Calling')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Annotation Of Peaks And Functional Enrichment",p,br=T,link='#AnnoAndGO_Calling')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Table 4 - Number of Peaks Calleds and Genes With Peaks using MACS peak calling algorithm",p,br=T,style='font-style:italic',link='#T4')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Reads and Coverage In Peaks",p,br=T,link='#OnAndOff_Calling')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("GC Content of Peaks",p,br=T,link='#GC_Calling')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Peaks in Genes",p,br=T,link='#PeaksGenes_Calling')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Correlation Between Sample Peak Sets",p,br=T,link='#GenomMetric_Calling')
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
hwrite("Table 5 - Pair-wise sample Jaccard Measures and links to GenoMetriCorr results",p,br=T,style='font-style:italic',link='#T4')
#hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",p)
#hwrite("Quantitative Analysis of ChIP signal",p,br=T,link='#Diffbind_Calling')

###############################################################
##Get Reads stats table
hwrite(hwrite("Section 1 - Post Alignment QC",name='s1'),p,heading=2)

hwrite("This first section discusses sequence read QC metrics and the distributions of filtered sequence reads within gene features. These metrics allow for a rapid identification of library of sequencing problems as well as identification of any sample group specific genomic feature enrichment",br=T,p)
hwrite(" ",br=T,p)
hwrite("Read Filtering",style='font-weight: bold',br=T,p,name='Read_Filtering')
hwrite("Prior to analysis of coverage and peak calling, ChIP-seq data is filtered to remove low quality reads and artifacts. Such artifacts and noise related signal may confound many downstream tools and the use of the following filters can 
help clean data to allow for a more successfull ChIP-seq analysis.",br=T,p)
hwrite("Inorder to remove noise related reads and sequencing artifacts, the aligned files are filtered successively by:",br=T,p)
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Removing/Counting reads mapped to main contigs/chromosomes (delRand) since downstream analysis does not include random or unassembled contigs.",br=T,p)
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Removing/Counting reads mapped to mapping outside Duke Excluded Regions (Excluded),regions previously identified as enriched across multiple unrelated ChIP-seq experiments.",br=T,p)
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Removing/Counting reads with low mapping quality ,MAPQ < 15, (Filtered) and hence less reliable in downstream analysis.",br=T,p)
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Marking/Counting reads which are mapped to the exact genomic locations of another mapped read (Duplicated),since such reads are often due to PCR amplification/bias.",br=T,p)
hwrite(" ",br=T,p)
hwrite("A Summary of the numbers of reads remaining after each successive filtering step can be seen in Table-1 and Figure 1 alongside the calculated duplciation rate (Percentage of reads at final filtering step marked as Duplicated).",br=T,p)
hwrite(" ",br=T,p)


hwrite(hwrite("Table 1 - Quality Control and Summary of Read Distributions of Samples Within Project",name="T1"),heading=3,p,center=TRUE)

tryCatch(

MakeSSRS(SampleSheet,genome,p)

,error=function(e) ErrorForHTML(p),

finally=print("Read Stats HTML done"))

hwrite("Table shows sample information and the breakdown of read numbers and duplication rate after successive read quality filtering steps.",br=T,p)
hwrite("",br=T,p)

tryCatch(

MakeReadsBarplot(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Read Stats Image HTML done"))
hwrite("Figure 1 - Barplot of reads remaining after successive filtering steps",br=T,p)
hwrite("",br=T,p)
hwrite("Reads in Genes",style='font-weight: bold',br=T,p,name='Read_In_Features')
hwrite("As well as assessment of the quality of reads within a dataset, it is often useful to examine the numbers of reads mapping to genomic locations of interest.
For many epigenetic marks the percentage of reads and hence signal at the promoter, TSS and along the gene body will reveal the ChIP success and
comparisons of genomic distribution of samples relating to the same ChIP antibody should share a highly similar distribution. Figure 2 and 3 show the total or percentage of reads which
map to genomic locations of gene upstream regions, TSS (+/- 500), gene body and intragenic regions
",br=T,p)

tryCatch(

GetReadDist(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Reads in features Image HTML done"))
hwrite("Figure 2 & 3 - Barplot of total reads and percentage of total reads within genomic features",br=T,p)





###############################################################
##Get Coverage table
hwrite(hwrite("Coverage statistics",name='s2'),heading=2,br=T,p)
hwrite("",br=T,p)
hwrite("This section investigates the distribution of reads along the genome and so provides information on the quality and effiency of ChIP for the samples under investigation
as well as links to files to visualise the read distributions along the genome.",br=T,p)
hwrite(" ",br=T,p)
hwrite("Coverage QC Metrics",style='font-weight: bold',br=T,p,name='Coverage_QC')
hwrite("For transcription factor ChIP experiments a more specific signal is expected along the genome whereas no IP input controls would have a diffuse non-specific signal through-out the genome. By the use of appropriate coverage related metrics
and visualisation in genome browsers the relative success of ChIP experiments can often be established ",p)
hwrite("and so two sets of metrics usefull to coverage QC are introduced within this section-",br=T,p)
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Measures of the uniformity of Coverage (SD, Gini and adjusted Gini scores)",br=T,p)
hwrite("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Measures of the correlation between coverage on positive or negative strands (Cross-coverage scores)",br=T,p)
hwrite(" ",br=T,p)
hwrite("Also introduced in this section are genomewide coverage graphs allowing for the visualation of read depth (pileup) at points along the genome. 
",br=T,p)

hwrite("Summary of SD,Gini and adjusted Gini scores as well as links to the relevant Bedgraph and BigWig coverage files are shown in Table 2.",br=T,p)

hwrite(hwrite("Table 2 - Assessment of Enrichment Efficiency and Links to Genome Coverage Graphs",name="T2"),heading=3,p,center=TRUE)

tryCatch(

MakeSSCT(SampleSheet,genome,p)

,error=function(e) ErrorForHTML(p),

finally=print("Coverage Stats HTML done"))

hwrite("Table shows sample information relating to coverage inequality within samples (SD and Gini scores) and links to BigWig coverage files",br=T,p)
hwrite("",br=T,p)
hwrite("Genomic Depth of Coverage",style='font-weight: bold',br=T,p,name='Depth_QC')
hwrite("A simple assessment of read coverage or pile-up for each sample is to plot the number of base pairs from the genome at a given read depth. 
A sample with a higher quality ChIP enrichment will have a greater proportion of the genome at higher read depths due to specific enrichment where as an input will show a far
smaller overall proportion of the genome at such depths. In figure 4 the log 2 base pairs of genome at differing read depths (including duplicated reads) are plotted and so 
a low number of base pairs covered at a high depth will often be observed for all samples due to some PCR bias.
",br=T,p)

hwrite("",br=T,p)



tryCatch(

addImage("CoveragePlot.png",file.path(WkgDir,"HTML_Report","Plots","CoveragePlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Coverage plot Image image inserted"))
hwrite("Figure 4 - Plot of log2 base pairs at given read depths.",br=T,p)
hwrite("",br=T,p)
hwrite("Cross-Coverage and Predicted Fragment Length",style='font-weight: bold',br=T,p,name='FragmentLength_QC')
hwrite("The use of coverage profiles on the positive and negative strands to predict fragment length is an integral part of many peaks callers 
and provides a good QC measure for the quality of a ChIP-seq fragment.",br=T,p)
hwrite("Since ChIP-sequencing often involves short reads repesenting the end of a ChIPed fragment, reads from the positive and negative strand will be shifted around a true peak
or enriched region by half the fragment length. For a good ChIP-seq sample therefore this relationship between the reads on the negative and positive strand should be evident
and fragment length be close to that expected for this experiment. For ChIP samples a minimum point close to the selected fragment length is expected whereas for control samples a minimum point closer to the read length is expected
",br=T,p)
hwrite("Figure 5 shows the effects on shifting the genomic positions of positive and negative reads towards each other while measuring the proportion of genome covered at each
shift compared to proportion of genome covered wih no shifts. The shift at which the minimum proportion of the genome is covered represents the predicted fragment length.
.",br=T,p)

tryCatch(

addImage("CoverageShiftPlot.png",file.path(WkgDir,"HTML_Report","Plots","CoverageShiftPlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Coverage shift plot Image image inserted"))
hwrite("Figure 5 - Plot of proportion of genome covered following shifting of reads on positive and negative strands towards eachother",br=T,p)
hwrite("",br=T,p)

hwrite("Inequality of Coverage",style='font-weight: bold',br=T,p,name='Gini_QC')
hwrite("A high quality TF or Histone ChIP-seq experiment will have genomic islands of high read depths and long spans of genomic regions devoid of reads. In contrast a good
input control will have few islands of high read depth and consist of small fluctuations in read depth along wider areas. The use of SD and Gini scores allow for the quantitative 
assessment of uneveneness in the read depth along a genome and so allow for a QC measure of how specifically enriched is a sample or how biased is an input control.
",br=T,p)
hwrite("Figures 6-8 show the SD, Gini and adjusted Gini scores respectively for samples and any input controls. These plots can be used to assess the specificity/bias in your sample/input
and hence give a measure of the specific relative enrichment of Sample-Input combinations",br=T,p)

tryCatch(

addImage("SDsPlot.png",file.path(WkgDir,"HTML_Report","Plots","SDsPlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("On/Off image inserted"))

tryCatch(

addImage("GinisPlot.png",file.path(WkgDir,"HTML_Report","Plots","GinisPlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("On/Off image inserted"))

tryCatch(

addImage("AdjustedGinisPlot.png",file.path(WkgDir,"HTML_Report","Plots","AdjustedGinisPlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("On/Off image inserted"))

hwrite("Figure 6-8 - Plots of coverage inequality as assessed by SD, Gini and Adjusted Gini scores for samples",p) 
hwrite("(red)",p,style='font-family: monospace;color: #ff2233')
hwrite("and any corresponding inputs",p)
hwrite("(blue)",style='font-family: monospace;color:  #003F87',br=T,p)
hwrite("",br=T,p)
hwrite("Average Gene/TSS Coverage",style='font-weight: bold',br=T,p,name='TSS_QC')
hwrite("Further to the assessment of the number reads within certain genomic features seen in Section 1, many epigenetic marks are often characterised by their average distibution across the promoter and TSS.
Input samples conversely should show little enrichment across promoter,TSS or gene body and so any increase in average read depth across these regions may represent a feature specific bias. 
Figure 9 shows the average read coverage across all gene promoters and TSSs scaled to the total read length and from here any sample specific enrichment or input bias can be seen
",br=T,p)


tryCatch(

addImage("AverageTSSPlot.png",file.path(WkgDir,"Coverage","AverageTSSPlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Ave TSS image inserted"))
hwrite("Figure 9 - Average coverage across Genes scaled to total reads in sample",p) 

###############################################################
hwrite(hwrite("Peak Calling", name='s3'),heading=2,br=T,p)
hwrite("",p) 
hwrite("In this section, genomic regions of ChIP enrichment are identifed and profiled for association with other genomic features and their underlying genomic properties.",br=T,p)
hwrite("Identification of theses regions (Peaks) significantly enriched for the epigenetic mark under evaluations can be performed using many differing approaches and algorithms.",br=T,p)
hwrite("As part of the analysis pipeline, several peak callers are used to identify enriched ChIP regions. These differing peak callers allow for different classes of epigenetic marks to be identified as well as act as a further QC measure
. Samples showing little agreement between peak callers often contain higher degrees of non-specific signal or noise and so often lead to low quality peak calls",br=T,p)  

hwrite("",br=T,p)  
hwrite("Correspondence Between Peak-Callers",style='font-weight: bold',br=T,p,name='AcossPeakCallers_QC')
hwrite("In Table 3, the pair-wise agreement between peak calls is assessed. For each sample and pair-wise comparison the percentage of overlapping peaks and the Jaccard Index score for peak sets is shown. Samples showing a 
lower percentage or Jaccard score when compared to other samples within the group is often a sign of an outlying or poor sample.",br=T,p)


hwrite(hwrite("Table 3 - Percentage of overlapping peaks and the Jaccard Index score for pairwise comparisons between peak callers",name="T3"),heading=3,p,center=TRUE)

tryCatch(


MakeSSAcrossPeaks(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Across Peaks HTML done"))

 
###############################################################
##Get Peaks tables (Macs)
hwrite("",br=T,p)  
hwrite("Macs Peaks", heading=3,style='font-weight: bold',p,name="Macs_Peaks",br=T)

hwrite("A well-known and popular peak caller is ",p)
hwrite("MACs",link="http://liulab.dfci.harvard.edu/MACS/index.html",p)
hwrite(" (Model-based Analysis of ChIP-Seq)",p,br=T)
hwrite("Calling Peaks With MACS",style='font-weight: bold',br=T,p,name='Macs_Calling')
hwrite("MACS's approach is to adjust read positions by the estimated fragment length (see section?) and to scan windows along the genome for enrichment over input as well as local area using a poisson distribution.
The output from MACs is the genomic locations of enriched regions, the position of maximum enrichment over input within enriched regions and associated score and p-value of this enrichment. For a more detailed
explanation of MACS algorithm and output please see their website ",p)
hwrite("here",link="http://liulab.dfci.harvard.edu/MACS/index.html",p)
hwrite("and their paper ",p)
hwrite("here",link="http://www.ncbi.nlm.nih.gov/sites/entrez?db=pubmed&cmd=search&term=18798982[pmid]",p,br=T)
hwrite("Annotation Of Peaks And Functional Enrichment",style='font-weight: bold',br=T,p,name='AnnoAndGO_Calling')
hwrite("Following MACS peak calling, these peaks are associate with genes from the relevant genome and a Gene Ontology functional test is performed on every sample's peak calling results.",br=T,p)
hwrite("To annotate peaks to genes, all genes are first extended by 2000bp upstream from their TSS depending on their strand/orientation. Peaks are then first annotated to the extended genes they fall within. This can lead 
to a peak being associated with more than 1 gene if the extended gene boundaries cross eachother. Following this peaks which were not found to be within extended genes are then annotated to the most proximal gene irrespective of 
strand or distance. This results in every peak being associated with at least 1 gene. The number of peaks landing within 2000bp extended genes and the number of extended genes found to contain a peak are seen in Table4",br=T,p)
hwrite("Once peaks have been associated with gene, tests for an enrichment of peaks within genes in a specific functional term can be performed. Due to the differing lengths of genes and associated gene length bias of soe fucntional
terms and appropriate test accounting for differences in gene length is required. Here we apply a precision test, measuring the frequency of peaks in a functional group's genes while accounting for the proportion of genome 
related to this group. Such an approach is used in the functional enrichment tool ",p)
hwrite("GREAT",link="http://great.stanford.edu/public/html/splash.php",p,br=T)



hwrite(hwrite("Table 4 - Number of Peaks Calleds and Genes With Peaks using MACS peak calling algorithm",name="T4"),heading=3,p,center=TRUE)

tryCatch(

MakeSSMP(SampleSheet,genome,p)

,error=function(e) ErrorForHTML(p),

finally=print("Macs Peaks HTML done"))
hwrite("Table shows numbers of peaks called by MACS for each sample and the number of genes found to contain a peak within or upstream by 2000bp",br=T,p)
hwrite("",br=T,p)
hwrite("Following peak calling, several within and between sample QC measures of the produced peaks can performed to identify datasets as poor or non-reproducible. An imformative measure of peak quality is the percentage of total reads
mapping to within peak regions. A sample showing a comparitively low percentage of reads in peaks when compared to other samples maybe symptomatic of poor IP or high background noise. Figure 10 shows the total annd proportion of toal reads mapping within 
and outside peaks",br=T,p)

tryCatch(

PlotOnOffCounts(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Macs Peaks HTML done"))
hwrite("Figure 10 - The number of reads overlapping or not overlapping the MACs peak regions",br=T,p) 

hwrite("",br=T,p)
hwrite("Reads and Coverage In Peaks",style='font-weight: bold',br=T,p,name='OnAndOff_Calling')
hwrite("As well as the number of reads mapping within peak regions, the coverage profile of reads within peaks can be informative to effiency of sample ChIP. Samples showing large regions of high coverage outside of peaks show that although 
signal is present within samples, this signal is not recognisable as peaks or enriched regions. Such situations may occur to PCR duplication, a strand bias or high degrees of noise affecting the reliability of fragment length estimation or peak calling.",br=T,p)


tryCatch(

addImage("OnAndOffPeakCoverage.png",file.path(WkgDir,"HTML_Report","Plots","OnAndOffPeakCoverage.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("On/Off image inserted"))
hwrite("Figure 11 - The number of base pairs in Log2 at given read depths within the MACs peak regions and outside them",br=T,p) 

hwrite("",br=T,p)
hwrite("GC Content of Peaks",style='font-weight: bold',br=T,p,name='GC_Calling')
hwrite("Peaks calls within sample groups will show a similar genomic distribution. Poor or erroneous peak calling can therefore be identifed by examining properties of the underlying sequence of the peaks. In figure 12 the 
distibution of GC content of peaks within sample groups is show. Samples showing a very different distribution of GC in peaks compared to those within the same conditions or factors often are the result of poor peak calling and/or
GC bias within the sequencing of that sample",br=T,p)


tryCatch(

addImage("GCcontentInpeaks.png",file.path(WkgDir,"HTML_Report","Plots","GCcontentInpeaks.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("GC content image inserted"))
hwrite("Figure 12 - Boxplot of the GC content of MACs peak regions between samples",br=T,p) 


#tryCatch(

#addImage("PosAndNegRatioCoverage.png",file.path(WkgDir,"HTML_Report","Plots","PosAndNegRatioCoverage.png"),"","",p)

#,error=function(e) ErrorForHTML(p),

#finally=print("Pos/Neg image inserted"))

tryCatch(
PlotPosAndNegInPeaks(SampleSheet,p)
,error=function(e) ErrorForHTML(p),
finally=print("Read Stats Image HTML done"))

hwrite("Figure 13-14 - The total reads from the positive or negative strand which overlap MACs peak regions and percentage of overlapping reads on positive and negative strands",br=T,p) 

hwrite("",br=T,p)
hwrite("Peaks In Genes",style='font-weight: bold',br=T,p,name='PeaksGenes_Calling')
hwrite("",br=T,p)
hwrite("The distribution of peaks within genes provides a measure of peak set quality when compared to expected or sample group distributions as well as highlighting potential relationships between the 
peak sets and gene function.In figures 15 to 17 the frequency, percentage and distibution of peaks in gene ands intragenic regions shown.",br=T,p)
hwrite("",br=T,p)
tryCatch(

GetPeakDist(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Reads in features Image HTML done"))
hwrite("Figure 15 & 16 - Barplot of total peak and percentage of total peaks within genomic features",br=T,p)

tryCatch(

addImage("AveragePeakTSSPlot.png",file.path(WkgDir,"Coverage","AveragePeakTSSPlot.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Ave Peak TSS image inserted"))
hwrite("Figure 17 - Average Peaks positions across Genes scaled to total reads in sample",p) 

hwrite("",br=T,p)
#hwrite("Quantitative Analysis of ChIP signal",style='font-weight: bold',br=T,p,name='Diffbind_Calling')



hwrite("",br=T,p)
hwrite("Correlation Between Sample Peak Sets",style='font-weight: bold',br=T,p,name='GenomMetric_Calling')
hwrite("Peak calling provides a good measure of binding events within a single sample but provides no information on the reproducibilty of peaks across samples. To assess this different measures of 
between sample peak correlation may be used.",br=T,p)

hwrite(" The degree of co-occurence between peak sets can be
assessed by both the rate of overlap between peaks sets (Precision Tests) and the degree of overlap (Jaccard Measures). Spacial relationshsips between non-overlapping peak sets can be tested by comparing observed 
distances between peaks to an expected distibution of distances.
 GenoMetriCorr assess both the co-orrcurence of peaks between peaksets and spatial relations ships between peak sets
and Jaccard Measures are provided in Table 5 alongside results of all pairwise comparisons between peak-sets.All full description of GenomMetricCorr cna be seen ",p)
hwrite("here.",p,br=T,link='http://genometricorr.sourceforge.net/GenometriCorr.pdf')
hwrite("",br=T,p)
hwrite(hwrite("Table 5 - Pair-wise sample Jaccard Measures and links to GenoMetriCorr results",name="T5"),heading=3,p,center=TRUE)
tryCatch(

GetPeakToPeak(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Sicer Peaks HTML done"))
hwrite("Table show Jaccard Measures for all pair wise comparisons and links to full GenoMetriCorr results per smaple",br=T)
hwrite("",br=T,p)

hwrite("Diffbind, written by Rory Stark and Gordon Brown, uses both co-occurence of peaks and pearson correlation of signal across samples to assess similarity between samples and provide a 
metric of biological reproducibity within your experiment.",br=T,p)


hwrite("",br=T,p)
hwrite("Figure 18,19 and 20 show the results from a Diffbind occupancy and affinity analysis. Peaks were first merged across all samples and those peaks occuring in at least two samples are used for further analysis. A binary score for the 
occurence of peaks within samples is used to produce custering plot seen in figure 15 whereas normalised counts within peaks across samples used to produce heatmap and PCA in figures 16 and 17 respectively.",br=T,p)


tryCatch(

addImage("Occupancy_Heatmap.png",file.path(WkgDir,"DiffBind","Occupancy_Heatmap.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Coverage plot Image image inserted"))
hwrite("Figure 18 - Pearson clustering of samples by co-occurence of MACS enriched regions",br=T,p) 


tryCatch(

addImage("Affinity_Heatmap.png",file.path(WkgDir,"DiffBind","Affinity_Heatmap.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Coverage plot Image image inserted"))
hwrite("Figure 19 - Pearson clustering of samples by counts within MACS enriched regions",br=T,p) 

tryCatch(

addImage("Affinity_PCA.png",file.path(WkgDir,"DiffBind","Affinity_PCA.png"),"","",p)

,error=function(e) ErrorForHTML(p),

finally=print("Coverage plot Image image inserted"))

hwrite("Figure 20 - PCA of samples by counts within MACS enriched regions",br=T,p) 





###############################################################
##Get Peaks tables (Sicer)
hwrite("Sicer Peaks", heading=3,p)

hwrite(hwrite("Table 6 - Number of Peaks Called using SICER peak calling algorithm",name="T6"),heading=3,p,center=TRUE)

tryCatch(

MakeSSSP(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("Sicer Peaks HTML done"))
hwrite("Table shows numbers of peaks called by Sicer for each sample",br=T,p)

###############################################################
##Get Peaks tables (TPICs)
hwrite("TPICS Peaks", heading=3,p)

hwrite(hwrite("Table 7 - Number of Peaks Called using TPICs peak calling algorithm",name="T7"),heading=3,p,center=TRUE)
tryCatch(

MakeSSTP(SampleSheet,p)

,error=function(e) ErrorForHTML(p),

finally=print("TPICs Peaks HTML done"))
hwrite("Table shows numbers of peaks called by TPICs for each sample",br=T,p)

closePage2(p)

##################################################
##################################################
##################################################
##################################################




