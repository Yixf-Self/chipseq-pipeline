python /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PythonScripts/ChIPSeqKick.py --genome GRCh37 --addSLXIDs SLX-4915
#perl /lustre/mib-cri/carrol09/MyPipe/AnotherPerlPickUp.pl
#perl /lustre/mib-cri/carrol09/MyPipe/MyStartToPipeline.pl 

perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/SLXID_PerlPickUp.pl /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp
perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/Project_PerlPickUp.pl /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp
perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/SLXID_StartToPipeline.pl /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp
perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/Project_StartToPipeline.pl /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp

bash /lustre/mib-cri/carrol09/Work/MyPipe/Process10/BashScripts/CatLimmsInfo.sh  /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp/Projects_ActualLocations.txt /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp/SLXIDs_ActualLocations.txt /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp/ActualLocations.txt /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp/Projects_LimmsInfo.txt /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp/SLXIDs_LimmsInfo.txt /lustre/mib-cri/carrol09/Work/PipelinePracticeSet/20111109_RossAdams_DN_HNF1bChIP/Temp/Lims_Info.txt

mkdir -p bamFiles
lfs setstripe bamFiles


Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RGetBamsScriptSetup.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RScriptSetUpForRealign.r
perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/FQPickUp.pl
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RScriptSetUpForFQFetch.r
#Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RRealignScriptSetUp.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RMergeScriptSetup.r
#Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RScriptSetUp.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RScriptSetUp_P1.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RScriptSetUp_P2.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RMacsSetUp.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RSicerSetUp.r
Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RTpicsSetUp.r
#Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RBamProcessScriptSetUp.r
#Rscript --vanilla /lustre/mib-cri/carrol09/Work/MyPipe/Process10/RScripts/RPeakCallScriptSetUp.r
