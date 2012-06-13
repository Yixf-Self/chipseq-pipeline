#!/bin/bash	

	perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/SLXID_PerlPickUp.pl $1
	perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/Project_PerlPickUp.pl $1
	perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/SLXID_StartToPipeline.pl $1
	perl /lustre/mib-cri/carrol09/Work/MyPipe/Process10/PerlScripts/Project_StartToPipeline.pl $1
	bash /lustre/mib-cri/carrol09/Work/MyPipe/Process10/BashScripts/CatLimmsInfo.sh  $1/Projects_ActualLocations.txt $1/SLXIDs_ActualLocations.txt $1/ActualLocations.txt $1/Projects_LimmsInfo.txt $1/SLXIDs_LimmsInfo.txt $1/Lims_Info.txt
