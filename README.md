chipseq-pipeline
================

ChiSeq installation
-------------------

Please see project https://github.com/crukci-bioinformatics/chipseq-installer

ChipSeq usage
-------------

* Setup your environment
    
    source /software/team18/chipseq-test/env_csh.sh
    
* Create an empty samplesheet

    CHIPSEQ_ROOT/chipseq-pipeline-master/Process10/RScripts/ChipSeq.r --JustDummy Yes 

* Edit your sample sheet (file: SampleSheet.csv) in excel
** GenomicsID needs to be unique
** SampleName is important
** AnalysisState needs to be set to 'RunMe' - very important
** all the rest could have 'NA'
** FQLocation e.g. /lustre/scratch109/sanger/wy2/peng/OneClean.fq.gz - alignment will be redone using bwa

* Run it

    bsub -n2 -M 8000 -R 'span[hosts=1] select[mem>=8000] rusage[mem=8000]' -G team18-grp -J chipseq-pipeline-test -o /lustre/scratch109/sanger/wy2/chipseq-test2/chipseq-pipeline-test_%J.out -q normal "$CHIPSEQ_ROOT/chipseq-pipeline-master/Process10/RScripts/ChipSeq.r --genome mm9 --callMacsPeaks Yes --callMacsMotifs Yes --callMacsPeakProfile Yes"

* Contacts

Thomas Carroll <Thomas.Carroll@cruk.cam.ac.uk>
Anne Pajon <Anne.Pajon@cruk.cam.ac.uk>

