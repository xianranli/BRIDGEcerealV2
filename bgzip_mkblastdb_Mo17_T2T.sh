#!/bin/bash

sp_dir='/home/xianran_li/bridgecereal/database/Maize/'

line_codes="Mo17_T2T"



################
################

for line_code in $line_codes
 do
  line_dir=$sp_dir$line_code
  if [ ! -e $line_dir ]; then
    mkdir $line_dir
    cd $line_dir
#    wget https://ftp.ncbi.nlm.nih.gov/genomes/all/GCA/022/117/705/GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly/GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly_genomic.fna.gz
#    gunzip GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly_genomic.fna.gz
#    perl -p -i -e 's/CM0.*chromosome /chr/' GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly_genomic.fna
#    perl -p -i -e 's/, whole.* //' GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly_genomic.fna
#    perl -p -i -e 's/sequence//' GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly_genomic.fna
#    mv GCA_022117705.1_Zm-Mo17-REFERENCE-CAU-T2T-assembly_genomic.fna Mo17_T2T.fa
  fi

  gb_fa=$line_dir$line_code'.fa'
  echo $gb_fa
  #gb_gz=$gb_fa'.gz'
  #gunzip -k $gb_gz

  samtools faidx $gb_fa

  for ch in {1..10}
   do
    ch_fa=$line_dir'/'$line_code'_chr'$ch'.fa'
    ch_gz=$ch_fa'.gz'
    if [ ! -e $ch_gz ]; then
     samtools faidx $gb_fa 'chr'$ch -o $ch_fa
     makeblastdb -in $ch_fa -dbtype nucl
     bgzip $ch_fa -@ 8
     samtools faidx $ch_gz
    fi
   done
  rm $gb_fa
  rm $gb_fa".fai"
done