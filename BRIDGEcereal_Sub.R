# Sub functions (03/31/23)
# 
# 1, For_extract_fa() for extracting .fa in both CDS and largefile conditions
# 2, For_upload_file() for uploading Parent1 and Parent2 chr or contigs
# 3, CHOICE
# 4, CLIPS(); 5, Slope_by_HSPs(); 6, Remove_CrossOverMatch()
#
# 7, For_dynamic_input() for User defined tree-cut, arrangement of plot order and to generate variety table ... 
# 8, For_trimming_CDS(); 9, For_trimming_gDNA(); 10, For_trimming_Gap(); 11, For_trimming_repeat(); for trimming step ...
# 12, Plot_SV() for plotting
#
#################################################################
#################################################################
For_extract_fa <-function(extract_flag,Genome,gff_folder_Species,Selected_gene,Users_folder,target_folder0,chromosome){

if(extract_flag==1){

G_gff_pattern <- paste(Genome,".*","_CDS_Working.gff3",sep="") ##
file0<-list.files(gff_folder_Species,pattern = G_gff_pattern);
file1<-read.table(paste(gff_folder_Species,file0,sep=''),header=F)
file2<-file1[which(file1[,9]==Selected_gene),c(1,4,5)]
file3<-paste(Users_folder,'/',"positions.txt",sep='')
write.table(file2,file3,row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")
strand_direction <- unique(file1[which(file1[,9]==Selected_gene),7])
target_folder1<-paste(target_folder0,Genome,"/",sep = "")
 samtools0<-as.data.frame(read.table(file3,header=F))
 samtools1<-paste(samtools0$V1,':',samtools0$V2,'-',samtools0$V3,sep='')
 samtools2 <- paste(target_folder1,Genome,"_",chromosome,".fa.gz",sep = ""); # ref
 for(member in 1:length(samtools1)){
 system_samtools0<- paste('samtools faidx',samtools2,samtools1[member],'>> ', paste(Users_folder,'/','query0.fa',sep='') ,sep=' ') 
 system(system_samtools0)
}
 merge_fa1<-readDNAStringSet(paste(Users_folder,'/','query0.fa',sep='')) #11/8/22
 merge_fa2<-DNAStringSet(unlist(merge_fa1))
 names(merge_fa2)<-c('query')
 system( paste('rm ',paste(Users_folder,'/','query0.fa',sep=''), sep=' ') )
query_COPY<-merge_fa2;
query_COPY2<-merge_fa2;
Gene<-Selected_gene;
query_length<-length(query_COPY[[1]]);
Name_update_1 <- paste(Gene,"_mRNA",sep = "");
Name_update_2 <- paste(Gene,"_CDS",sep = "");
names(query_COPY)<-Name_update_1;
names(query_COPY2)<-Name_update_2;
GeneRef<-c(query_COPY,query_COPY2);

##
if(strand_direction=='-'){
writeXStringSet(reverseComplement(query_COPY2),paste(Users_folder,'/', Gene,'_CDS',".fa",sep=''), append=FALSE, compress=FALSE, format="fasta");
writeXStringSet(reverseComplement(GeneRef),paste(Users_folder,'/',Gene,'_ref',sep = ''), append=FALSE, compress=FALSE, format="fasta");
} else if(strand_direction=='+'){
writeXStringSet(query_COPY2,paste(Users_folder,'/', Gene,'_CDS',".fa",sep=''), append=FALSE, compress=FALSE, format="fasta");
writeXStringSet(GeneRef,paste(Users_folder,'/',Gene,'_ref',sep = ''), append=FALSE, compress=FALSE, format="fasta");
}

#writeXStringSet(GeneRef,paste(Users_folder,'/',Gene,'_ref',sep = ''), append=FALSE, compress=FALSE, format="fasta");

my_list <- list(strand_direction,query_length)
return(my_list)

}

}
#################################################################
#################################################################
For_upload_file <-function(Users_folder, upload_name, Parent){

          fileName0<- paste(Users_folder,'/',upload_name,sep='')
          fileName1 <- as.character(strsplit(fileName0, ".fa.gz")) 
          uncompressed0 <- paste("gzip -d",fileName0, sep=' ') 
          uncompressed1 <- system(uncompressed0)
          fileName2 <- paste(fileName1,'.fa',sep='')
          fileName3 <- paste(fileName1,'.fa.gz',sep='')
          makedb0 <- paste('makeblastdb -in', fileName2 ,'-dbtype nucl',sep=' ') 
          makedb1 <- system(makedb0)
          bgzip0 <- paste('bgzip -@ 8 ',fileName2,sep=' ')
          bgzip1 <- system(bgzip0)
          samtools0 <- paste('samtools faidx',fileName3 ,sep=' ')
          samtools1 <- system(samtools0)
          ParentName0<- as.character(strsplit(upload_name, ".fa.gz"))
          ParentName1<- gsub('_.*','',ParentName0)
          dir0<- paste(Users_folder,'/',Parent,sep='')
          dir.create(dir0)
          move0<-list.files(Users_folder, pattern=paste(Parent,'_',sep='') )
          for(i in 1:length(move0)){
            move1 <- paste(Users_folder,'/',move0[i],sep='')
            move2<- paste('mv',move1,dir0,sep=' ')
            system(move2)
            }


}

#################################################################
#################################################################
CHOICE<- function(BlastSynWorking,query_length, distance_filter, Min_CDS_size_filter, Max_CDS_size_filter, ref_g) {
 
 CHOICE_input_file<-BlastSynWorking

 CHOICE_output_list <- list() 
 CHOICE_cluster_list<-list() 

 index_genome<-1 

 for(g_index in unique(CHOICE_input_file$Genome)){

   Target_g<- CHOICE_input_file[which(CHOICE_input_file$Genome==g_index), ]

    if(nrow(Target_g)>=2){

     Total_clusters<-length( unique( cutree( hclust(dist(Target_g[,6]) ), h = distance_filter) ) ) 
     
     hclust_similarity<-list() 
     hclust_size<-list() 
     hclust_members<-matrix(nrow=Total_clusters,ncol=1)

      for(cluster_index in 1:Total_clusters) {
       hclusters <-Target_g[ which( cutree(hclust(dist(Target_g[,6])), k =Total_clusters, h = distance_filter ) == cluster_index), ]
       hclust_similarity[cluster_index] <- mean(hclusters[,9])  
       hclust_size[cluster_index] <- sum(hclusters[,8])   
       hclust_members[cluster_index,1]<-nrow(hclusters) 
       }

     MeanSimilarity_cdsSize<-matrix(nrow=Total_clusters,ncol=2) 

      for(cluster_index in 1:Total_clusters) {
       MeanSimilarity_cdsSize[cluster_index,1]<-hclust_similarity[[cluster_index]]
       MeanSimilarity_cdsSize[cluster_index,2]<-hclust_size[[cluster_index]]
       }

     MeanSimilarity_cdsSize[,2]<-MeanSimilarity_cdsSize[,2]/query_length
     Size_filter<-which(MeanSimilarity_cdsSize[,2]>=Min_CDS_size_filter & MeanSimilarity_cdsSize[,2]<=Max_CDS_size_filter)
     
     if(length(Size_filter)>1){                                                               
      Similarity_filter<-which( max( MeanSimilarity_cdsSize[Size_filter,][,1] ) == MeanSimilarity_cdsSize[,1] ) 
      } else {
        Similarity_filter<-which( max( MeanSimilarity_cdsSize[Size_filter,][1] ) == MeanSimilarity_cdsSize[,1]  )
        }

     Target_cluster<-intersect(Size_filter,Similarity_filter)

     if(length(Target_cluster)==0){                             
      Ideal_Size <- 1.0
      Target_cluster<- which( abs(MeanSimilarity_cdsSize[,2] - Ideal_Size) == min( abs(MeanSimilarity_cdsSize[,2] - Ideal_Size) ) )
      } else if(length(Target_cluster)>1){
        Target_cluster<-which( max(MeanSimilarity_cdsSize[Target_cluster,][,1]) == MeanSimilarity_cdsSize[,1] )
        }

     CHOICE_output_list[[index_genome]]<-Target_g[ which( cutree( hclust( dist(Target_g[,6])), k =Total_clusters, h = distance_filter ) == Target_cluster), ]

     CHOICE_output_summary<-matrix(nrow=Total_clusters,ncol=9)
     CHOICE_output_summary[ ,1]<-rep(g_index, Total_clusters)
     CHOICE_output_summary[1,2]<-nrow(Target_g)
     CHOICE_output_summary[1,3]<-distance_filter/1000
     CHOICE_output_summary[1,4]<-Total_clusters
     CHOICE_output_summary[ ,5]<-1:nrow(MeanSimilarity_cdsSize)
     CHOICE_output_summary[ ,6]<-hclust_members
     CHOICE_output_summary[ ,7]<-round(MeanSimilarity_cdsSize[,1], digits = 3)
     CHOICE_output_summary[ ,8]<-round(MeanSimilarity_cdsSize[,2], digits = 3)
     CHOICE_output_summary[Target_cluster,9]<-c("Selected")
     colnames(CHOICE_output_summary)<-c("Genomes","HSPs","HeightCut (kb)","TotalClusters","ClusterIndex","Members","MeanSimilarity",paste("TotalLength","/",ref_g,sep=''),"CandidateCluster")
     CHOICE_output_Table<-as.data.frame(CHOICE_output_summary)
     CHOICE_cluster_list[[index_genome]]<-CHOICE_output_Table    # for table1
     } else {
       CHOICE_output_list[[index_genome]]<-CHOICE_input_file[ which(CHOICE_input_file$Genome==g_index), ] 
       }

      index_genome<-index_genome+1

                }

   Filtered_By_CHOICE <-as.data.frame(rbindlist(CHOICE_output_list))

   #if(length(CHOICE_cluster_list)!=0){
   #CHOICE_summary_table <-as.data.frame(rbindlist(CHOICE_cluster_list))
   #  }
   #my_list <- list(Filtered_By_CHOICE,CHOICE_summary_table)

   my_list <- list(Filtered_By_CHOICE,CHOICE_cluster_list)
   
   return(my_list)

}
#################################################################
#################################################################
CLIPS <- function(gDNAs_blast_) {
 #self_idx <- which(gDNAs_blast_[,1] == gDNAs_blast_[,2] & gDNAs_blast_[,3] < 100)
 #g_selfs <- gDNAs_blast_[self_idx,]
 #gDNAs_blast_0 <- gDNAs_blast_[-self_idx,]
 #genomes <- as.vector(unique(gDNAs_blast_[,1]));
 
 genomes <- as.vector(unique(gDNAs_blast_[,1]));

 self_idx <- which(gDNAs_blast_[,1] == gDNAs_blast_[,2])
 g_selfs <- gDNAs_blast_[self_idx,]
 
 #g_selfs <- g_selfs[-match(genomes, g_selfs[,1]),] ## remove the first entry of each self blast
  g_selfs <- g_selfs[(g_selfs[,7] - g_selfs[,9] + g_selfs[,8] - g_selfs[,10] != 0),]

 
 gDNAs_blast_0 <- gDNAs_blast_[-self_idx,]

 n_g <- length(genomes)  
 b_matrix <- diag(n_g)
 for (g1 in 1:(n_g - 1)) {
  g1_self <- subset(g_selfs, g_selfs[,1] == genomes[g1])
   for (g2 in (g1 + 1): n_g ) {
    g2_self <- subset(g_selfs, g_selfs[,1] == genomes[g2]) 
    b_matrix[g1, g2] <- Slope_by_HSPs(gDNAs_blast_0, genomes[g1], genomes[g2], g1_self, g2_self)
    b_matrix[g2, g1] <- Slope_by_HSPs(gDNAs_blast_0, genomes[g2], genomes[g1], g2_self, g1_self)
   }
 }
 colnames(b_matrix) <- genomes
 rownames(b_matrix) <- genomes
 #h_c <- hclust(dist(b_matrix))
 return (b_matrix)
}

Slope_by_HSPs <- function(gDNAs_, g1_, g2_, g1_self_, g2_self_) {
 gDNAs <- subset(gDNAs_, gDNAs_[,1] == g1_ & gDNAs_[,2] == g2_)
 exact_match <- unique(c(which(gDNAs[,7] %in% g1_self_[,7] & gDNAs[,8] %in% g1_self_[,8]), which(gDNAs[,9] %in% g2_self_[,7] & gDNAs[,10] %in% g2_self_[,8])))
 if (length(exact_match) > 0) { gDNAs <- gDNAs[-exact_match,]}
 if (nrow(g1_self_) > 0 & nrow(g2_self_) > 0 & nrow(gDNAs) > 1) { gDNAs <- Remove_CrossOverMatch(gDNAs, g1_self_, g2_self_) }
 q_hits <- c(gDNAs[,7], gDNAs[,8]); s_hits <- c(gDNAs[,9,], gDNAs[,10])
 b_ <- round(lm(q_hits ~ s_hits)$coeff[2], 2)
 return (b_)
}

Remove_CrossOverMatch <- function(gDNAs_, g1_self_, g2_self_) {
 rep_tag <- c()
 for (i in 1:nrow(gDNAs_)) {
  for (j in 1:nrow(g1_self_)) {
   g1_s_e <- sort(c(g1_self_[j,7], g1_self_[j,8], gDNAs_[i,7], gDNAs_[i,8]))
   d_s <- abs(g1_s_e[1] - g1_s_e[2]) + abs(g1_s_e[3] - g1_s_e[4]);
   if (d_s < 20) { rep_tag <- append(rep_tag, i); break} ## 20 can be changed
  }
   ## check the subject columns might be reduant, but just be safe
  for (k in 1:nrow(g2_self_)) {
   g2_s_e <- sort(c(g2_self_[k,7], g2_self_[k,8], gDNAs_[i,9], gDNAs_[i,10]))
   d_s <- abs(g2_s_e[1] - g2_s_e[2]) + abs(g2_s_e[3] - g2_s_e[4]);
   if (d_s < 20) { rep_tag <- append(rep_tag, i); break} ## 20 can be changed
  }
 }
 if (length(rep_tag) > 0) { gDNAs_ <- gDNAs_[-unique(rep_tag),] }
 return(gDNAs_);
}
#################################################################
#################################################################
For_dynamic_input <-function(Dir,list1, list2, list2_, dynamic_flag, tree_cut){
 
 if(dynamic_flag==1){
 

 #list1<-read.table(paste(Dir, 'list1.txt', sep = ''),header=F)$V1

 list2<-c(list2,list1)

 b_matrix<-read.table(paste(Dir, 'b_matrix_ori.txt', sep = ''),header=TRUE,sep="\t")
 h_c <- hclust( dist( round(b_matrix,2) ) )

 #User_tree_cut<-read.table(paste(Dir, 'User_tree_cut.txt', sep = ''),header=FALSE)$V1
 User_tree_cut<-tree_cut
 
 memb<-cutree(as.dendrogram(h_c), h=User_tree_cut )
 d1<-as.dendrogram(h_c)%>% get_nodes_attr("label")
 d2<-as.data.frame(memb)
 d3<-as.data.frame(d1[which(d1!="NA")])
 d4<-cbind(d2,d3)
 colnames(d4)<-c("cluster","tree_order")
 d5<-cbind(d4, as.data.frame(rownames(d4)))
 colnames(d5)<-c("cluster","tree_order","cluster_order")
 d6<-d5[,c(3,1,2)]
 d6[order(d6[,2]),]
 d6[d6$tree_order,]
 d7<-d6[d6$tree_order,][,1:2]

 b_matrix0<-round(dist( round(b_matrix,2), diag = TRUE, upper = TRUE ) ,2) #03/13/23
 b_matrix0<-as.data.frame(as.matrix(b_matrix0)) #03/13/23

 b_matrix_selected0<-b_matrix0[which(match(rownames(b_matrix0) ,list2)!='NA'),which(match(colnames(b_matrix0) ,list2)!='NA')]

 list_temp <- list1

 variety_0<- read.table(paste(Dir, "b_matrix_variety_Table1.txt", sep = ''),header=T,sep = '\t')  ## b_matrix_groups3, b_matrix_variety_Table1.txt

 variety_0<-variety_0[ with(variety_0, order(variety_0$genomes) ),  ]  # 03/03/23

 b_matrix_groups2_update<-read.table(paste(Dir, "b_matrix_groups2_ori.txt", sep = ''),header=T,sep = '\t')

 for(list1 in list_temp){

   bucket_filtered0 <-as.data.frame(cbind(rownames(b_matrix_selected0),b_matrix_selected0[,list1]))
   bucket_filtered1 <-bucket_filtered0[which(bucket_filtered0$V1!=list1),]

   bucket_filtered2<- bucket_filtered1[order(bucket_filtered1$V2,decreasing = FALSE),][1,1]  #03/13/23

   cluster_removed<-d7[which(d7$cluster_order==list1),2] ## cluster number
   num_list_1<-length(which(d7$cluster==cluster_removed)) ## Number of members in the cluster_removed
   mem_list_1<-d7[which(d7$cluster==cluster_removed),1] ## members in the cluster_removed

   attach_group0<-variety_0[which(variety_0$genomes==bucket_filtered2),2] # CML247 member ID
   replace_group0<-variety_0[which(variety_0$genomes==list1),2] # B97 member ID
   variety_0[which(variety_0$memb==replace_group0),2]<-attach_group0

   b_matrix_groups4 <- variety_0
   write.table(b_matrix_groups4, file=paste(Dir, 'b_matrix_variety_Table1_.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t") # 2/27/23

   colnames(b_matrix_groups4)<-c('genome','variety_group')
   b_matrix_groups4<-aggregate(b_matrix_groups4$genome~b_matrix_groups4$variety_group, b_matrix_groups4, paste, collapse=", ")

   b_matrix_groups4[ ,1]<-seq(1:nrow(b_matrix_groups4)) ## 03/03/23

   colnames(b_matrix_groups4)<-c('Variety groups','Varieties')

   replace_group0_number<- b_matrix_groups2_update[which(b_matrix_groups2_update$genomes_rep==list1), 3]
   b_matrix_groups2_update2<-b_matrix_groups2_update[which(b_matrix_groups2_update$genomes_rep!=list1), ]
   bucket_filtered2_number<-b_matrix_groups2_update2[which(b_matrix_groups2_update2$genomes_rep==bucket_filtered2),3]
   b_matrix_groups2_update2[which(b_matrix_groups2_update2$genomes_rep==bucket_filtered2),3]<- replace_group0_number+ bucket_filtered2_number

   write.table(b_matrix_groups2_update2,file=paste(Dir, 'b_matrix_groups2.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t")

   b_matrix_groups2_update<-read.table(paste(Dir, "b_matrix_groups2.txt", sep = ''),header=T,sep = '\t')

   b_matrix_selected0<-b_matrix_selected0[ which(rownames(b_matrix_selected0)!=list1), which(colnames(b_matrix_selected0)!=list1)]

   variety_0<- read.table(paste(Dir, "b_matrix_variety_Table1_.txt", sep = ''),header=T,sep = '\t')  ## b_matrix_groups3, b_matrix_variety_Table1.txt
   }

  d1_order <-list2_
  d2_order <-b_matrix_groups4
  d3_order_<-as.data.frame(matrix(nrow=length(d1_order),ncol=1))
  index_order<-1

  for(name in d1_order ){
  
     old_order0<- grep(name,d2_order[ ,2])

     if(length(old_order0)>1){

      temp_list <- strsplit(d2_order[ ,2], ', ')
       
        for(i_j in old_order0){

            if(length(which(name==temp_list[[i_j]]))==1){
               
               old_order0<-i_j

            } 

         } 
  
       }

     d3_order_[index_order, 1] <-  d2_order[old_order0, 2]

     index_order<-index_order+1

     }
 b_matrix_groups4[, 2]<- d3_order_[,1]

 b_matrix_groups4[,3]<-data.frame(representation=list2_)
 b_matrix_groups4<-b_matrix_groups4[ , c('Variety groups','representation','Varieties')]

 b_matrix_groups4_<- as.data.frame(matrix(nrow=nrow(b_matrix_groups4),ncol=1))

   for(memb_ in 1:nrow(b_matrix_groups4)){
    b_matrix_groups4_[memb_, 1]<-length(unlist(strsplit(unlist(strsplit(b_matrix_groups4$Varieties,' ,'))[memb_],', ',fixed=TRUE)))
    }
 b_matrix_groups4[,4]<- b_matrix_groups4_
 colnames(b_matrix_groups4)<-c('Variety groups','Representative','Varieties',"Members")
 b_matrix_groups4 <- b_matrix_groups4[  ,c("Variety groups", "Representative", "Members", "Varieties")]

 return(b_matrix_groups4)

} else if (dynamic_flag==2) {

 b_matrix_groups2_temp<-read.table(paste(Dir, "b_matrix_groups2_ori.txt", sep = ''),header=T,sep = '\t')

 write.table(b_matrix_groups2_temp,file=paste(Dir, 'b_matrix_groups2.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t")

 b_matrix_groups4 <- read.table(paste(Dir, "b_matrix_variety_Table1.txt", sep = ''),header=T,sep = '\t')

 b_matrix_groups4<-b_matrix_groups4[ with(b_matrix_groups4, order(b_matrix_groups4$genomes) ),  ]  # 03/03/23

 colnames(b_matrix_groups4)<-c('genome','variety_group')
 b_matrix_groups4<-aggregate(b_matrix_groups4$genome~b_matrix_groups4$variety_group, b_matrix_groups4, paste, collapse=", ")
 colnames(b_matrix_groups4)<-c('Variety groups','Varieties')

 d1_order <-list2
 d2_order <-b_matrix_groups4
 d3_order_<-as.data.frame(matrix(nrow=length(d1_order),ncol=1))
 index_order<-1

 for(name in d1_order ){
  
  old_order0<- grep(name,d2_order[ ,2])
      
   if(length(old_order0)>1){

      temp_list <- strsplit(d2_order[ ,2], ', ')
       
        for(i_j in old_order0){

            if(length(which(name==temp_list[[i_j]]))==1){
               
               old_order0<-i_j

            } 

        } 

   }

  d3_order_[index_order, 1] <-  d2_order[old_order0, 2]

  index_order<-index_order+1

 }
 b_matrix_groups4[, 2]<- d3_order_[,1]
### 03/03/23

### 03/06/23
 b_matrix_groups4[, 3]<-data.frame(representation=list2)
 b_matrix_groups4<-b_matrix_groups4[ , c('Variety groups','representation','Varieties')]
### 03/06/23

## 03/13/23
 b_matrix_groups4_<- as.data.frame(matrix(nrow=nrow(b_matrix_groups4),ncol=1))
 for(memb_ in 1:nrow(b_matrix_groups4)){
  b_matrix_groups4_[memb_, 1]<-length(unlist(strsplit(unlist(strsplit(b_matrix_groups4$Varieties,' ,'))[memb_],', ',fixed=TRUE)))
   }
 b_matrix_groups4[,4]<- b_matrix_groups4_
 colnames(b_matrix_groups4)<-c('Variety groups','Representative','Varieties',"Members")
 b_matrix_groups4 <- b_matrix_groups4[  ,c("Variety groups", "Representative", "Members", "Varieties")]
## 03/13/23

#my_list <- list()
#my_list <- return(b_matrix_groups4, b_matrix_groups2_update)
return(b_matrix_groups4)
}

}
#################################################################
#################################################################
For_trimming_CDS <-function(Dir,Gene,list2,CoordinateFilter0){

Name0<- list2

CDS_gDNA_blast <- read.table(paste(Dir, Gene, '_ref_CDS-Haplotype_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
####  for CDS_gDNA_blast
CoordinateFilter1<-CDS_gDNA_blast

CoordinateFilter2 <- CoordinateFilter1[which(CoordinateFilter1$V2 %in% Name0),]

CDS_gDNA_blast_New1 <- list()
index_coor1 <- 1

for(gname2 in unique(CoordinateFilter2$V2)){

CoordinateFilter3<- CoordinateFilter2[which(CoordinateFilter2$V2==gname2),]
Target1<-CoordinateFilter0[which(rownames(CoordinateFilter0)==unique(CoordinateFilter3$V2)),]

###############
for(i in 1:nrow(CoordinateFilter3)) {           


if(CoordinateFilter3[i,9]<CoordinateFilter3[i,10]){

     CoordinateFilter3_1 <- CoordinateFilter3[i, ]

    if (CoordinateFilter3_1$V9>=Target1$start & CoordinateFilter3_1$V9<=Target1$end & CoordinateFilter3_1$V10>=Target1$end) {
       
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1$V9-Target1$start+1    
     CoordinateFilter3_1$V10 <- Target1$end-Target1$start+1

  } else if (CoordinateFilter3_1$V10>=Target1$start & CoordinateFilter3_1$V10<=Target1$end & CoordinateFilter3_1$V9<=Target1$start) {
    
     CoordinateFilter3_1$V9 <- Target1$start-Target1$start+1
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1$V10-Target1$start+1

  } else if (CoordinateFilter3_1$V9>=Target1$start & CoordinateFilter3_1$V10<=Target1$end) {
           
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1$V9-Target1$start+1
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1$V10-Target1$start+1
     
  } else if (CoordinateFilter3_1$V9<=Target1$start & CoordinateFilter3_1$V10>=Target1$end){
     
     CoordinateFilter3_1$V9 <- Target1$start-Target1$start+1
     CoordinateFilter3_1$V10 <- Target1$end-Target1$start+1
     
  } else if (CoordinateFilter3_1$V9>=Target1$end | CoordinateFilter3_1$V10<=Target1$start){
     
     CoordinateFilter3_1$V9 <- -1
     CoordinateFilter3_1$V10 <- -1

  }

  CoordinateFilter3[i,9]<-CoordinateFilter3_1$V9
  CoordinateFilter3[i,10]<-CoordinateFilter3_1$V10


} else if (CoordinateFilter3[i,9]>CoordinateFilter3[i,10]){

    CoordinateFilter3_1 <- CoordinateFilter3[i, ]
    CoordinateFilter3_1_Temp_V10 <- CoordinateFilter3_1$V9    
    CoordinateFilter3_1_Temp_V9 <- CoordinateFilter3_1$V10

    if (CoordinateFilter3_1_Temp_V9>=Target1$start & CoordinateFilter3_1_Temp_V9<=Target1$end & CoordinateFilter3_1_Temp_V10>=Target1$end) {
       
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1_Temp_V9-Target1$start+1    
     CoordinateFilter3_1$V9 <- Target1$end-Target1$start+1

  } else if (CoordinateFilter3_1_Temp_V10>=Target1$start & CoordinateFilter3_1_Temp_V10<=Target1$end & CoordinateFilter3_1_Temp_V9<=Target1$start) {
    
     CoordinateFilter3_1$V10 <- Target1$start-Target1$start+1
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1_Temp_V10-Target1$start+1

  } else if (CoordinateFilter3_1_Temp_V9>=Target1$start & CoordinateFilter3_1_Temp_V10<=Target1$end) {
           
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1_Temp_V9-Target1$start+1
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1_Temp_V10-Target1$start+1
     
  } else if (CoordinateFilter3_1_Temp_V9<=Target1$start & CoordinateFilter3_1_Temp_V10>=Target1$end){
     
     CoordinateFilter3_1$V10 <- Target1$start-Target1$start+1
     CoordinateFilter3_1$V9 <- Target1$end-Target1$start+1

  } else if (CoordinateFilter3_1_Temp_V9>=Target1$end | CoordinateFilter3_1_Temp_V10<=Target1$start){
     
     CoordinateFilter3_1$V10 <- -1
     CoordinateFilter3_1$V9 <- -1
  }

  CoordinateFilter3[i,9]<-CoordinateFilter3_1$V9
  CoordinateFilter3[i,10]<-CoordinateFilter3_1$V10

} # else if 

} # for loop row

CDS_gDNA_blast_New1[[index_coor1]] <- CoordinateFilter3
index_coor1 <- index_coor1+1

} # for loop genome

CDS_gDNA_blast_left <- as.data.frame(rbindlist(CDS_gDNA_blast_New1)) ## Raw

write.table(CDS_gDNA_blast_left, file=paste(Dir, Gene, '_ref_CDS-Haplotype_out_m8_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = FALSE)


return(CDS_gDNA_blast_left)

}
#################################################################
#################################################################
For_trimming_gDNA <-function(Dir,Gene, list2,CoordinateFilter0){ 

gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
CoordinateFilter1 <- subset(gDNAs_blast, gDNAs_blast[,4]> 0)

Name0<- list2

CoordinateFilter1 <- CoordinateFilter1[which(CoordinateFilter1$V1 %in% Name0 & CoordinateFilter1$V2 %in% Name0), ]

gDNAs_blast_New0 <- list()

index_coor <- 1

for(gname in rownames(CoordinateFilter0)) {

CoordinateFilter3 <- CoordinateFilter1[which(CoordinateFilter1$V1==gname), ]

Target1<-CoordinateFilter0[which(rownames(CoordinateFilter0)==gname), ]

for(i in 1:nrow(CoordinateFilter3)) {

Target2<-CoordinateFilter0[which(rownames(CoordinateFilter0)==CoordinateFilter3[i,]$V2), ]

CoordinateFilter3_1 <- CoordinateFilter3[i, ]

## V7 and V8
  if(CoordinateFilter3_1$V7>=Target1$start & CoordinateFilter3_1$V7<=Target1$end & CoordinateFilter3_1$V8>=Target1$end) {
    
     CoordinateFilter3_1$V7 <- CoordinateFilter3_1$V7-Target1$start+1
     CoordinateFilter3_1$V8 <- Target1$end-Target1$start+1

  } else if(CoordinateFilter3_1$V8>=Target1$start & CoordinateFilter3_1$V8<=Target1$end & CoordinateFilter3_1$V7<=Target1$start) {
           
     CoordinateFilter3_1$V7 <- Target1$start-Target1$start+1
     CoordinateFilter3_1$V8 <- CoordinateFilter3_1$V8-Target1$start+1

  } else if(CoordinateFilter3_1$V7>=Target1$start & CoordinateFilter3_1$V8<=Target1$end) {
     
     CoordinateFilter3_1$V7 <- CoordinateFilter3_1$V7-Target1$start+1
     CoordinateFilter3_1$V8 <- CoordinateFilter3_1$V8-Target1$start+1

  } else if(CoordinateFilter3_1$V7<=Target1$start & CoordinateFilter3_1$V8>=Target1$end) {
     
     CoordinateFilter3_1$V7 <- Target1$start-Target1$start+1
     CoordinateFilter3_1$V8 <- Target1$end-Target1$start+1

  } else if(CoordinateFilter3_1$V7>=Target1$end | CoordinateFilter3_1$V8<=Target1$start ){
    
     CoordinateFilter3_1$V7 <- 0
     CoordinateFilter3_1$V8 <- 0

  }

  CoordinateFilter3[i,7]<-CoordinateFilter3_1$V7
  CoordinateFilter3[i,8]<-CoordinateFilter3_1$V8

## V7 and V8

## V9 and V10
if(CoordinateFilter3[i,9]<CoordinateFilter3[i,10]){
   
    CoordinateFilter3_1 <- CoordinateFilter3[i, ]

  if(CoordinateFilter3_1$V9>=Target2$start & CoordinateFilter3_1$V9<=Target2$end & CoordinateFilter3_1$V10>=Target2$end) {

     CoordinateFilter3_1$V9 <- CoordinateFilter3_1$V9-Target2$start+1
     CoordinateFilter3_1$V10 <- Target2$end-Target2$start+1

  } else if(CoordinateFilter3_1$V10>=Target2$start & CoordinateFilter3_1$V10<=Target2$end & CoordinateFilter3_1$V9<=Target2$start) {

     CoordinateFilter3_1$V9 <- Target2$start-Target2$start+1
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1$V10-Target2$start+1

  } else if(CoordinateFilter3_1$V9>=Target2$start & CoordinateFilter3_1$V10<=Target2$end) {
           
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1$V9-Target2$start+1
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1$V10-Target2$start+1

  } else if(CoordinateFilter3_1$V9<=Target2$start & CoordinateFilter3_1$V10>=Target2$end) {

     CoordinateFilter3_1$V9 <- Target2$start-Target2$start+1
     CoordinateFilter3_1$V10 <- Target2$end-Target2$start+1
  
  } else if(CoordinateFilter3_1$V9>=Target2$end | CoordinateFilter3_1$V10<=Target2$start ){

     CoordinateFilter3_1$V9 <- 0
     CoordinateFilter3_1$V10 <- 0

  }


  CoordinateFilter3[i,9]<-CoordinateFilter3_1$V9
  CoordinateFilter3[i,10]<-CoordinateFilter3_1$V10


} else if (CoordinateFilter3[i,9]>CoordinateFilter3[i,10]){         ######################

    CoordinateFilter3_1 <- CoordinateFilter3[i, ]
    CoordinateFilter3_1_Temp_V10 <- CoordinateFilter3_1$V9    
    CoordinateFilter3_1_Temp_V9 <- CoordinateFilter3_1$V10

  if(CoordinateFilter3_1_Temp_V9>=Target2$start & CoordinateFilter3_1_Temp_V9<=Target2$end & CoordinateFilter3_1_Temp_V10>=Target2$end) {

     CoordinateFilter3_1$V10 <- CoordinateFilter3_1_Temp_V9-Target2$start+1
     CoordinateFilter3_1$V9 <- Target2$end-Target2$start+1

  } else if(CoordinateFilter3_1_Temp_V10>=Target2$start & CoordinateFilter3_1_Temp_V10<=Target2$end & CoordinateFilter3_1_Temp_V9<=Target2$start) {

     CoordinateFilter3_1$V10 <- Target2$start-Target2$start+1
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1_Temp_V10-Target2$start+1

  } else if(CoordinateFilter3_1_Temp_V9>=Target2$start & CoordinateFilter3_1_Temp_V10<=Target2$end) {
           
     CoordinateFilter3_1$V10 <- CoordinateFilter3_1_Temp_V9-Target2$start+1
     CoordinateFilter3_1$V9 <- CoordinateFilter3_1_Temp_V10-Target2$start+1

  } else if(CoordinateFilter3_1_Temp_V9<=Target2$start & CoordinateFilter3_1_Temp_V10>=Target2$end) {

     CoordinateFilter3_1$V10 <- Target2$start-Target2$start+1
     CoordinateFilter3_1$V9 <- Target2$end-Target2$start+1

  } else if(CoordinateFilter3_1_Temp_V9>=Target2$end | CoordinateFilter3_1_Temp_V10<=Target2$start){

     CoordinateFilter3_1$V10 <- 0
     CoordinateFilter3_1$V9 <- 0
  }

  CoordinateFilter3[i,9]<-CoordinateFilter3_1$V9
  CoordinateFilter3[i,10]<-CoordinateFilter3_1$V10

} # elseif

} # for loop row

gDNAs_blast_New0[[index_coor]] <- CoordinateFilter3

index_coor<- index_coor+1

} # for loop genome 

gDNAs_blast_New1 <- as.data.frame(rbindlist(gDNAs_blast_New0)) ## new raw
gDNAs_blast_New2 <- gDNAs_blast_New1[which(gDNAs_blast_New1$V7!=0), ] # filter1

gDNAs_blast_left <- gDNAs_blast_New2[which(gDNAs_blast_New2$V9!=0), ] # filter2

write.table(gDNAs_blast_left, file=paste(Dir, Gene, '_Haplotype-Self_out_m8_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = FALSE)

return(gDNAs_blast_left)

}
#################################################################
#################################################################
For_trimming_Gap <-function(Dir,Gene, list2,CoordinateFilter0){

Name0<- list2

N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
CoordinateFilter1<-N_Gap


if(length(intersect(Name0,CoordinateFilter1$Genome)) == 0){

write.table(N_Gap, file=paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = TRUE)

N_Gap_left <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), sep = '\t', header = T, stringsAsFactors = F);


} else if(length(intersect(Name0,CoordinateFilter1$Genome)) != 0){

CoordinateFilter2 <- CoordinateFilter1[which(CoordinateFilter1$Genome %in% Name0),]

N_Gap_New1 <- list()
index_coor1 <- 1

for(gname2 in unique(CoordinateFilter2$Genome)){

CoordinateFilter3<- CoordinateFilter2[which(CoordinateFilter2$Genome==gname2),]

Target1<-CoordinateFilter0[which(rownames(CoordinateFilter0)==unique(CoordinateFilter3$Genome)),]

N_Gaps1 <- matrix(nrow=nrow(CoordinateFilter3), ncol=3, byrow=TRUE)

for(i in 1:nrow(CoordinateFilter3)){

if(CoordinateFilter3[i,]$GAP_Start>Target1$end | CoordinateFilter3[i,]$GAP_End<Target1$start){

N_Gaps1[i,1]<-''
N_Gaps1[i,2]<- ''
N_Gaps1[i,3]<- ''

} else if (CoordinateFilter3[i,]$GAP_Start<Target1$start & CoordinateFilter3[i,]$GAP_End>Target1$end){

N_Gaps1[i,1]<-gname2
N_Gaps1[i,2]<-Target1$start-Target1$start+1
N_Gaps1[i,3]<-Target1$end-Target1$start+1
} else if (CoordinateFilter3[i,]$GAP_Start>Target1$start & CoordinateFilter3[i,]$GAP_End<Target1$end){

N_Gaps1[i,1]<-gname2
N_Gaps1[i,2]<-CoordinateFilter3[i,]$GAP_Start-Target1$start+1
N_Gaps1[i,3]<-CoordinateFilter3[i,]$GAP_End-Target1$start+1
} else if (CoordinateFilter3[i,]$GAP_Start>Target1$start & CoordinateFilter3[i,]$GAP_Start<Target1$end & CoordinateFilter3[i,]$GAP_End>Target1$end ){

N_Gaps1[i,1]<-gname2
N_Gaps1[i,2]<-CoordinateFilter3[i,]$GAP_Start-Target1$start+1
N_Gaps1[i,3]<-Target1$end-Target1$start+1
} else if (CoordinateFilter3[i,]$GAP_End>Target1$start & CoordinateFilter3[i,]$GAP_End<Target1$end & CoordinateFilter3[i,]$GAP_Start<Target1$start){

N_Gaps1[i,1]<-gname2
N_Gaps1[i,2]<-Target1$start-Target1$start+1
N_Gaps1[i,3]<-CoordinateFilter3[i,]$GAP_End-Target1$start+1
}

} # for loop

colnames(N_Gaps1)<-c("Genome","GAP_Start","GAP_End")

N_Gap2 <- as.data.frame(N_Gaps1) ## new input files

N_Gap_New1[[index_coor1]] <- N_Gap2

index_coor1 <- index_coor1+1

} # for loop

N_Gap_left <- as.data.frame(rbindlist(N_Gap_New1)) ## new input files
write.table(N_Gap_left, file=paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = TRUE)


N_Gap_left<-read.table(paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
return(N_Gap_left)

} # elseif
####
######### gap

}
#################################################################
#################################################################
For_trimming_repeat <-function(Dir,Gene, list2,CoordinateFilter0){

Name0<- list2

repmask <- read.table(paste(Dir, Gene, '_repMask2', sep = ''), header = F, sep = "\t", stringsAsFactors = F);
CoordinateFilter1 <- repmask[abs(repmask[,8] - repmask[,7])> 100,]

CoordinateFilter2 <- CoordinateFilter1[which(CoordinateFilter1$V1 %in% Name0),]

repmask_New1 <- list()
index_coor1 <- 1

for(gname2 in unique(CoordinateFilter2$V1)){

CoordinateFilter3<- CoordinateFilter2[which(CoordinateFilter2$V1==gname2),]


Target1<-CoordinateFilter0[which(rownames(CoordinateFilter0)==unique(CoordinateFilter3$V1)),]


CoordinateFilter3_1 <- CoordinateFilter3[which((CoordinateFilter3$V7>=Target1$start & CoordinateFilter3$V7<=Target1$end) | (CoordinateFilter3$V8>=Target1$start & CoordinateFilter3$V8<=Target1$end ) | (CoordinateFilter3$V7<=Target1$start & CoordinateFilter3$V8>=Target1$end ) ),]  ### filter

if(nrow(CoordinateFilter3_1)==0){

CoordinateFilter2_2 <-CoordinateFilter2

for(i in 1:nrow(CoordinateFilter2_2)){

CoordinateFilter2_2[i,7]<- -1
CoordinateFilter2_2[i,8]<- -1
}
repmask_New1[[index_coor1]] <- CoordinateFilter2_2
index_coor1 <- index_coor1+1
repmask <- as.data.frame(rbindlist(repmask_New1)) ## new input files

} else {

for(i in 1:nrow(CoordinateFilter3_1)){

 if (CoordinateFilter3_1[i,]$V7>=Target1$start & CoordinateFilter3_1[i,]$V7<=Target1$end & CoordinateFilter3_1[i,]$V8>=Target1$end) {
    
     CoordinateFilter3_1[i,]$V7 <- CoordinateFilter3_1[i,]$V7-Target1$start+1
     CoordinateFilter3_1[i,]$V8 <- Target1$end-Target1$start+1
  } else if (CoordinateFilter3_1[i,]$V8>=Target1$start & CoordinateFilter3_1[i,]$V8<=Target1$end & CoordinateFilter3_1[i,]$V7<=Target1$start) {
           
     CoordinateFilter3_1[i,]$V7 <- Target1$start-Target1$start+1
     CoordinateFilter3_1[i,]$V8 <- CoordinateFilter3_1[i,]$V8-Target1$start+1
  } else if (CoordinateFilter3_1[i,]$V7>=Target1$start & CoordinateFilter3_1[i,]$V8<=Target1$end) {
     
     CoordinateFilter3_1[i,]$V7 <- CoordinateFilter3_1[i,]$V7-Target1$start+1
     CoordinateFilter3_1[i,]$V8 <- CoordinateFilter3_1[i,]$V8-Target1$start+1
  } else if (CoordinateFilter3_1[i,]$V7<=Target1$start & CoordinateFilter3_1[i,]$V8>=Target1$end) {
     
     CoordinateFilter3_1[i,]$V7 <- Target1$start-Target1$start+1
     CoordinateFilter3_1[i,]$V8 <- Target1$end-Target1$start+1
  }

}

repmask_New1[[index_coor1]] <- CoordinateFilter3_1
index_coor1 <- index_coor1+1
repeat_left <- as.data.frame(rbindlist(repmask_New1)) ## new input files
repmask <- repeat_left
write.table(repeat_left, file=paste(Dir, Gene, '_repMask2_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = FALSE)


} # else

} # for loop


return(repmask)

}
#################################################################
#################################################################
Plot_SV <- function(genomes, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction, Query_coordinate, Near_Gene_coor,plotSV_options, b_matrix_groups2, query_length) {

# if (output_flag == 1) {png(file = paste(Dir, Gene, '_NAM.png', sep = ''), width= 19 * .5, height= 12 * .75 , pointsize= 10 , units = "in", res = 600)};
# if (output_flag == 1) { png(file = paste(Dir, Gene, '.png', sep = ''), width= 10, height= 11 ,res = 600, pointsize= 10, units = "in") };
 
  if (output_flag == 1) { png(file = paste(Dir, Gene, '.png', sep = ''), width= plotSV_options[1], height= plotSV_options[2] , pointsize= plotSV_options[3], res = 600, units = "in") };

 par(mar = c(1.2, 1.0, 0, 0) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .9, cex.lab = 1, family = "mono"); ## ??

 plot(-100, -100, xlim = x_lim, ylim = c(-0.5,length(genomes) + 0), xlab = '', ylab = '', xaxt = "n", yaxt = "n", bty = "n");

 for (g in 1:length(genomes)) {

## To add arrows and axis ##
   if (g == length(genomes)) {
      if(strand_direction=='+'){  
        arrows(range(gDNAs_blast[,9:10])[1], length(genomes)-0.5, range(gDNAs_blast[,9:10])[2], length(genomes)-0.5,lwd=2.5);
        } else if(strand_direction=='-'){
        arrows(range(gDNAs_blast[,9:10])[2], length(genomes)-0.5, range(gDNAs_blast[,9:10])[1], length(genomes)-0.5, lwd=2.5);
        }
     }


  if (g == length(genomes)) {

    if( (range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1]) >10*1000 ){                        #03/07/23

    axis(1, at = c( seq( from=range(gDNAs_blast[,9:10])[1],to=range(gDNAs_blast[,9:10])[2],
      by=(range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1])/(10-1)   ) ),
        # length.out=10   ) ),
      labels= paste0( c( round(  seq(from=range(gDNAs_blast[,9:10])[1]/1000, to=range(gDNAs_blast[,9:10])[2]/1000, length.out=10) , 0)    ),'kb')
     ,cex.axis=0.8,cex.lab=0.8,tick = TRUE,col = "blue", lty = 1, lwd = 2.5, lwd.ticks=1.5,tck =0.02,col.ticks = "black",col.axis = "black");

      } else if( (range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1] ) >=5*1000 & (range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1] ) <=10*1000){           #03/07/23

    axis(1, at = c( seq( from=range(gDNAs_blast[,9:10])[1],to=range(gDNAs_blast[,9:10])[2],
      by=(range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1])/(5-1)   ) ),
      labels= paste0(c( round(  seq(from=range(gDNAs_blast[,9:10])[1]/1000, to=range(gDNAs_blast[,9:10])[2]/1000,length.out=5) , 0)  )  ,'kb')
     ,cex.axis=0.8,cex.lab=0.8,tick = TRUE,col = "blue", lty = 1, lwd = 2.5, lwd.ticks=1.5,tck =0.02,col.ticks = "black",col.axis = "black");

      } else if( (range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1]) < 5*1000 & (range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1]) >1000 ){           #03/07/23

    axis(1, at = c( seq( from=range(gDNAs_blast[,9:10])[1],to=range(gDNAs_blast[,9:10])[2],
      by=(range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1])/(3-1)   ) ),
      labels= paste0(c( round(  seq(from=range(gDNAs_blast[,9:10])[1]/1000, to=range(gDNAs_blast[,9:10])[2]/1000,length.out=3) ,0 )  )  ,'kb')           
     ,cex.axis=0.8,cex.lab=0.8,tick = TRUE,col = "blue", lty = 1, lwd = 2.5, lwd.ticks=1.5,tck =0.02,col.ticks = "black",col.axis = "black");

      } else if( (range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1]) <= 1000 ){           #03/07/23

    axis(1, at = c( seq( from=range(gDNAs_blast[,9:10])[1],to=range(gDNAs_blast[,9:10])[2],
      by=(range(gDNAs_blast[,9:10])[2]-range(gDNAs_blast[,9:10])[1])/(2-1)   ) ),
      labels= paste0(c( round(  seq(from=range(gDNAs_blast[,9:10])[1], to=range(gDNAs_blast[,9:10])[2],length.out=2) ,0 )  )  ,'bp')
     ,cex.axis=0.8,cex.lab=0.8,tick = TRUE,col = "blue", lty = 1, lwd = 2.5, lwd.ticks=1.5,tck =0.02,col.ticks = "black",col.axis = "black");

     }

 }
## To add arrows and axis ##

  if (g < length(genomes)) {
   gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g + 1]);
   for (k in 1:nrow(gDNAs)) {
    polygon(c(gDNAs[k,7:8], gDNAs[k,c(10,9)]), length(genomes) - g - c(0.3, .3, 0.9, 0.9), col = adjustcolor( "gray", alpha.f = 0.5), border = "NA");
   }
  }
 
  ## for mRNA
  #for (cds_i in 1:length(cds_ids) ) {
  #  CDSs <- subset(CDS_gDNA_blast, CDS_gDNA_blast[,1] == paste(cds_ids[cds_i], '_mRNA', sep = '') & CDS_gDNA_blast[,2] == genomes[g] );
  #  arrow_code <- 1;
  #  if (CDSs[1,9] > CDSs[1, 10]) {arrow_code <- 2}
  #  if (nrow(CDSs) > 0) {
 ##    arrows(max(CDSs[,9:10]) + 100, length(genomes) - g - 0.175, min(CDSs[,9:10]) - 100, length(genomes) - g - 0.175, code = arrow_code, angle = 15, length = .05);
  #   rect(CDSs[,9], length(genomes) - g - 0.25, CDSs[,10] , length(genomes) - g - 0.1, col = "burlywood",  border = "NA");
  #  }
  # }

  ### for CDS
  for (cds_i in 1:length(cds_ids) ) {

    CDSs <- subset(CDS_gDNA_blast, CDS_gDNA_blast[,1] == paste(cds_ids[cds_i], '_CDS', sep = '') & CDS_gDNA_blast[,2] == genomes[g] );
    
  #  arrow_code <- 1;

    CDSs <- CDSs[ which(CDSs[,9]!=-1), ]

  #  if (CDSs[1,9] > CDSs[1, 10]) {arrow_code <- 2}

  #  if( length( unique(CDSs[,9]) ) >1){

    if (nrow(CDSs) > 0) {
    
  #  CDSs <- CDSs[which(CDSs[,9]!=-1), ]
     arrow_code <- 1;

     if (CDSs[1,9] > CDSs[1, 10]) {arrow_code <- 2}

     arrows(max(CDSs[,9:10]) + (query_length*0.4), length(genomes) - g - 0.175, min(CDSs[,9:10]) - (query_length*0.4), length(genomes) - g - 0.175, code = arrow_code, angle = 15, length = .1);
 
     rect(CDSs[,9], length(genomes) - g - 0.25, CDSs[,10] , length(genomes) - g - 0.1, col = cds_col[cds_i], border ="NA");

    }

  #   }

   }

  self <- subset(gDNAs_blast,  gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g ])
  sizes <- round((max(self[,7:8]) - min(self[,7:8]) )/ 1000, 2);

  if(haplotypes == 0){
 # legend(max(self[,7:8]),length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)', sep = '')), bty = "n", adj = c(0, 0), cex = .8 )
  legend(max(self[,7:8]),length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)', sep = '')), bty = "n", adj = c(0, 0), cex = .8 )
}

if(haplotypes == 1){

 #if(file.exists(paste(Dir, 'b_matrix_groups2.txt', sep = ''))){

 #b_matrix_groups2 <- read.table(paste(Dir, 'b_matrix_groups2.txt', sep = ''),header=T)

 haplotypes0 <- subset(b_matrix_groups2,  b_matrix_groups2$genomes_rep == genomes[g]) ## 09/26/22
 haplotypes1 <- haplotypes0$haplotypes_rep ## 09/26/22
 #legend(max(self[,7:8])+80,length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)',sep = ''), paste('(', haplotypes1, ' varieties)',sep = '') ),
 legend(max(x_lim)-1800, length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)',sep = ''), paste('(', haplotypes1, ' varieties)',sep = '') ), #03/13/23
 bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.0 ) ## 09/26/22
 
#}    #03/06/23 +80 ?? cex = 1.0

} 

   for (k in 1:nrow(self)) {
   if (self[k,7] == self[k,9] & self[k,8] == self[k,10]) {
    rect(self[k,7] - 100, length(genomes) - g, self[k,8] + 100, length(genomes) - g - 0.05, col = "darksalmon", border = "NA");
   } 
 
  gap <- subset(N_Gap, N_Gap[,1] == genomes[g]);
  if (nrow(gap) > 0) {
   segments(gap[,2], rep(length(genomes) - g - 0.025, nrow(gap)), gap[,3], rep(length(genomes) - g - 0.025, nrow(gap)), lty = 2, lwd=3, col = "black")
  }
  
  }

   # for annotation
 #  gIDs <- subset(Anno, Anno$Genome == genomes[g] & Anno$Type == 'gene');
 #  gCDS <- subset(Anno, Anno$Genome == genomes[g] & Anno$Type == 'CDS');
 #  if (nrow(gIDs) > 0) {
 #  arrows(gIDs$Start, rep(length(genomes) - g + 0.05, nrow(gIDs)), gIDs$End, rep(length(genomes) - g + 0.05, nrow(gIDs)), code = gIDs$Strand, angle = 15, length = .05, lwd = .5 )
 #   }
 #  if (nrow(gCDS) > 0) { 
 #   for (k in 1:nrow(gCDS)) {
 #    rect(gCDS$Start[k], length(genomes) - g + 0.02, gCDS$End[k], length(genomes) - g - 0.05 - 0.02, col = "darksalmon", lwd = .5)
 #   }
 #  };

if(repeats==1){
## Repeats
   if (nrow(repmask) > 0) {
   g_repmask <- subset(repmask, repmask[,1] == genomes[g]);
   if (nrow(g_repmask) > 0) {
    g_rep_y <- length(genomes) - g - 0.025 + 0.01 ;
    rect(g_repmask[,7], g_rep_y + 0.1, g_repmask[,8], g_rep_y +  0.2, col = "black", border = "NA");
    g_repmask_L <- g_repmask[abs(g_repmask[,8] - g_repmask[,7]) > 200,]
   #g_repmask_Harb <- subset(g_repmask, g_repmask[,7] == 'DNA/Harbinger')
 #   if(nrow(g_repmask_Harb) > 0) {text(rowMeans(g_repmask_Harb[,2:3]), rep(g_rep_y + 0.3, nrow(g_repmask_Harb)), g_repmask_Harb[,4], cex = .6, col = "red")}
 #    if(nrow(g_repmask_L) > 0) {text(rowMeans(g_repmask_L[,7:8]), rep(g_rep_y + 0.3, nrow(g_repmask_L)), g_repmask_L[,2], cex = .6)}
   }
  }
}

####
if(Query_coordinate[1,1]!=''){

#if( nrow(Query_coordinate)>0 ){

if(genomes[g]== Ref_genome){

rect(Query_coordinate[,1], length(genomes) - g - 0.09, Query_coordinate[,2] , length(genomes) - g +0.05, border ='blue')

if(Near_Gene_coor[1,1]!=''){
rect(Near_Gene_coor[,1], length(genomes) - g - 0.09, Near_Gene_coor[,2] , length(genomes) - g +0.05, border ='red', lwd=1.5 ) 
}

}

}
####


 }

 if (output_flag == 1) {dev.off()}
}
#################################################################
#################################################################