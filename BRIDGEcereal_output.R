# BRIDGEcereal_output functions (03/28/23)

BRIDGEcereal_output <- function(User_folder0,perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown, Backup_folder,strand_direction,database_folder,gff_folder,script_folder,User_folder) {

system1 <- paste("perl", paste(script_folder,'extract_syn_fa.pl',sep=''),perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown,Backup_folder, 1, 0,sep=' ');
system2 <- paste("perl", paste(script_folder,'extract_syn_fa.pl',sep=''),perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown,Backup_folder, 0, 2,sep=' ');

Dir <- paste(User_folder, User_folder0,'/',sep = '');

Dir0<-paste(User_folder, User_folder0,'/',paste(Gene,"_ref",sep = ""),sep = '');
system1_User<- paste(system1,Dir0,sep=' ')
system2_User<- paste(system2,Dir0,sep=' ')

system(system1_User);

######################################################################## To search for the outliers!
#Backup_folder_Gene<-paste(Backup_folder,Gene,'/',sep="");
Backup_folder_Gene<-paste(Backup_folder,'Candidate_genes','/',Gene,'/',sep="");


BlastSyn<-paste(Backup_folder_Gene,Gene,'_Haplotype_syn',sep = "");
Blast_Ori<-paste(Backup_folder_Gene,Gene,'_Blast_Original',sep = "");
BlastSynWorking<-read.table(BlastSyn,header=T); ## _Haplotype_syn
write.table(BlastSynWorking, file = Blast_Ori,sep= "\t",quote = FALSE,row.names = FALSE);


distance_filter<-input$Distancefilter
Min_CDS_size_filter <- input$CDSfilter[1]
Max_CDS_size_filter <- input$CDSfilter[2]
ref_g<-input$Pickgenome
#BlastSynWorking

source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
CHOICE_output <- CHOICE(BlastSynWorking, query_length, distance_filter, Min_CDS_size_filter, Max_CDS_size_filter, ref_g)
Filtered_HaplotypeSyn <- CHOICE_output[[1]]
Information_list<- CHOICE_output[[2]]

write.table(Filtered_HaplotypeSyn, file= BlastSyn,sep= "\t",quote = FALSE,row.names = FALSE);

#Filtered_HaplotypeSyn <-as.data.frame(rbindlist(Filter_list0))
#write.table(Filtered_HaplotypeSyn, file= BlastSyn,sep= "\t",quote = FALSE,row.names = FALSE);


##############################
if (input$Pickformat == "fasta_seq"){

BlastSynWorking_strand<-read.table(BlastSyn,header=T); ## _Haplotype_syn
Filtered_HaplotypeSyn_Strand0<-BlastSynWorking_strand[which(BlastSynWorking_strand$Genome==input$Pickgenome),c(6,7)][1, ] ## Only the first one
if( (Filtered_HaplotypeSyn_Strand0$subject_end-Filtered_HaplotypeSyn_Strand0$subject_start) >0 ){
    strand_direction<-'+';
} else if((Filtered_HaplotypeSyn_Strand0$subject_end-Filtered_HaplotypeSyn_Strand0$subject_start) <0){
    strand_direction<-'-';
}

}
##############################

if(length(Information_list)!=0){
Information_output0 <-as.data.frame(rbindlist(Information_list))
Information_output<-Information_output0[which(Information_output0$Genomes!=''),]
}

########
########
if(nrow(anti_join(BlastSynWorking,Filtered_HaplotypeSyn))!=0){
Outlier4<-anti_join(BlastSynWorking,Filtered_HaplotypeSyn); # differences of _Haplotype_syn (NEW or filtered) and _Blast_Original; Not shown in plot!
Outlier4_name <- unique(Outlier4[,4]);
Outlier4_name2 <- intersect(input$id,Outlier4_name);

}
######################################################################## To search for the outliers!
system(system2_User);
#Backup_folder_Gene<-paste(Backup_folder,Gene,'/',sep="");

Backup_folder_Gene<-paste(Backup_folder,'Candidate_genes','/',Gene,'/',sep="");
result_files<-list.files(Backup_folder_Gene, pattern = Gene)

#file.copy(file.path(paste(Backup_folder,Gene,'/',sep=""), result_files), perlArg4_Users_folder) # OK ????
file.copy(file.path(paste(Backup_folder,'Candidate_genes','/',Gene,'/',sep=""), result_files), perlArg4_Users_folder) # OK ????

####################### To check genomes
#BlastSyn<-paste(Gene,'_Haplotype_syn',sep = "");
#BlastSynWorking<-read.table(BlastSyn,header=T);
#Test_Genome0<-unique(BlastSynWorking[,4]);

#g_lab <- intersect(genomes,Test_Genome0);
#genomes_r <- intersect(genomes,Test_Genome0);
####################### To check genomes

##
Gene <- gsub(' ','',input$Gene)
d1_CDS<-read.table(paste(Dir, Gene, '_ref_mRNA-Haplotype_out_m8', sep = ''),header=F)
d2_CDS<-d1_CDS[ which(d1_CDS$V1==paste(Gene,'_','CDS',sep='')), ]
write.table(d2_CDS, file=paste(Dir, Gene, '_ref_CDS-Haplotype_out_m8', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = FALSE)
file.remove( paste(Dir, Gene, '_ref_mRNA-Haplotype_out_m8', sep = '') )
##

output$plot <- renderPlot({

output$plot2 <- NULL
output$plot3 <- NULL
output$plot4 <- NULL
output$info <- NULL
output$submit_trim <-NULL
output$info2 <- NULL
output$bucket <- NULL
output$Haplotypes <- NULL

Gene <- gsub(' ','',input$Gene)
cds_ids <- gsub(' ','',input$Gene)
Ref_genome <- input$Pickgenome

var_types <- c('snp', 'ins', 'del');
indel_col <- c("grey", "red", "red");
cds_col <- c("yellowgreen", "brown");

CDS_gDNA_blast <- read.table(paste(Dir, Gene, '_ref_CDS-Haplotype_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);

gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
gDNAs_blast <- subset(gDNAs_blast, gDNAs_blast[,4]> 0)

N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
#Anno <- read.table(paste(Dir, Gene, '_Haplotype_anno', sep = ''), sep = '\t', header = T, stringsAsFactors = F);

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repmask <- read.table(paste(Dir, Gene, '_repMask2', sep = ''), header = F, sep = "\t", stringsAsFactors = F);
#repmask <- repmask[abs(repmask[,8] - repmask[,7])> 100,]
repmask <- repmask[abs(repmask[,8] - repmask[,7])> 0,]
}

## 03/13/23
gDNAs_blast<-gDNAs_blast[which(gDNAs_blast$V1 %in% input$id), ]
gDNAs_blast<-gDNAs_blast[which(gDNAs_blast$V2 %in% input$id), ]
## 03/13/23


x_lim <- range(gDNAs_blast[,9:10]) + c(0, 2000)
output_flag = 0

#genomes <- sort(unique(gDNAs_blast[,2]));

genomes <- input$id ## from shiny input

####################### To check genomes
BlastSyn<-paste(Dir, Gene,'_Haplotype_syn',sep = "");
BlastSynWorking<-read.table(BlastSyn,header=T);
Test_Genome0<-unique(BlastSynWorking[,4]);
Test_Genome1 <- intersect(genomes,Test_Genome0);
if('query' %in% Test_Genome1){
 genomes <- c(sort(Test_Genome1[which(Test_Genome1 != 'query')]), 'query')  ##
} else {
 genomes <- Test_Genome1
}
####################### To check genomes

genomes_r <- genomes

g_lab <- genomes_r;

haplotypes <- 0
b_matrix_groups2<-''

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repeats<-1 ## 
}else {
repeats<-0 ##
}


### 03/02/23
if( file.exists(paste(Users_folder,'/',"positions.txt",sep='') ) ){

###### 2/22/23 
Query_coordinate<-read.table(paste(Users_folder,'/',"positions.txt",sep=''),header=F)
Query_coordinate <- cbind(Query_coordinate$V2-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp, Query_coordinate$V3-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp) #03/07/23
###### 2/22/23

######## 03/01/23
Query_coordinate_2<-read.table(paste(Users_folder,'/',"positions.txt",sep=''),header=F)

G_gff_pattern_2 <- paste(default_ref,".*","_gene_Working.gff3",sep="");
file0<-list.files(gff_folder_Species,pattern = G_gff_pattern_2);

if(strand_direction=='+'){

query_gene_up<-c(unique(Query_coordinate_2$V1),  range(Query_coordinate_2[,2:3])[1]-perlArg5_PickUp, range(Query_coordinate_2[,2:3])[1])    # chr s e
query_gene_down<-c(unique(Query_coordinate_2$V1),  range(Query_coordinate_2[,2:3])[2], range(Query_coordinate_2[,2:3])[2]+perlArg6_PickDown)

} else if(strand_direction=='-'){

#query_gene_up<-c(unique(Query_coordinate_2$V1),range(Query_coordinate_2[,2:3])[2], range(Query_coordinate_2[,2:3])[2]+perlArg5_PickUp)
#query_gene_down<-c(unique(Query_coordinate_2$V1),range(Query_coordinate_2[,2:3])[1]-perlArg6_PickDown, range(Query_coordinate_2[,2:3])[1])
query_gene_up<-c(unique(Query_coordinate_2$V1),  range(Query_coordinate_2[,2:3])[1]-perlArg5_PickUp, range(Query_coordinate_2[,2:3])[1])    # chr s e
query_gene_down<-c(unique(Query_coordinate_2$V1),  range(Query_coordinate_2[,2:3])[2], range(Query_coordinate_2[,2:3])[2]+perlArg6_PickDown)

}

up_down_combined<-as.data.frame(rbind(query_gene_up,query_gene_down))
write.table(up_down_combined, paste(Users_folder,'/',"up_down_combined.txt",sep=''), row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")
system_bedtools0<- paste('bedtools intersect -a ', paste(gff_folder_Species,file0,sep='') , ' -b ', paste(Users_folder,'/','up_down_combined.txt',sep=''), ' > ', paste(Users_folder,'/','Gene_captured.txt',sep='') ,sep = " ")
system(system_bedtools0)

remove_temp2<- paste('rm ',Users_folder,'/','up_down_combined.txt',sep='')
system(remove_temp2)

if(file.size(paste(Users_folder,'/','Gene_captured.txt',sep=''))!=0 ){

Near_gene0<-read.table(paste(Users_folder,'/','Gene_captured.txt',sep=''), header=F)

Near_gene1<-Near_gene0[which(Near_gene0$V9!=Gene), c(4,5)]

if(nrow(Near_gene1)>=1){

  if(strand_direction=='+'){
  
  Near_Gene_coor <<- cbind(Near_gene1$V4-(range(Query_coordinate_2[,2:3])[1]-perlArg5_PickUp), Near_gene1$V5-(range(Query_coordinate_2[,2:3])[1]-perlArg5_PickUp))
  
  }else if(strand_direction=='-'){

  #Near_Gene_coor <<-cbind((range(Query_coordinate_2[,2:3])[2]+perlArg5_PickUp)-Near_gene1$V5, (range(Query_coordinate_2[,2:3])[2]+perlArg5_PickUp)-Near_gene1$V4)
  Near_Gene_coor <<- cbind(Near_gene1$V4-(range(Query_coordinate_2[,2:3])[1]-perlArg5_PickUp), Near_gene1$V5-(range(Query_coordinate_2[,2:3])[1]-perlArg5_PickUp))

  }

} else if(nrow(Near_gene1)==0){
Near_Gene_coor <<- as.matrix(c(''),nrow=1,ncol=1)
}

} else {
Near_Gene_coor <<- as.matrix(c(''),nrow=1,ncol=1)
}

######## 03/01/23

} else {

Query_coordinate <<-as.matrix(c(''),nrow=1,ncol=1)
Near_Gene_coor <<- as.matrix(c(''),nrow=1,ncol=1)

}
######## 03/02/23

#Near_Gene_coor <- as.matrix(c(''),nrow=1,ncol=1) #3/22/23
plotSV_options<- as.numeric( c(10, 11, 10 ) )
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE); 
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction, Query_coordinate, Near_Gene_coor,plotSV_options,b_matrix_groups2)
##

#timer_end <- Sys.time();
#job_timer0 <- paste("Time used for your job: ", round(as.numeric(gsub('Time difference of ','',difftime(timer_end,timer_start))),2) ,' Seconds', '.',sep='')
#output$job_timer <- renderText({ job_timer0 })

Test_Up_down_stream_remainder <- paste("We suggest update preferred upstream and downstream inputs in this step ...",sep='')
output$Up_down_stream_remainder <- renderText({ Test_Up_down_stream_remainder })


 }, height = function() {length(input$id)*13+400} )  ## ## renderPlot from *10 to *13 03/06/23 for plot1


output$done <- renderUI({
    actionButton("done", label = "DONE",style = "background-color:#FF6666")
  })


output$clustertree <- renderUI({
    actionButton("clustertree", label = "(3) TREE",,style = "background-color:#3399FF")
  })


gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
#genomes <- input$id
#b_matrix_ <- CLIPS(gDNAs_blast,genomes)

b_matrix_ <<- CLIPS(gDNAs_blast)
#write.table(round(b_matrix_,2), file = paste(Dir, 'b_matrix_ori_.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t")

########################

observeEvent(input$clustertree,{

val <- reactiveValues(clickx = NULL, clicky = NULL)
  
  observe({

    input$plot2_click

    isolate({
      val$clickx = c(val$clickx, input$plot2_click$x)
      val$clicky = c(val$clicky, input$plot2_click$y) 
    })
  }) #adding clicks to list

output$plot3 <- NULL 
output$plot4 <- NULL 
output$info <- NULL 
output$submit_trim <-NULL 
output$info2 <- NULL
output$plot2 <- NULL  
output$Haplotypes <- NULL

#output$bucket <- NULL

output$table4 <- NULL # 2/28/23

output$plot2 <- renderPlot({

Gene <- gsub(' ','',input$Gene)  ## from shiny input

genomes <- input$id ## from shiny input

####################### To check genomes
BlastSyn<-paste(Dir, Gene,'_Haplotype-Self_out_m8',sep = "");
BlastSynWorking<-read.table(BlastSyn,header=F);
Test_Genome0<-unique(BlastSynWorking$V2);

Test_Genome1 <- intersect(sort(genomes),sort(Test_Genome0));

if('query' %in% Test_Genome1){
 genomes <- Test_Genome1[which(Test_Genome1 != 'query')]  ##
} else {
 genomes <- Test_Genome1
}
####################### To check genomes

#b_matrix_ <-read.table(paste(Dir, 'b_matrix_ori_.txt', sep = ''), header=T,sep="\t");

temp_b<-b_matrix_[which(row.names(b_matrix_) %in% genomes), ]
b_matrix<-temp_b[ ,which(colnames(temp_b) %in% genomes)]


write.table(round(b_matrix,2), file = paste(Dir, 'b_matrix_ori.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t")

#h_c <- hclust(dist(b_matrix));

h_c <- hclust( dist(round(b_matrix,2) ) ); #2/9/23

if(sum(as.matrix(dist(b_matrix)))!=0){

Title<-paste("Clustering on all haplotypes", "You can do tree cut on Height (y-axis) using single_click",sep='\n')
as.dendrogram(h_c) %>% set("labels_cex", 0.9) %>% sort(type = "nodes") %>% highlight_branches %>% plot(main = Title,ylab = "Height",horiz = FALSE);

} else if(sum(as.matrix(dist(b_matrix)))==0){
plot(x =c(0:100) , y =c(0:100) , xlab = '', ylab = '', xaxt = "n", yaxt = "n", bty = "n",type="n")
text(55, 25, labels='Calculated distance matrix ==0, only ONE haplotype!', cex=2.0,col="red") 
}


color_option <- c("blue","black","red","orange","grey","green")
abline(a=NULL, b=NULL, val$clicky, col=color_option[ceiling(as.numeric(val$clicky)) %% 6 +1],lty = 2,lwd=3 );


observeEvent(input$plot2_click,{



output$plot3 <- NULL
output$plot4 <- NULL
output$info <- NULL 
output$submit_trim <-NULL
output$info2 <- NULL
output$info4 <- NULL
output$info3 <- NULL

    memb<-cutree(as.dendrogram(h_c), h=input$plot2_click$y)
    
    #write.table(input$plot2_click$y ,file=paste(Dir, 'User_tree_cut.txt', sep = ''),row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")
    tree_cut <<- input$plot2_click$y

    b_matrix0 <- cbind(b_matrix, cluster =as.data.frame(memb)) 

    b_matrix_groups <- b_matrix0[,'memb',drop=FALSE]
    b_matrix_groups1<-b_matrix_groups[order(b_matrix_groups$memb), , drop = FALSE]

    b_matrix_groups3 <- cbind(rownames(b_matrix_groups1), data.frame(b_matrix_groups1, row.names=NULL))


  colnames(b_matrix_groups3)<-c('genomes','memb')

   Ref_row<-b_matrix_groups3[which(b_matrix_groups3$genomes==default_ref),] # 2/9/23
   NonRef_row<-b_matrix_groups3[which(b_matrix_groups3$genomes!=default_ref),] # 2/9/23
   b_matrix_groups3<-rbind(Ref_row,NonRef_row) #2/9/23


  uniq_genome<-list()
  for(i in unique(b_matrix_groups3$memb)){
  uniq_genome[[i]]<-b_matrix_groups3[which(b_matrix_groups3$memb==i),][1,]
  }
  b_matrix_groups2 <- as.data.frame(rbindlist(uniq_genome))

  memb_count<-b_matrix_groups1 %>% group_by(memb) %>% summarize(count=n())

  b_matrix_groups2 <- cbind(b_matrix_groups2,memb_count$count) ## 09/26/22
  colnames(b_matrix_groups2) <- c('genomes_rep','main_clusters','haplotypes_rep')
  
  write.table(b_matrix_groups2,file=paste(Dir, 'b_matrix_groups2.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t")
  write.table(b_matrix_groups2,file=paste(Dir, 'b_matrix_groups2_ori.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t") #2/28/23

  
info2_text<- paste0(c('Cut tree based on your selected height ~', round(input$plot2_click$y,2),',', 'with color:',color_option[ceiling(input$plot2_click$y) %% 6 +1],',', 'Click on "Plot selected haplotypes" to view haplotypes ...'), collapse= " ")
output$info2 <- renderText({info2_text})

#2/28/23 NULL
output$bucket <- renderUI({
    
    bucket_list(
      header = "Candidate haplotypes for plotting",
      group_name = "bucket_list_group",
      orientation = "horizontal",

    add_rank_list(text = "Empty bucket",
      labels =  NULL, 
      input_id = "list_1"),

    add_rank_list(text = "Order of plot",
                    labels = b_matrix_groups2$genomes_rep, 
                    input_id = "list_2")
    )  
  })


output$Haplotypes <- renderUI({
    actionButton("Haplotypes", "(4) Plot selected haplotypes",style = "background-color:#CCCCFF")
  })

############################ 2/9/23
b_matrix_groups4 <- b_matrix_groups3

b_matrix_groups4<-b_matrix_groups4[ with(b_matrix_groups4, order(b_matrix_groups4$genomes) ),  ]  # 03/03/23

colnames(b_matrix_groups4)<-c('genome','variety_group')
b_matrix_groups4<-aggregate(b_matrix_groups4$genome~b_matrix_groups4$variety_group, b_matrix_groups4, paste, collapse=", ")
colnames(b_matrix_groups4)<-c('Variety groups','Varieties')

write.table(b_matrix_groups3, file=paste(Dir, 'b_matrix_variety_Table1.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t") # 2/27/23
write.table(b_matrix_groups4, file=paste(Dir, 'b_matrix_variety_Table.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t") # 2/27/23


####################
####################
 d1_order <-b_matrix_groups2$genomes_rep

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

 b_matrix_groups4[, 3]<-data.frame(representation=d1_order)

 b_matrix_groups4<-b_matrix_groups4[ , c('Variety groups','representation','Varieties')]

 b_matrix_groups4_<- as.data.frame(matrix(nrow=nrow(b_matrix_groups4),ncol=1))
 for(memb_ in 1:nrow(b_matrix_groups4)){
  b_matrix_groups4_[memb_, 1]<-length(unlist(strsplit(unlist(strsplit(b_matrix_groups4$Varieties,' ,'))[memb_],', ',fixed=TRUE)))
   }
 b_matrix_groups4[,4]<- b_matrix_groups4_
 colnames(b_matrix_groups4)<-c('Variety groups','Representative','Varieties',"Members")
 b_matrix_groups4 <- b_matrix_groups4[  ,c("Variety groups", "Representative", "Members", "Varieties")]

 output$table4 <-DT::renderDataTable({
 datatable(b_matrix_groups4,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Variety Table: ', htmltools::em('Varieties in each group based on your tree cut')
  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=20, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top", columnDefs = list(list( className = 'dt-center', targets = "_all"))  )  ) %>% 
                             formatStyle(columns=ncol(b_matrix_groups4), target = c("cell"),backgroundColor = c("gold") ) %>% 
                             formatStyle('Representative',backgroundColor = styleEqual(d1_order, c('yellow')) ) %>% #03/13/23 Representation
                             formatStyle(columns =2:ncol(b_matrix_groups4)-1, fontSize = '150%') #03/13/23 2:3

   }) # DT::renderDataTable
####################
####################


 }) # input$plot2_click


  }, height = function() {length(input$id)*13+400}) ## renderplot2 from *10 to *13 03/06/23

 })   ## input$clustertree


#################### 2/28/23
####################
observeEvent( c(input$list_2,input$list_1) ,{


source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE); 

#write.table(input$list_2,file=paste(Dir, 'list2.txt', sep = ''),row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")
#write.table(input$list_1,file=paste(Dir, 'list1.txt', sep = ''),row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")

list2<-input$list_2
list1<-input$list_1

#list2<-read.table(paste(Dir, 'list2.txt', sep = ''),header=F)$V1

list2_<-list2


#if(file.size( paste(Dir, 'list1.txt', sep = '') )!=0){
if(length(list1) != 0){

 dynamic_flag<-1
 b_matrix_groups4<-For_dynamic_input(Dir,list1, list2,list2_,dynamic_flag,tree_cut)


 output$table4 <-DT::renderDataTable({
 datatable(b_matrix_groups4,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Variety Table: ', htmltools::em('Varieties in each group based on your tree cut')
  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=20, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top", columnDefs = list(list( className = 'dt-center', targets = "_all"))  )  ) %>% 
                             formatStyle(columns=ncol(b_matrix_groups4), target = c("cell"),backgroundColor = c("gold") ) %>% 
                             formatStyle('Representative',backgroundColor = styleEqual(list2_, c('yellow') ) ) %>% #03/13/23 Representation
                             formatStyle(columns =2:ncol(b_matrix_groups4)-1, fontSize = '150%') #03/13/23 2:3

  }) # DT::renderDataTable


#} else if(file.size( paste(Dir, 'list1.txt', sep = '') )==0){        # if input$list_1!=NULL
} else if( length(list1) == 0 ){

 
 dynamic_flag<-2
 b_matrix_groups4<-For_dynamic_input(Dir,list1, list2,list2_,dynamic_flag,tree_cut)


 output$table4 <-DT::renderDataTable({
 datatable(b_matrix_groups4,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Variety Table: ', htmltools::em('Varieties in each group based on your tree cut')
  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=20, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top", columnDefs = list(list( className = 'dt-center', targets = "_all"))  )  ) %>% 
                             formatStyle(columns=ncol(b_matrix_groups4), target = c("cell"),backgroundColor = c("gold") ) %>% 
                             formatStyle('Representative',backgroundColor = styleEqual(input$list_2, c('yellow')) ) %>% #03/13/23 Representation
                             formatStyle(columns =2:ncol(b_matrix_groups4)-1, fontSize = '150%') #03/13/23 2:3
  }) # DT::renderDataTable

} # else if

}) # observeEvent c(input$list_1, input$list_2)
#####################
##################### 2/28/23





######################
observeEvent(input$Haplotypes,{

output$info <- NULL
output$plot4 <- NULL
output$submit_trim <- NULL


output$extract_fa <- NULL
output$plotSV_parameters <- NULL



info4_text<- paste('Left single_click and right double_click on top of figure to select preferred coordinates for Trimming ... ',sep='')
output$info4 <- renderText({info4_text})

color_option <- c("blue","black","red","orange","grey","green")

val1 <- reactiveValues(clickx = NULL, clicky = NULL)
val2 <- reactiveValues(clickx = NULL, clicky = NULL)

 observe({

    input$plot3_click
    input$plot3_dblclick

    isolate({
      val1$clickx = c(val1$clickx, input$plot3_click$x)
      val1$clicky = c(val1$clicky, input$plot3_click$y) 
      
      val2$clickx = c(val2$clickx, input$plot3_dblclick$x)
      val2$clicky = c(val2$clicky, input$plot3_dblclick$y)
   })

  }) #adding clicks to list
##
Gene <- gsub(' ','',input$Gene)
cds_ids <- gsub(' ','',input$Gene)
Ref_genome <- input$Pickgenome

var_types <- c('snp', 'ins', 'del');
indel_col <- c("grey", "red", "red");
cds_col <- c("yellowgreen", "brown");

CDS_gDNA_blast <- read.table(paste(Dir, Gene, '_ref_CDS-Haplotype_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
gDNAs_blast <- subset(gDNAs_blast, gDNAs_blast[,4]> 0);

N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
#Anno <- read.table(paste(Dir, Gene, '_Haplotype_anno', sep = ''), sep = '\t', header = T, stringsAsFactors = F)

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repmask <- read.table(paste(Dir, Gene, '_repMask2', sep = ''), header = F, sep = "\t", stringsAsFactors = F);
repmask <- repmask[abs(repmask[,8] - repmask[,7])> 100,]
}



output_flag = 0
##

## 03/13/23
gDNAs_blast<-gDNAs_blast[which(gDNAs_blast$V1 %in% input$list_2), ]
gDNAs_blast<-gDNAs_blast[which(gDNAs_blast$V2 %in% input$list_2), ]
## 03/13/23
x_lim <- range(gDNAs_blast[,9:10]) + c(0, 2000)

output$plot3 <- renderPlot({

Genome_order <- input$list_2 ## New haplotypes's order
genomes_r <- Genome_order
genomes <- Genome_order
n_g <- length(genomes)
g_lab <- genomes_r;

haplotypes <- 1

 if(file.exists(paste(Dir, 'b_matrix_groups2.txt', sep = ''))){
   b_matrix_groups2 <<- read.table(paste(Dir, 'b_matrix_groups2.txt', sep = ''),header=T)
   }

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repeats<-1 ## 
}else {
repeats<-0 ##
}


# 03/07/23
if( file.exists(paste(Users_folder,'/',"positions.txt",sep='') ) ){

Query_coordinate<-read.table(paste(Users_folder,'/',"positions.txt",sep=''),header=F)
Query_coordinate<-cbind(Query_coordinate$V2-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp, Query_coordinate$V3-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp)

} else {

Query_coordinate <<-as.matrix(c(''),nrow=1,ncol=1)
Near_Gene_coor <<- as.matrix(c(''),nrow=1,ncol=1)

}

####################################################
#Near_Gene_coor <- as.matrix(c(''),nrow=1,ncol=1) #3/22/23
plotSV_options<- as.numeric( c(10, 11 , 10 ) )
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE); 
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction, Query_coordinate, Near_Gene_coor,plotSV_options,b_matrix_groups2) ## plot in shiny

arrows(as.numeric(val1$clickx), as.numeric(val1$clicky)+0.5, as.numeric(val1$clickx), as.numeric(val1$clicky)+0.1,length = 0.25, lwd=3,col=color_option[1])
arrows(as.numeric(val2$clickx), as.numeric(val2$clicky)+0.5, as.numeric(val2$clickx), as.numeric(val2$clicky)+0.1,length = 0.25, lwd=3,col=color_option[3])

recttext <- function(xl, yb, text, rectArgs = NULL, textArgs = NULL) {
  center<-c(xl,yb)
  do.call('text', c(list(x = center[1], y = center[2], labels = text), textArgs))
}

recttext(as.numeric(val1$clickx), as.numeric(val1$clicky)+0.7, 'left', textArgs = list(col = 'blue', cex = 1.5))
recttext(as.numeric(val2$clickx), as.numeric(val2$clicky)+0.7, 'Right',textArgs = list(col = 'red', cex = 1.5))


}, height = function() {length(input$id)*10+300}) ## ## renderplot3

######
observeEvent(input$plot3_click,{

Genome_order <- input$list_2 ## New haplotypes's order
genomes_r <- Genome_order
genomes <- Genome_order


############
for(i in 1:nrow(gDNAs_blast)) {           
if(gDNAs_blast[i,9]>gDNAs_blast[i,10]){  
   temp10<- gDNAs_blast[i,9]
   temp9<- gDNAs_blast[i,10]
   gDNAs_blast[i,10]<- temp10
   gDNAs_blast[i,9]<- temp9
}
}
############

x_p_s <- c();
x_p_s[1] <- floor(input$plot3_click$x[1]);
n_g <- length(genomes_r);
for (g1 in 1:(n_g - 1)) {
 gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes_r[g1] & gDNAs_blast[,2] == genomes_r[g1 + 1])
 gDNAs <- subset(gDNAs, gDNAs[,7] <= x_p_s[g1] & gDNAs[, 8] >= x_p_s[g1]);
 x_p_s[g1 + 1] <- gDNAs[1,9] - gDNAs[1,7] + x_p_s[g1];

}

observeEvent(input$plot3_dblclick,{

x_p_ss <- c();
x_p_ss[1] <- floor(input$plot3_dblclick$x[1]);
n_g <- length(genomes_r);
for (g1 in 1:(n_g - 1)) {
 gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes_r[g1] & gDNAs_blast[,2] == genomes_r[g1 + 1])
 gDNAs <- subset(gDNAs, gDNAs[,7] <= x_p_ss[g1] & gDNAs[, 8] >= x_p_ss[g1]);
 x_p_ss[g1 + 1] <- gDNAs[1,9] - gDNAs[1,7] + x_p_ss[g1];

}

Combined<-cbind(x_p_s,x_p_ss);
colnames(Combined)<-c("start","end");
rownames(Combined)<-genomes;

Combined<-rbind(Combined, average =colMeans(Combined, na.rm=FALSE))

write.table(Combined,file=paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''),sep="\t")

Combined <-read.table(paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''), header=T)

if(is.na(tail(Combined,1)[,1])) {

info_text<-paste('Please click/double click again ...',',','Coordinates not complete!',sep=' ')

} else if (is.na(tail(Combined,1)[,2])){

info_text<-paste('Please click/double click again ...',',','Coordinates not complete!',sep=' ')

} else {

info_text<-paste('left coordinate ~ ', round(as.numeric(Combined[1,1]/1000),2),'kb',';', ' right coordinate ~ ', round(as.numeric(Combined[1,2]/1000),2),'kb','.',' You can click on Trim Button ...',sep='')

}

output$info <- renderText({ info_text })

output$submit_trim <- renderUI({

        if(!is.na(tail(Combined,1)[,1]) & !is.na(tail(Combined,1)[,2])) {
           
            if(Combined[1,1] < Combined[1,2])   {

              actionButton("submit_trim", label = "(5) Trim",style = "background-color:#66FF66")

                     }
                }       

    })

#output$PNG <- renderUI({

#        if(!is.na(tail(Combined,1)[,1]) & !is.na(tail(Combined,1)[,2])) {
           
#            if(Combined[1,1] < Combined[1,2])  {
                
#                    downloadButton(outputId='PNG',label = "Check saved .png result",style = "background-color:#66FF66")
                
#                     }
#                }       

#    })

output$plotSV_parameters <- renderUI({

        if(!is.na(tail(Combined,1)[,1]) & !is.na(tail(Combined,1)[,2])) {
           
            if(Combined[1,1] < Combined[1,2])  {
                
                tagList(
                         column(4, sliderInput("width", "PNG width", min = 3, max = 12, step =1, value =8)),
                         column(4, sliderInput("height", "PNG height", min = 3, max = 12, step =1, value =9)),
                         column(4, sliderInput("pointsize", "PNG pointsize", min = 8, max = 15, step =1, value =10))
                        )
                
                     }
                }       

    })

output$extract_fa <- renderUI({

        if(!is.na(tail(Combined,1)[,1]) & !is.na(tail(Combined,1)[,2])) {
           
            if(Combined[1,1] < Combined[1,2])   {

              actionButton("extract_fa", label = "Extract trimmed fasta",style = "background-color:#FF99CC")
                     
                     }

                }       

    })

info3_text<- paste('You may reset arrows (selected coordinates) using button: "Plot selected haplotypes" ', sep="")
output$info3 <- renderText({info3_text})


})  ## plot3_dblclick

}) ## plot3_click

########### plot3_hover reveals repeats
if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {

Genome_order0 <- input$list_2
repmask_hover0 <- read.table(paste(Dir, Gene, '_repMask2', sep = ''), header = F, sep = "\t", stringsAsFactors = F);

if( length( intersect(repmask_hover0$V1, Genome_order0 ) ) >0 ){   #4/10/23

repmask_hover<-repmask_hover0[which(repmask_hover0$V1 %in% Genome_order0), ]

repmask_hover$V13<-match(repmask_hover$V1, Genome_order0)

repmask_hover<-repmask_hover[order(repmask_hover$V13),][,1:12]

Genome_order <- unique(repmask_hover$V1)

g_rep_y<-list()
for (g in 1:length(Genome_order0)) {
 g_rep_y[[g]] <-c(length(Genome_order0)-g)
}

repmask_hover <- repmask_hover[which(repmask_hover$V1 %in% Genome_order),c(1,2,7,8)]

hover_list<-list()
for (g in 1:length(Genome_order)) {
 repmask_hover0 <-repmask_hover[which(repmask_hover$V1 %in% Genome_order[g]), ]
 repmask_hover0[,5]<-as.numeric(g_rep_y[[g]][1])-0.25
 repmask_hover0[,6]<-as.numeric(g_rep_y[[g]][1])+0.25
 hover_list[[g]]<-repmask_hover0[,c(2:6)]
}
repmask_hover1<- as.data.frame(rbindlist(hover_list)) ## TEName, V7, V8, y-0.25, y+0.25

hover_tag<-1 #4/10/23

} else { #4/10/23

hover_tag<-0 #4/10/23

} #4/10/23


} ## repmask exists ..
###

if (hover_tag != 0 ) { #4/10/23

observeEvent(input$plot3_hover,{

#if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) { #4/10/23

repmask_hover2_reactive <- reactive({
   repmask_hover1[which(with(repmask_hover1, input$plot3_hover$y>=repmask_hover1[,4] & input$plot3_hover$y<=repmask_hover1[,5])), c(1,2,3) ]
     })
temp_hover<-repmask_hover2_reactive() 
repmask_hover3_reactive <- reactive({
   temp_hover[which(with(temp_hover, input$plot3_hover$x>=temp_hover[,2] & input$plot3_hover$x<=temp_hover[,3])), c(1) ]
 })
output$info_TE <- renderText({paste(c("Involved TE:", repmask_hover3_reactive()),collapse = '  ')})

 }) ## plot3_hover

} ## repmask exists ..#4/10/23
########### plot3_hover reveals repeats



})  ## input$Haplotypes


########## Start To Trim
observeEvent(input$submit_trim,{

                progress <- Progress$new(session, min=1, max=10)
                on.exit(progress$close())
                progress$set(message = 'Trimming selected regions ...',
                detail = 'Almost there...')
                for (i in 1:5) {
                progress$set(value = i)
                Sys.sleep(0.3)
                }

Gene <- gsub(' ','',input$Gene)
cds_ids <- gsub(' ','',input$Gene)
Ref_genome <- input$Pickgenome
var_types <- c('snp', 'ins', 'del');
indel_col <- c("grey", "red", "red");
cds_col <- c("yellowgreen", "brown");

list2<- input$list_2

CoordinateFilter0<-read.table(paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''),header=T) 
CoordinateFilter0<-round(CoordinateFilter0,0)
CoordinateFilter0<-CoordinateFilter0[which(row.names(CoordinateFilter0) %in% list2),] # 03/13/23

source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE); 

CDS_gDNA_blast <- For_trimming_CDS(Dir,Gene,list2,CoordinateFilter0)
gDNAs_blast <- For_trimming_gDNA(Dir,Gene,list2,CoordinateFilter0)
N_Gap<-For_trimming_Gap(Dir,Gene,list2,CoordinateFilter0)

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repmask <- For_trimming_repeat(Dir,Gene,list2,CoordinateFilter0)
}

## End of Trim

output$info_Trim0 <- renderText({ paste(" ",sep='') }) #03/07/23
output$info_Trim <- renderText({ paste("User trimmed graph for ",gsub(' ','',input$Gene),sep='' ) }) #03/07/23

#output$info_plotSV0 <- renderText({ paste(" ",sep='') }) 
output$info_plotSV <- renderText({ paste("(Optional) If you want to reformat your saved PNG file, please use associated parameters in left (below the Trim button)
 to adjust your preferred PNG format (... will be automatically saved in a downloadable file)", sep='' ) }) 

####
#Anno <- read.table(paste(Dir, Gene, '_Haplotype_anno', sep = ''), sep = '\t', header = T, stringsAsFactors = F)
#N_Gap <- read.table( paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), header=T ) 
#gDNAs_blast <- gDNAs_blast_left

#output_flag = 0

output_flag = 0 # 3/20/23

#b_matrix_groups2 <- read.table(paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''),header=T) 

genomes <- input$list_2
genomes_r <- genomes
n_g <- length(genomes)
g_lab <- genomes_r;

#x_lim <- range(CoordinateFilter0)+c(0,2000)
x_lim <- range(CoordinateFilter0)-range(CoordinateFilter0)[1] + 1 + c(0,2000)

haplotypes<-1

 if(file.exists(paste(Dir, 'b_matrix_groups2.txt', sep = ''))){
   b_matrix_groups2 <<- read.table(paste(Dir, 'b_matrix_groups2.txt', sep = ''),header=T)
   }

output$plot4 <- renderPlot({

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repeats<-1 ##
}else {
repeats<-0 ##
}


Query_coordinate<-as.matrix(c(''),nrow=1,ncol=1)
Near_Gene_coor <- as.matrix(c(''),nrow=1,ncol=1)

plotSV_options<- as.numeric( c(10, 11 , 10 ) )
####################################################
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE); 
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction, Query_coordinate, Near_Gene_coor,plotSV_options,b_matrix_groups2) ## plot in shiny

output_flag = 1 # # 3/20/23
plotSV_options<- as.numeric( c(input$width,input$height,input$pointsize ) )
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction, Query_coordinate, Near_Gene_coor,plotSV_options,b_matrix_groups2) # 1/5/23

#zip(paste(Dir,gsub(' ','',input$Gene),'.zip',sep=''), paste(Dir,gsub(' ','',input$Gene),'.png',sep=''), flags = "-r9Xj")

}, height = function() {length(input$list_2)*13+400}) #03/06/23 add a height for plot4

})  ## input$submit_trim  Trim
#############################################################################################
#############################################################################################



########################## A table showing clustering results
if(length(Information_list)!=0){

output$table1 <-DT::renderDataTable({
datatable(Information_output,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 1: ', htmltools::em('Clustering information based on Blast results (At least two places).')
  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=10, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top")) %>% formatStyle(columns=1:ncol(Information_output), target = c("cell"),backgroundColor = styleEqual(c("Selected"), c('lightblue')))
  }) # DT::renderDataTable

}
########################## A table showing clustering results

########################## A table showing Blast result which is presented in main plot
Filtered_HaplotypeSyn_Plotted <- Filtered_HaplotypeSyn[which(Filtered_HaplotypeSyn$Genome!=''),]

colnames(Filtered_HaplotypeSyn_Plotted) <- c("query","query start","query end","Genome","chromosome","subject start","subject end","size","similarity")

output$table2 <-DT::renderDataTable({
datatable(Filtered_HaplotypeSyn_Plotted,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 2: ', htmltools::em('Blast results used for plotting.')
  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=10, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top")) %>% formatStyle(columns=c(4,8,9), target = c("cell"), backgroundColor = c('yellow'))
  }) # DT::renderDataTable
########################## A table showing Blast result which is presented in main plot

########################## A table showing Blast result which is not presented in main plot
BlastSynWorking_0<-read.table(Blast_Ori,header=T); ## Blast_Original
BlastSynWorking_1 <- BlastSynWorking_0[which(BlastSynWorking_0$Genome!=''),]
#BlastSynWorking_2 <- BlastSynWorking[which(BlastSynWorking$Genome!=''),]
BlastSynWorking_2 <-Filtered_HaplotypeSyn[which(Filtered_HaplotypeSyn$Genome!=''),] # 1/5/23

NotShown0 <- anti_join(BlastSynWorking_1,BlastSynWorking_2) # Not shown in plot, other genomic positions.

colnames(NotShown0) <- c("query","query start","query end","Genome","chromosome","subject start","subject end","size","similarity")

if(nrow(NotShown0)!=0){

output$table3 <-DT::renderDataTable({
datatable(NotShown0, caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 3: ', htmltools::em('Blast results not shown in plot.')
  ),filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=10, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top")) %>% formatStyle(columns=c(4,8,9), target = c("cell"), backgroundColor = c('orange'))
  }) # DT::renderDataTable
}
########################## A table showing Blast result which is not presented in main plot ??

## Based on selected haplotypes and region
observeEvent(input$extract_fa,{

    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    progress$set(message = 'Extract trimmed fasta ...',
                 detail = 'Almost done...')
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.1)  ## ??
    }

Gene <- gsub(' ','',input$Gene)

CoordinateFilter0 <- read.table(paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''),header=T) 

CoordinateFilter0<-round(CoordinateFilter0,0)
Sel_Hap_Coor <- CoordinateFilter0[which(row.names(CoordinateFilter0)!='average'),] # remove average values

query_extract_fa<-list();

dna_Haplotype_fa <- readDNAStringSet(paste(Dir, Gene, '_Haplotype.fa', sep = ''));

for (x_lines in 1:nrow(Sel_Hap_Coor)) {
  query_name <- row.names(Sel_Hap_Coor[x_lines,]); 
  start_1 <- Sel_Hap_Coor[x_lines,][,1]; 
  end_1 <- Sel_Hap_Coor[x_lines,][,2];
  query_extract <- dna_Haplotype_fa[grepl(query_name, dna_Haplotype_fa@ranges@NAMES)];
  query_extract_fa[[x_lines]] <- subseq(query_extract, start=start_1, end=end_1);
  writeXStringSet(query_extract_fa[[x_lines]], file=paste0(Dir,x_lines, '_Selected_New.fa'));

  }

system_fasta0<-paste("cat ", paste(Dir,'*_Selected_New.fa',sep='')," > ", paste(Dir,Gene,'_User_Selected.fa',sep=''), sep=' ')
system(system_fasta0);
system( paste('rm ',paste(Dir,'*_Selected_New.fa',sep=''), sep='') )

  }) # observeEvent input$extract_fa
## Based on selected haplotypes and region


}  ## function