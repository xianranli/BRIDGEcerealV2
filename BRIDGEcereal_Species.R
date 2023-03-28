### 03/28/23
### BRIDGEcereal_Species() 

############ BRIDGEcereal_Species
options(shiny.maxRequestSize=300*1024^2) ## Max size of uploaded file (300Mb in this case)

BRIDGEcereal_Species <- function(Speciesx,default_ref,GeneExample,database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_Speciesx){

page_number <- gsub(' ', '/', paste(' ',Speciesx,sep=''))

########################################
page_title<-paste('This is BRIDGEcereal ',Speciesx,' Page',sep='');
page_subtitle<-paste(Speciesx,' Alignment output',sep='');
target_folder0<-paste(database_folder,Speciesx,'/',sep='');
default_choice <-list.files(target_folder0);
Genome_choice <- c('',default_choice);
chromosome_choice <- c('',gsub('.*_','',gsub('.fa.nin','',list.files(paste(database_folder,Speciesx,'/',default_ref, sep=''), pattern='*.fa.nin')))) # Need 'IWGSC', default_ref
gff_folder_Species <- paste(gff_folder,Speciesx,'/',sep='');

#Backup_folder<-paste(database_folder,Speciesx,'/',default_ref,'/','Candidate_genes','/',sep=''); ## Need 'IWGSC',default_ref

#Backup_folder<-'/mnt/946c1663-fcbd-4a78-8887-c55f23c5b496/bszhang/Test_Candidate/Candidate_genes/';
Backup_folder<-paste(candidate_dir,Speciesx,'/', sep=''); 


perlArg0_db_sp <- paste(database_folder,Speciesx,'/',sep='')

#######

G_gff_pattern <- paste(default_ref,".*","_gene_Working.gff3",sep="");
file0<-list.files(gff_folder_Species,pattern = G_gff_pattern);
file1<-read.table(paste(gff_folder_Species,file0,sep=''),header=F);

#GeneID_example<-file1[1,9];
GeneID_example<-GeneExample;
########################################


  page(
#    href = "/page10",

href = page_number , # page of species


ui <- function(request){

################ 2/8/23
inputIp <- function(inputId, value=''){
    tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}
################ 2/8/23


###03/07/23
checkjs1 <- 'function checkFileName1(fieldObj) {
    var FileName  = fieldObj.value;
    var FileBase = FileName.split(/[\\\\/]/).pop();
    if (! FileBase.startsWith("Parent1")) {
        fieldObj.value = "";
        alert("File does not start with Parent1! The correct name is Parent1_chr**.fa.gz");
        return false;
    } 

    if (! FileBase.endsWith("fa.gz")) {
        fieldObj.value = "";
        alert("File does not end with fa.gz! Please use bgzip to compress your .fa file");
        return false;
    }

    return true;
}'

checkjs2 <- 'function checkFileName2(fieldObj) {
    var FileName  = fieldObj.value;
    var FileBase = FileName.split(/[\\\\/]/).pop();
    if (! FileBase.startsWith("Parent2")) {
        fieldObj.value = "";
        alert("File does not start with Parent2! The correct name is Parent1_chr**.fa.gz");
        return false;
    } 

    if (! FileBase.endsWith("fa.gz")) {
        fieldObj.value = "";
        alert("File does not end with fa.gz! Please use bgzip to compress your .fa file");
        return false;
    }

    return true;
}'

attrib_replace <- function(x, cond, ...) {
  if (all(names(cond) %in% names(x)) && identical(cond, x[names(cond)])) x <- c(x, list(...))
  if ("attribs" %in% names(x)) x$attribs <- attrib_replace(x$attribs, cond = cond, ...)
  if ("children" %in% names(x)) x$children <- lapply(x$children, function(ch) attrib_replace(ch, cond = cond, ...))
  x
}
###03/07/23

      source( paste(script_folder,'BRIDGEcereal_tag.R',sep=''), local = TRUE);
      tagfunction(page_title,page_subtitle,Genome_choice,chromosome_choice, default_choice,GeneID_example,default_ref)


    }, # For ui function of page_10

# To add server function part for page10

server <- function(input, output, session){

################ 2/8/23
output$testtext <- renderText({
  
  #User_ip<<- gsub( '\\.', '_',input$ipid)
   User_ip<<- gsub( '\\.', '_', unlist(strsplit(input$ipid,', '))[1] ) #03/15/23
   empty_text<-''
  })
################ 2/8/23
      
      source( paste(script_folder,'BRIDGEcereal_Pre_run.R',sep=''), local = TRUE);
      Pre_run(default_choice,gff_folder,gff_folder_Species,User_folder,Backup_folder, html_Speciesx, User_ip) ## jobs_folder replaced by Backup_folder; AllGenomes_GeneName removed; gpattern removed


###### Start submit function! 
observeEvent(input$submit,{

observeEvent(c(input$Upstream , input$Downstream),{

#timer_start <- Sys.time();

########### with progress ...
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    progress$set(message = 'In progress ...',
                 detail = 'This may take a little while...')
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
####################################################
#ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )

#ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip #03/15/23
#ip_address<-unlist(strsplit(ip_address,', '))[1]
#ip_address <- gsub( '\\.', '_', ip_address )

ip_address <- User_ip

User_Gene<-gsub(' ','',input$Gene)                      
User_folder0<- paste(ip_address,'_',User_Gene,sep='')

#if( file.exists( paste(User_folder, User_folder0 , sep='')) ){
#remove_exist_file <- paste('rm -r ',paste(User_folder, User_folder0 , sep=''),sep='')
#system(remove_exist_file)
#}
#Users_folder1<-paste('mkdir ', User_folder , User_folder0 , sep='')
#system(Users_folder1)  ##

if( !file.exists( paste(User_folder, User_folder0 , sep='')) ){
Users_folder1<-paste('mkdir -m 777 ', User_folder , User_folder0 , sep='')
system(Users_folder1)  ##
} else {
remove_exist_file <- paste('rm -r ',paste(User_folder, User_folder0, sep=''),sep='')
system(remove_exist_file)
Users_folder1<-paste('mkdir -m 777 ', User_folder , User_folder0 , sep='')
system(Users_folder1)  ##
}


Users_folder<-paste(User_folder, User_folder0 , sep='')  ## User's ip_gene

#if (input$fasta=='' ) {

##########
if (input$Pickformat=='CDS'){

#default_ref_updated<- input$Pickgenome;
#Backup_folder<-paste(database_folder,Speciesx,'/',default_ref_updated,'/','Candidate_genes','/',sep='');
Genome<-input$Pickgenome
chromosome<-input$Chr
Selected_gene<-gsub(' ','',input$Gene)

Gene<-gsub(' ','',input$Gene)

extract_flag<-1
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
strand_direction <- For_extract_fa(extract_flag,Genome,gff_folder_Species,Selected_gene,Users_folder,target_folder0,chromosome)[[1]]
query_length <- For_extract_fa(extract_flag,Genome,gff_folder_Species,Selected_gene,Users_folder,target_folder0,chromosome)[[2]]

perlArg1_PickGenome <- input$Pickgenome;
perlArg2_PickGene <- gsub(' ','',input$Gene);
perlArg3_PickChr <- input$Chr;

perlArg4_Users_folder <-Users_folder;

if(as.numeric(input$Upstream)>100){
perlArg5_PickUp <- 100*1000;
} else {
perlArg5_PickUp <- as.numeric(input$Upstream)*1000;
}

if(as.numeric(input$Downstream)>100){
perlArg6_PickDown <- 100*1000;
} else {
perlArg6_PickDown <- as.numeric(input$Downstream)*1000;
}

#}
##########
##############
##############

source(paste(script_folder,"BRIDGEcereal_output.R",sep=''), local = TRUE);
BRIDGEcereal_output(User_folder0,perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown, Backup_folder,strand_direction, database_folder,gff_folder,script_folder,User_folder)

########
########
########
########
########

} else if (input$Pickformat=='fasta_seq') {   #### input$fasta not null
###
query0<-input$fasta
tmp <- tempfile(fileext = ".fa")
   if (startsWith(query0, ">")){
     writeLines(query0, tmp)
   } else {
     writeLines(paste0(">query\n",query0), tmp) ##

   }
dna <- readDNAStringSet(tmp)
##########
dna_COPY<-dna
names(dna_COPY)<-input$Chr

Name_update_4 <- paste(gsub(' ','',input$Gene),"_",input$Chr,".fa",sep = "");
writeXStringSet(dna_COPY, file=paste(Users_folder,'/',Name_update_4,sep=''), append=FALSE, compress=FALSE, format="fasta");


   makedb0 <- paste('makeblastdb -in', paste(Users_folder,'/',Name_update_4,sep='') ,'-dbtype nucl',sep=' ')
   makedb1 <- system(makedb0)
          
   bgzip0 <- paste('bgzip -@ 2 ',paste(Users_folder,'/',Name_update_4,sep=''),sep=' ')
   bgzip1 <- system(bgzip0)

   samtools0 <- paste('samtools faidx',paste(Users_folder,'/',Name_update_4,'.gz',sep='') ,sep=' ')
   samtools1 <- system(samtools0)

   dir0<- paste(Users_folder,'/',gsub(' ','',input$Gene),sep='')
   dir.create(dir0)

   move0<-list.files(Users_folder, pattern=paste(gsub(' ','',input$Gene),'_',sep='') );
   for(i in 1:length(move0)){
    
         move1 <- paste(Users_folder,'/',move0[i],sep='')
         move2<- paste('mv',move1,dir0,sep=' ')
         system(move2)

         }

##########
#######
query_COPY<-dna;
query_COPY2<-dna;
perlArg4_Users_folder <-Users_folder;
Gene <- gsub(' ','',input$Gene);  
cds_ids <- gsub(' ','',input$Gene);
query_length<-length(query_COPY[[1]]);
Name_update_1 <- paste(Gene,"_mRNA",sep = "");
Name_update_2 <- paste(Gene,"_CDS",sep = "");
names(query_COPY)<-Name_update_1;
names(query_COPY2)<-Name_update_2;
GeneRef<-c(query_COPY,query_COPY2);
writeXStringSet(GeneRef,paste(Users_folder,'/',Gene,'_ref',sep = ''), append=FALSE, compress=FALSE, format="fasta");
######

#default_ref_updated<- input$Pickgenome;
#Backup_folder<-paste(database_folder,Speciesx,'/',default_ref_updated,'/','Candidate_genes','/',sep='');

perlArg1_PickGenome <- input$Pickgenome;
perlArg2_PickGene <- gsub(' ','',input$Gene);
perlArg3_PickChr <- input$Chr;
perlArg4_Users_folder <-Users_folder;

perlArg5_PickUp <- 0;
perlArg6_PickDown <- 0;
##########################################
##########################################

source(paste(script_folder,"BRIDGEcereal_output.R",sep=''), local = TRUE);
BRIDGEcereal_output(User_folder0,perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown, Backup_folder,strand_direction, database_folder,gff_folder,script_folder,User_folder)

}                       ## input$fasta not null


}) ## observeEvent input$Upstream and input$Downstream !!


}) ## observeEvent submit !!

#################### To large file
####################
observeEvent(input$Largefile,{

observeEvent(c(input$Upstream , input$Downstream),{

#timer_start <- Sys.time();

########### with progress ...
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    progress$set(message = 'In progress ...',
                 detail = 'This may take a little while...')
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.2)  ## ??
    }
####################################################
#ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )
#ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip
#ip_address<-unlist(strsplit(ip_address,', '))[1]
#ip_address <- gsub( '\\.', '_', ip_address )

ip_address <- User_ip

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')

Users_folder<-paste(User_folder, User_folder0 , sep='')  ## User's ip_gene !!
####################################################
#default_ref_updated<- input$Pickgenome;
#Backup_folder<-paste(database_folder,Speciesx,'/',default_ref_updated,'/','Candidate_genes','/',sep='');

Genome<-input$Pickgenome
chromosome<-input$Chr
Selected_gene<-gsub(' ','',input$Gene)

Gene<-gsub(' ','',input$Gene)


extract_flag<-1
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
strand_direction <- For_extract_fa(extract_flag,Genome,gff_folder_Species,Selected_gene,Users_folder,target_folder0,chromosome)[[1]]
query_length <- For_extract_fa(extract_flag,Genome,gff_folder_Species,Selected_gene,Users_folder,target_folder0,chromosome)[[2]]


perlArg1_PickGenome <- input$Pickgenome;
perlArg2_PickGene <- gsub(' ','',input$Gene);
perlArg3_PickChr <- input$Chr;

perlArg4_Users_folder <-Users_folder;

if(as.numeric(input$Upstream)>100){
perlArg5_PickUp <- 100*1000;
} else {
perlArg5_PickUp <- as.numeric(input$Upstream)*1000;
}

if(as.numeric(input$Downstream)>100){
perlArg6_PickDown <- 100*1000;
} else {
perlArg6_PickDown <- as.numeric(input$Downstream)*1000;
}

####################

source(paste(script_folder,"BRIDGEcereal_output.R",sep=''), local = TRUE);
BRIDGEcereal_output(User_folder0,perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown, Backup_folder,strand_direction, database_folder,gff_folder,script_folder,User_folder)

}) ## observeEvent input$Upstream and input$Downstream !!


}) # observeEvent input$Largefile
#################### To large file, The End
####################

output$Save <- downloadHandler(

  filename = function() {
   #file = paste(User_Gene,'.zip',sep='')
    file = paste(gsub(' ','',input$Gene),'.zip',sep='')
  },
  content = function(file) {
  
  #ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )
  
  #ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip
  #ip_address<-unlist(strsplit(ip_address,', '))[1]
  #ip_address <- gsub( '\\.', '_', ip_address )

  ip_address <- User_ip

  User_folder0<- paste(ip_address,'_',gsub(' ','',input$Gene),sep='')
  Users_folder<-paste(User_folder, User_folder0 , sep='')  ## User's ip_gene !!
  

  #03/07/23
  keep_files<-list.files(path=Users_folder, pattern=gsub(' ','',input$Gene) )                    
  #keep_files_<-list.files(path=Users_folder, pattern="b_matrix_groups2.txt" ) 
  #rm_files<-list.files(Users_folder)[ which(!list.files(Users_folder) %in% keep_files ) ]

  rm_files<-list.files(Users_folder)[ which(!list.files(Users_folder) %in% c(keep_files) ) ]

  if(length(rm_files)>=1){
  file.remove(paste(Users_folder,'/',rm_files, sep=''))
  }

  if(length(file.exists( list.files(path=Users_folder,pattern=c(".fa.n*")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c(".fa.n*")), sep=''))
  }

  if(length(file.exists( list.files(path=Users_folder,pattern=c("_Crossover")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_Crossover")), sep=''))
  }

  if(length(file.exists( list.files(path=Users_folder,pattern=c("_anno")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_anno")), sep=''))
  }

  if(length(file.exists( list.files(path=Users_folder,pattern=c("_ref$")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_ref$")), sep=''))
  }
  
  if(length(file.exists( list.files(path=Users_folder,pattern=c("_ref_.*_out$")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_ref_.*_out$")), sep=''))
  }

  if(length(file.exists( list.files(path=Users_folder,pattern=c("gb_out_m8")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("gb_out_m8")), sep=''))
  }

  if(length(file.exists( list.files( path=Users_folder,pattern=c("_left")) ))>=1 ) {
   file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_left")), sep=''))
  }
  #03/07/23

  ## Parent1 or Parent2
  if(file.exists(paste(Users_folder,'/','Parent1',sep=''))){
  remove_Parent1 <- paste('rm -r ',Users_folder,'/','Parent1',sep='')
  system(remove_Parent1)
  }
  if(file.exists(paste(Users_folder,'/','Parent2',sep=''))){
  remove_Parent2 <- paste('rm -r ',Users_folder,'/','Parent2',sep='')
  system(remove_Parent2)
  }

  zip(paste(Users_folder,'.zip',sep=''), Users_folder, flags = "-r9Xj")
  file.copy(paste(Users_folder,'.zip',sep=''),file)
  
  file.remove(paste(Users_folder,'.zip',sep='')) #03/07/23

 },
 
  contentType = "application/zip"

)
######

######

####
observeEvent(input$done,{

#ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )
#ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip
#ip_address<-unlist(strsplit(ip_address,', '))[1]
#ip_address <- gsub( '\\.', '_', ip_address )

ip_address <- User_ip

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')
Users_folder<-paste(User_folder, User_folder0 , sep='')
Gene <- gsub(' ','',input$Gene)

if(Gene!=''){

system_clean<-paste('rm -r ',Users_folder, sep=' ') 
system(system_clean)

refresh()

}else{refresh()}

}) ## observeEvent DONE, To remove all temp files


    } # server function of Page_10


  ) # page for Page_10
} # Page_10 function
##################