### 03/07/23
### Function Species : Call tagfunction and Pre_run functions for loading app's page; To prepare input file (gene***_ref) for extract_syn_fa.pl; Call BRIDGEcereal_output function for app's output
### Function tagfunction : Design of app's ui (page's left part) and output (page's right part) ...
### Function Pre_run  : Serching database based on User's gene ID, check fasta format, upload user's fasta file and others ...
### Function BRIDGEcereal_output : Run extract_syn_fa.pl to get all raw output files for downstream R script processing (All figures and tables ...) ...
### Function Plot_SV  : To plot Fig1, Fig3 and Fig4.
############ Species
options(shiny.maxRequestSize=300*1024^2) ## Max size of uploaded file (300Mb in this case)

Species <- function(Speciesx,default_ref,GeneExample, database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_Speciesx){

page_number <- gsub(' ', '/', paste(' ',Speciesx,sep=''))

########################################
page_title<-paste('This is BRIDGEcereal ',Speciesx,' Page',sep='');
page_subtitle<-paste(Speciesx,' Alignment output',sep='');
target_folder0<-paste(database_folder,Speciesx,'/',sep='');
default_choice <-list.files(target_folder0);
Genome_choice <- c('',default_choice);
chromosome_choice <- c('',gsub('.*_','',gsub('.fa.nin','',list.files(paste(database_folder,Speciesx,'/',default_ref, sep=''), pattern='*.fa.nin')))) # Need 'IWGSC', default_ref
gff_folder_Species <- paste(gff_folder,Speciesx,'/',sep='');

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


   source( paste(script_folder,'BRIDGEcereal_Sub.R',sep=''), local = TRUE);

      tagfunction(page_title,page_subtitle,Genome_choice,chromosome_choice, default_choice,GeneID_example,default_ref)


    }, # For ui function of page_10

# To add server function part for page10

server <- function(input, output, session){


################ 2/8/23
output$testtext <- renderText({ 
  User_ip<<- gsub( '\\.', '_',input$ipid)
  empty_text<-''
  })
################ 2/8/23


source( paste(script_folder,'BRIDGEcereal_Sub.R',sep=''), local = TRUE)
Pre_run(default_choice,gff_folder,gff_folder_Species,User_folder,Backup_folder,html_Speciesx) ## jobs_folder replaced by Backup_folder; AllGenomes_GeneName removed; gpattern removed


###### Start submit function! 
observeEvent(input$submit,{

observeEvent(c(input$Upstream , input$Downstream),{

timer_start <- Sys.time();

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

################ 2/8/23
ip_address <- User_ip
################ 2/8/23

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
query <- "query"

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

#writeXStringSet(merge_fa2, paste(Users_folder,'/','query.fasta',sep=''), append=FALSE, compress=FALSE, format="fasta"); 
 
#system( paste('rm ',paste(Users_folder,'/','query0.fa',sep=''), paste(Users_folder,'/',"positions.txt",sep=''), sep=' ') ) 
system( paste('rm ',paste(Users_folder,'/','query0.fa',sep=''), sep=' ') ) # 2/22/23
#######
query_COPY<-merge_fa2;
query_COPY2<-merge_fa2;
perlArg4_Users_folder <-Users_folder;
Gene <- gsub(' ','',input$Gene);  ## from shiny input
cds_ids <- gsub(' ','',input$Gene); ##  ?? from shiny input, for plotting! 
query_length<-length(query_COPY[[1]]);
Name_update_1 <- paste(Gene,"_mRNA",sep = "");
Name_update_2 <- paste(Gene,"_CDS",sep = "");
names(query_COPY)<-Name_update_1;
names(query_COPY2)<-Name_update_2;
GeneRef<-c(query_COPY,query_COPY2);
writeXStringSet(GeneRef,paste(Users_folder,'/',Gene,'_ref',sep = ''), append=FALSE, compress=FALSE, format="fasta");
######

perlArg1_PickGenome <- input$Pickgenome;
perlArg2_PickGene <- gsub(' ','',input$Gene);
perlArg3_PickChr <- input$Chr;

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
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
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

#writeXStringSet(dna, file=paste(Users_folder,'/',"query.fasta",sep=''), append=FALSE, compress=FALSE, format="fasta");

Name_update_4 <- paste(gsub(' ','',input$Gene),"_",input$Chr,".fa",sep = "");
writeXStringSet(dna_COPY, file=paste(Users_folder,'/',Name_update_4,sep=''), append=FALSE, compress=FALSE, format="fasta");

#file.rename(paste(Users_folder,'/',"query.fasta",sep=''), paste(Users_folder,'/',Name_update_4,sep='') );
#system_replace3<-paste("perl -p -i -e 's/",gsub(' ','',input$Gene),"/",input$Chr,"/g' ", paste(Users_folder,'/',Name_update_4,sep=''),sep='');
#system(system_replace3);

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
Gene <- gsub(' ','',input$Gene);  ## from shiny input
cds_ids <- gsub(' ','',input$Gene); ##  ?? from shiny input, for plotting! 
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
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
BRIDGEcereal_output(User_folder0,perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown, Backup_folder,strand_direction, database_folder,gff_folder,script_folder,User_folder)

}                       ## input$fasta not null


}) ## observeEvent input$Upstream and input$Downstream !!


}) ## observeEvent submit !!

#################### To large file
####################
observeEvent(input$Largefile,{

observeEvent(c(input$Upstream , input$Downstream),{

timer_start <- Sys.time();

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

################ 2/8/23
ip_address <- User_ip
################ 2/8/23

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')

Users_folder<-paste(User_folder, User_folder0 , sep='')  ## User's ip_gene !!
####################################################
#default_ref_updated<- input$Pickgenome;
#Backup_folder<-paste(database_folder,Speciesx,'/',default_ref_updated,'/','Candidate_genes','/',sep='');

Genome<-input$Pickgenome
chromosome<-input$Chr
Selected_gene<-gsub(' ','',input$Gene)
query <- "query"

G_gff_pattern <- paste(Genome,".*","_CDS_Working.gff3",sep="")
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

 merge_fa1<-readDNAStringSet(paste(Users_folder,'/','query0.fa',sep=''))
 merge_fa2<-DNAStringSet(unlist(merge_fa1))
 names(merge_fa2)<-c('query')
#writeXStringSet(merge_fa2, paste(Users_folder,'/','query.fasta',sep=''), append=FALSE, compress=FALSE, format="fasta");
#system( paste('rm ',paste(Users_folder,'/','query0.fa',sep=''), paste(Users_folder,'/',"positions.txt",sep=''), sep=' ') )
 system( paste('rm ',paste(Users_folder,'/','query0.fa',sep=''), sep=' ') )
#######
query_COPY<-merge_fa2;
query_COPY2<-merge_fa2;
perlArg4_Users_folder <-Users_folder;
Gene <- gsub(' ','',input$Gene);  ## from shiny input
cds_ids <- gsub(' ','',input$Gene); ##  ?? from shiny input, for plotting! 
query_length<-length(query_COPY[[1]]);
Name_update_1 <- paste(Gene,"_mRNA",sep = "");
Name_update_2 <- paste(Gene,"_CDS",sep = "");
names(query_COPY)<-Name_update_1;
names(query_COPY2)<-Name_update_2;
GeneRef<-c(query_COPY,query_COPY2);
writeXStringSet(GeneRef,paste(Users_folder,'/',Gene,'_ref',sep = ''), append=FALSE, compress=FALSE, format="fasta");
######

perlArg1_PickGenome <- input$Pickgenome;
perlArg2_PickGene <- gsub(' ','',input$Gene);
perlArg3_PickChr <- input$Chr;

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
source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
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
  
  ip_address <- User_ip

  User_folder0<- paste(ip_address,'_',gsub(' ','',input$Gene),sep='')
  Users_folder<-paste(User_folder, User_folder0 , sep='')  ## User's ip_gene !!
  
  #03/07/23
  keep_files<-list.files(path=Users_folder, pattern=gsub(' ','',input$Gene) )                    
  rm_files<-list.files(Users_folder)[ which(!list.files(Users_folder) %in% keep_files ) ]
  file.remove(paste(Users_folder,'/',rm_files, sep=''))

  file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c(".fa.n*")), sep=''))
  file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_Crossover")), sep=''))
  file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_anno")), sep=''))
  file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_ref$")), sep=''))
  file.remove(paste(Users_folder,'/',list.files(path=Users_folder,pattern=c("_ref_.*_out$")), sep=''))

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



#})
####
observeEvent(input$done,{


################ 2/8/23
ip_address <- User_ip
################ 2/8/23

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')
Users_folder<-paste(User_folder, User_folder0 , sep='')
Gene <- gsub(' ','',input$Gene)

if(Gene!=''){

system_clean<-paste('rm -r ',Users_folder,sep=' ')
system(system_clean)

refresh()

}else{refresh()}

}) ## observeEvent DONE, To remove all temp files


    } # server function of Page_10


  ) # page for Page_10
} # Page_10 function
####################################################################################################################
####################################################################################################################
####################################################################################################################
tagfunction <- function(page_title,page_subtitle,Genome_choice,chromosome_choice, default_choice,GeneID_example,default_ref){
      
      tagList(

          fluidPage(theme = shinytheme("readable")), ## 2/9/23 cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
        # fluidPage(shinythemes::themeSelector()),

     #   h1("This is WheatTest Page!",style="text-align:center"),
         h1(page_title,style="text-align:center"),
        nav_links,

# To add ui part for page6   

useShinyjs(),

column(8,offset = 5, titlePanel(page_subtitle)), # titlePanel

sidebarLayout(

sidebarPanel(

################ 2/8/23
inputIp("ipid"),
textOutput("testtext"),
################ 2/8/23


column(12,wellPanel(div(id='my_textinput1' ,
                   textInput("Gene",paste("Gene name ",'(such as ',GeneID_example,' for ',default_ref,')'," or YourID for fasta sequence",sep='') )))),
                   tags$style(type="text/css", "#my_textinput1 {color: red}","#my_textinput1 {font-size:14px;}"),

uiOutput("html"), # 2/8/23

column(12, offset = 4 , actionButton("Check_ID", label = "(1) Check Gene ID",style = 'background-color:#FFCCCC; padding:20px; font-size:100%')),
bsTooltip("Check_ID", "Check your gene name in our database","right", options = NULL),

column(12,
pickerInput(
  inputId = "Pickgenome", 
  label = "Pick Genome (Please select one!) :", 
  choices = Genome_choice,
  selected = c(''), ## by default

  options = list(
    'actions-box' = TRUE, 
    size = 30,
    'selected-text-format' = "count > 1"
  ), 
  multiple = FALSE,
)
),


column(12,
pickerInput(
  inputId = "Chr", 
  label = "Chromosome (Please select one!)", 
  choices = chromosome_choice,
  selected = c(''), ## by default
  options = list(
    'actions-box' = TRUE, 
    size = 21,
    'selected-text-format' = "count > 1"
  ), 
  multiple = FALSE,
)
),

column(12,
pickerInput(
  inputId = "Pickformat", 
  label = "CDS (Coding sequence); OR your fasta sequence :", 
  choices = c('CDS','fasta_seq'),
  selected = c('CDS'), ## by default
  options = list(
    'actions-box' = TRUE, 
    size = 18,
    'selected-text-format' = "count > 1"
  ), 
  multiple = FALSE,
)
),

column(12,textAreaInput("fasta","Your fasta sequence (Please add first line: >YourID Before pasting your DNA sequence!)",height='100px')),
bsTooltip("fasta", ">YourID as the first line","right", options = NULL),

tags$script(checkjs1), #03/07/23
column(12,attrib_replace( fileInput("upload1", "Upload Parent1 (Format: Parent1_chr**.fa.gz)", multiple = FALSE),
       list(id = "upload1", type = "file"), onchange = "checkFileName1(this);") ),
  
tags$script(checkjs2), #03/07/23
column(12,attrib_replace( fileInput("upload2", "Upload Parent2 (Format: Parent2_chr**.fa.gz)", multiple = FALSE),
       list(id = "upload2", type = "file"), onchange = "checkFileName2(this);") ), 

#column(12,fileInput("upload1", "Upload Parent1 (Format: Parent1_chr**.fa.gz)", multiple = FALSE)),  
#column(12,fileInput("upload2", "Upload Parent2 (Format: Parent2_chr**.fa.gz)", multiple = FALSE)), 

column(12,textInput("Upstream","Upstream (kb), Default length=(gene)*10% kb, Max input should <=100 (kb)",value=0)), #2/9/23
column(12,textInput("Downstream","Downstream (kb), Default length=(gene)*10% kb, Max input should <=100 (kb)",value=0)), #2/9/23

#column(12,textInput("Upstream","Upstream (kb), max input should <=100 (kb)",value=0)),
#column(12,textInput("Downstream","Downstream (kb), max input should <=100 (kb)",value=0)),

column(12,
pickerInput(
  inputId = "id", 
  label = "Genomes (Defalt: all genomes selected) :", 
   choices = default_choice,
   selected = default_choice,
  options = list(
    'actions-box' = TRUE, 
    size = 50,
    'selected-text-format' = "count > 1"
  ), 
  multiple = TRUE,
)
),

column(12,sliderInput("Distancefilter", "Distance filter between mapped clusters (1kb-50kb) :", min = 1000, max = 50000, value =20000)),
column(12,sliderInput("CDSfilter", "Expected CDS size compared to Reference (fold change:0.25-4) :", min = 0.25, max = 4, value =c(0.75,1.25))),

column(12,actionButton("submit", label = "(2) Submit",class = "btn-warning")),
bsTooltip("submit", "Please double check your input (format), and then submit your job","right", options = NULL),


uiOutput("Largefile"),

uiOutput("clustertree"),

uiOutput("Haplotypes"),

uiOutput("bucket"),

uiOutput("submit_trim"),

uiOutput("extract_fa"),

#uiOutput("Download"), #03/07/23

downloadButton('Save',label = "Save .zip file to ...",style = "background-color:#FFFFFF"),

uiOutput("done"),

), # sidebarPanel


mainPanel(
fluidRow(
  
############### IP test
# tags$head(
#    tags$script(src="getIP.js")
#  ),
 verbatimTextOutput('IP'),       
############### IP test
 
     column(12,verbatimTextOutput("visits")), ## number of visits

     column(12,verbatimTextOutput("infogene")), ## number of jobs submitted

     textOutput('coordinates_test'),
     tags$head(tags$style("#coordinates_test{color: blue;
                                 font-size: 22px;
                                 font-style: Arial;
                                 }"
                         )
     ),

     textOutput('fasta_test'),
     tags$head(tags$style("#fasta_test{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
                         )
     ),


    textOutput('upload1_name_test'),
     tags$head(tags$style("#upload1_name_test{color: red;
                                 font-size: 24px;
                                 font-style: italic;
                                 }"
                         )
     ),  #03/07/23

     textOutput('upload1_test'),
     tags$head(tags$style("#upload1_test{color: green;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
                         )
     ),


     textOutput('upload2_name_test'),
     tags$head(tags$style("#upload2_name_test{color: red;
                                 font-size: 24px;
                                 font-style: italic;
                                 }"
                         )
     ),  #03/07/23


     textOutput('upload2_test'),
     tags$head(tags$style("#upload2_test{color: green;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
                         )
     ),

     textOutput('job_timer'),
     tags$head(tags$style("#job_timer{color: orange;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                         )
     ),

     textOutput('Up_down_stream_remainder'),
     tags$head(tags$style("#Up_down_stream_remainder{color: blue;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                         )
     ),

#### progress in middle
tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
      )
    )
  ),
#### progress in middle


     column(6, plotOutput("plot",click = NULL,dblclick = NULL,width = "100%",height = 'auto')),

     column(6, plotOutput("plot2",click = "plot2_click",dblclick = NULL,width = "100%",height = 'auto')),
     
     column(12,verbatimTextOutput("info2")),

     column(12,verbatimTextOutput("info4")),

     column(12,verbatimTextOutput("info3")),

     column(12,verbatimTextOutput("info_TE")),
     
     column(12, offset=0, align="center", textOutput('info') ),                     
     tags$head(tags$style("#info{color: blue;
                                 font-size: 24px;
                                 font-style: Arial;
                                 }"
                         )
     ), # 03/06/23

     column(12, plotOutput("plot3",click = "plot3_click",dblclick = "plot3_dblclick",hover = "plot3_hover",width = "100%",height = 'auto')),

     column(12,verbatimTextOutput("info_Trim0")), #03/07/23
     column(12,offset=0, align="center", textOutput("info_Trim")), #03/07/23
     tags$head(tags$style("#info_Trim{color: blue;
                                 font-size: 24px;
                                 font-style: Arial;
                                 }"
                         )
     ), #03/07/23

     # column(12,verbatimTextOutput("info")),

     #column(12, plotOutput("plot4",click = NULL,dblclick = NULL,width = "100%")),
     column(12, offset=0,align="center", plotOutput("plot4",click = NULL,dblclick = NULL,width = "100%",height = 'auto')), #03/06/23

     column(12, offset = 0,DT::dataTableOutput("table4"),style='padding-top:5px; padding-bottom:5px'), # 2/21/23

     column(12, offset = 0,DT::dataTableOutput("table1"),style='padding-top:5px; padding-bottom:5px'), ## 06/17, cluster information
      
     column(12, offset = 0,DT::dataTableOutput("table2"),style='padding-top:5px; padding-bottom:5px'),

     column(12, offset = 0,DT::dataTableOutput("table3"),style='padding-top:5px; padding-bottom:5px'),


)
) # mainPanel


) # sidebarLayout


      ) # For tagList

    } # function
####################################################################################################################
####################################################################################################################
####################################################################################################################
Pre_run <- function(default_choice,gff_folder,gff_folder_Species,User_folder,Backup_folder,html_Speciesx) {

# hide button Save for now
#observe({ toggle(id="Save", condition=!is.null(input$Download))})
observe({ toggle(id="Save", condition=!is.null(input$done))}) #03/07/23

## To track login times
num_visit0 <- as.numeric(read.table(paste(script_folder, "Visit_Times.txt",sep=''),header=F));
num_visit1 <- paste("There have been",num_visit0,"visitors!",sep=" ");
output$visits <- renderText({ num_visit1 });
num_visitNew<- num_visit0+1;
write.table(num_visitNew,paste(script_folder, "Visit_Times.txt",sep=''),row.names=FALSE,col.names=FALSE,quote = FALSE);
## To track login times

## To count submitted jobs (genes)

num_files0 <- length(list.files(paste(Backup_folder, 'Candidate_genes', sep = '')) ); # number of jobs submitted ! USE backup_folder
num_files1<-paste("There're",num_files0,"genes or jobs have been submitted on this page! You can try yours.",sep=' ');
output$infogene <- renderText({ num_files1 });
##

####
observeEvent(input$Check_ID, {

    if( (gsub(' ','',input$Gene) != "") & (length(grep(' ', gsub(' ','',input$Gene)))==0)  ){
 
 key_table <- paste(gff_folder,Speciesx,'/','species_genekey.txt',sep='');
 Gene_Key <- read.table(key_table,header=F)


for(key in Gene_Key$V1){
 if(length(grep(key, gsub(' ','',input$Gene)))==0){
        Test_GeneName2 <- paste("No coordinate information for this query ID! Please double-check your input.",sep='');
        output$coordinates_test <- renderText({ Test_GeneName2 });
    }}


 for(key in Gene_Key$V1){

 if(length(grep(key, gsub(' ','',input$Gene)))==1){

 matched_g <- Gene_Key[which(Gene_Key$V1==key),2]
         updatePickerInput(session = session, inputId = "Pickgenome", selected = matched_g)

AllGenomes_key_info <- paste(gff_folder,Speciesx,'/','All_gene_Working_infor.gff3',sep='');

################ 2/8/23
ip_address <- User_ip
################ 2/8/23

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')
if( file.exists( paste(User_folder, User_folder0 , sep='')) ){
remove_exist_file <- paste('rm -r ',paste(User_folder, User_folder0 , sep=''),sep='')
system(remove_exist_file)
}
Users_folder1<-paste('mkdir -m 777 ', User_folder , User_folder0 , sep='')
system(Users_folder1)  ## 
Users_folder<-paste(User_folder, User_folder0 , sep='')
matched_gene_file <- paste(Users_folder,'/matched_gene.gff',sep='')
system_grep <- paste('grep -w',gsub(' ','',input$Gene),AllGenomes_key_info,'>',matched_gene_file,sep=' ')
system(system_grep)

if (file.size(matched_gene_file)!=0) {

 matched_gene1<-read.table(matched_gene_file,header=F)
         updatePickerInput(session = session, inputId = "Chr", selected = matched_gene1$V1)

        if(nrow(matched_gene1)==1){


          if( (as.numeric(matched_gene1[3])-as.numeric(matched_gene1[2]) ) > 20*1000 ){
          updateSliderInput(inputId="Distancefilter", value = (as.numeric(matched_gene1[3])-as.numeric(matched_gene1[2]) ) ) ## 03/03/23
           } 

        default_up_down_stream<-round(((as.numeric(matched_gene1[3])-as.numeric(matched_gene1[2]))*0.1)/1000,2) # 2/9/23
        updateTextInput(inputId='Upstream' ,value=as.numeric(default_up_down_stream) ) # 2/9/23
        updateTextInput(inputId='Downstream' ,value=as.numeric(default_up_down_stream) ) # 2/9/23
        
        matched_gene1<-paste(matched_gene1[1],':',prettyNum(c(matched_gene1[2]),big.mark=",",scientific=FALSE),'-',prettyNum(c(matched_gene1[3]),big.mark=",",scientific=FALSE), ',', paste('(',matched_gene1[4],')',' Strand.',sep='') ,sep=' ') # 2/8/23

        
        Test_GeneName2 <- paste(c("The query gene is located at ", matched_gene1), collapse= " ")
        output$coordinates_test <- renderText({ Test_GeneName2 });

       output$html <- renderUI({
      #  tags$a(href=paste('https://www.maizegdb.org/gene_center/gene/',gsub(' ','',input$Gene),sep=''), target='_blank',h4(paste('More information about gene',gsub(' ','',input$Gene),sep=' ') ) )      
      #  tags$a(href=paste(html_Speciesx,gsub(' ','',input$Gene),sep=''), target='_blank',h4(paste('More information about gene',gsub(' ','',input$Gene),sep=' ') ) )

         tags$a(href=paste(html_Speciesx,gsub(' ','',input$Gene),sep=''), target='_blank',paste('Click to view more information about gene',gsub(' ','',input$Gene),sep=' '), style = "font-size:16px; color:blue; font-style:italic;");


       }) # 2/8/23


        }

system_delete<-paste('rm',matched_gene_file,sep=' ')
system(system_delete)

} else if(file.size(matched_gene_file)==0){
#system_delete<-paste('rm',matched_gene_file,sep=' ')
system_delete<-paste('rm -r',Users_folder,sep=' ')
system(system_delete)}

 }
 }
   

   } else if((gsub(' ','',input$Gene) != "") & (length(grep(' ', gsub(' ','',input$Gene)))!=0)){

        Test_GeneName2 <- paste("No coordinate information for this query ID! Please double-check your input.",sep='');
        output$coordinates_test <- renderText({ Test_GeneName2 });
    }




  })
####
## enable or disable some of input selections
observeEvent(input$Pickformat, {

    if (input$Pickformat == "fasta_seq"){
     
      shinyjs::enable(id = "fasta")
      
      shinyjs::disable(id = "Upstream")
      shinyjs::disable(id = "Downstream")


    } else if (input$Pickformat == "CDS" ) {
      
      shinyjs::disable(id = "fasta")
      
    } 

    })
###### To test fasta format
observeEvent(input$fasta, {

    if(input$fasta != ""){
     
     query0<-input$fasta

    if (startsWith(query0, paste(">",gsub(' ','',input$Gene),sep='') )) {
    Test_fasta2 <- paste("Correct fasta input. You can proceed ...",sep='');
    output$fasta_test <- renderText({ Test_fasta2 });

              updatePickerInput(session = session, inputId = "id",
                        choices = c(default_choice,gsub(' ','',input$Gene)),
                        selected = c(default_choice,gsub(' ','',input$Gene)))
     
   } else {
    Test_fasta2 <- paste("Incorrect fasta format! Please double check (>YourID as first line name !!!)",sep='');
    output$fasta_test <- renderText({ Test_fasta2 });
   }

    } 
  
  })
###### To test fasta format

###### upload
observeEvent(input$upload1, {

        #03/07/23
        if( paste("Parent1","_",input$Chr,".fa.gz",sep='') != input$upload1$name ) {
                 
        Test_upload1_name <- paste("Failed upload! Wrong name detected for your file. Please double check your file name ...",sep='')  
        output$upload1_name_test <- renderText({ Test_upload1_name })
        shinyjs::disable(id = "submit")
        return()

       } #03/07/23





                progress <- Progress$new(session, min=1, max=10)
                on.exit(progress$close())
                progress$set(message = 'Processing your compressed file Parent1 ...',
                detail = 'Almost there...')
                for (i in 1:5) {
                progress$set(value = i)
                Sys.sleep(0.3)
                }


          if (is.null(input$upload1)) return()

#############
################ 2/8/23
ip_address <- User_ip
################ 2/8/23

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')

if( file.exists( paste(User_folder, User_folder0 , sep='')) ){

remove_exist_file <- paste('rm -r ',paste(User_folder, User_folder0 , sep=''),sep='')
system(remove_exist_file)

}

Users_folder1<-paste('mkdir -m 777 ', User_folder , User_folder0 , sep='')
system(Users_folder1)  ## 

Users_folder<-paste(User_folder, User_folder0 , sep='')
#############
       
          file.copy(input$upload1$datapath, paste0(Users_folder,'/',input$upload1$name) )
          
          fileName0<- paste(Users_folder,'/',input$upload1$name,sep='')
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
          

          ParentName0<- as.character(strsplit(input$upload1$name, ".fa.gz"))
          ParentName1<- gsub('_.*','',ParentName0)

         dir0<- paste(Users_folder,'/','Parent1',sep='')
         dir.create(dir0)
         

         move0<-list.files(Users_folder, pattern='Parent1_')

         for(i in 1:length(move0)){
        
         move1 <- paste(Users_folder,'/',move0[i],sep='')
         move2<- paste('mv',move1,dir0,sep=' ')
         system(move2)

         }

          updatePickerInput(session = session, inputId = "id",
                        choices = c(default_choice,'Parent1'),
                        selected = c(default_choice,'Parent1'))

    Test_upload1 <- paste("Completion of Parent1 upload. You can proceed ...",sep='')
    output$upload1_test <- renderText({ Test_upload1 })

shinyjs::disable(id = "submit")

output$Largefile <- renderUI({

    actionButton("Largefile", label = "(2) Submit (large file)",style="color: FF66B2; background-color: #FFFF99; border-color: #c34113; border-radius: 10px; border-width: 2px")

  })               

        } ) ## observe
######

observeEvent(input$upload2, {

        #03/07/23
        if( paste("Parent2","_",input$Chr,".fa.gz",sep='') != input$upload2$name ) {
                 
        Test_upload2_name <- paste("Failed upload! Wrong name detected for your file. Please double check your file name ...",sep='')  
        output$upload2_name_test <- renderText({ Test_upload2_name })
        shinyjs::disable(id = "submit")
        return()

       } #03/07/23

                progress <- Progress$new(session, min=1, max=10)
                on.exit(progress$close())
                progress$set(message = 'Processing your compressed file Parent2 ...',
                detail = 'Almost there...')
                for (i in 1:5) {
                progress$set(value = i)
                Sys.sleep(0.3)
                }

          if (is.null(input$upload2)) return()


#############

################ 2/8/23
ip_address <- User_ip
################ 2/8/23

User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')
Users_folder<-paste(User_folder, User_folder0 , sep='')
#############
#############
       
          file.copy(input$upload2$datapath, paste0(Users_folder,'/',input$upload2$name) )
          
          fileName0<- paste(Users_folder,'/',input$upload2$name,sep='')
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
          

          ParentName0<- as.character(strsplit(input$upload2$name, ".fa.gz"))
          ParentName2<- gsub('_.*','',ParentName0)

         dir0<- paste(Users_folder,'/','Parent2',sep='')
         dir.create(dir0)
         

         move0<-list.files(Users_folder, pattern='Parent2_')

         for(i in 1:length(move0)){
        
         move1 <- paste(Users_folder,'/',move0[i],sep='')
         move2<- paste('mv',move1,dir0,sep=' ')
         system(move2)

         }

          updatePickerInput(session = session, inputId = "id",
                        choices = c(default_choice,'Parent1','Parent2'),
                        selected = c(default_choice,'Parent1','Parent2'))


        Test_upload2 <- paste("Completion of Parent2 upload. You can proceed ...",sep='')
        output$upload2_test <- renderText({ Test_upload2 })

    shinyjs::disable(id = "submit")

    output$Largefile <- renderUI({

    actionButton("Largefile", label = "Submit (For user uploaded haplotype)",style="color: FF66B2; background-color: #FFFF99; border-color: #c34113; border-radius: 10px; border-width: 2px")

  })    

        })


}   # function
####################################################################################################################
####################################################################################################################
####################################################################################################################
BRIDGEcereal_output <- function(User_folder0,perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown, Backup_folder,strand_direction,database_folder,gff_folder,script_folder,User_folder) {

system1 <- paste("perl", paste(script_folder,'extract_syn_fa.pl',sep=''),perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown,Backup_folder, 1, 0,sep=' ');
system2 <- paste("perl", paste(script_folder,'extract_syn_fa.pl',sep=''),perlArg0_db_sp,perlArg1_PickGenome ,perlArg2_PickGene,perlArg3_PickChr,perlArg4_Users_folder,perlArg5_PickUp,perlArg6_PickDown,Backup_folder, 0, 2,sep=' ');

Dir <- paste(User_folder, User_folder0,'/',sep = '');

Dir0<-paste(User_folder, User_folder0,'/',paste(Gene,"_ref",sep = ""),sep = '');
system1_User<- paste(system1,Dir0,sep=' ')
system2_User<- paste(system2,Dir0,sep=' ')

system(system1_User);

######################################################################## To search for the outliers!
Backup_folder_Gene<-paste(Backup_folder,'Candidate_genes','/',Gene,'/',sep="");

BlastSyn<-paste(Backup_folder_Gene,Gene,'_Haplotype_syn',sep = "");
Blast_Ori<-paste(Backup_folder_Gene,Gene,'_Blast_Original',sep = "");
BlastSynWorking<-read.table(BlastSyn,header=T); ## _Haplotype_syn
write.table(BlastSynWorking, file = Blast_Ori,sep= "\t",quote = FALSE,row.names = FALSE);


##########
FilterNew0<-BlastSynWorking

Filter_list0 <- list()

Information_list<-list()

indexNew1<-1

for(indexNew in unique(FilterNew0$Genome)){

FilterNew1<- FilterNew0[which(FilterNew0$Genome==indexNew),]

if(nrow(FilterNew1)>=2){

Trees<-length(unique(cutree(hclust(dist(FilterNew1[,6])), h = input$Distancefilter)))

hclusters_similarity<-list()
hclusters_size<-list()

Information_matrix_Members<-matrix(nrow=Trees,ncol=1)

for(Treecut in 1:Trees) {
hclusters <-FilterNew1[which(cutree(hclust(dist(FilterNew1[,6])), k =Trees, h = input$Distancefilter ) ==Treecut),]
hclusters_similarity[Treecut]<-mean(hclusters[,9])
hclusters_size[Treecut]<-sum(hclusters[,8])

Information_matrix_Members[Treecut,1]<-nrow(hclusters)

}

hcluster_matrix<-matrix(nrow=Trees,ncol=2)

for(Treecut in 1:Trees) {
hcluster_matrix[Treecut,1]<-hclusters_similarity[[Treecut]]
hcluster_matrix[Treecut,2]<-hclusters_size[[Treecut]]
}
hcluster_matrix[,2]<-hcluster_matrix[,2]/query_length
Size_filter1<-which(hcluster_matrix[,2]>=input$CDSfilter[1] & hcluster_matrix[,2]<=input$CDSfilter[2])


if(length(Size_filter1)>1){
Similarity_filter1<-which(max(hcluster_matrix[Size_filter1,][,1])==hcluster_matrix[,1])
}else{

Similarity_filter1<-which(max(hcluster_matrix[,1])==hcluster_matrix[,1])

}

Target_cluster1<-intersect(Size_filter1,Similarity_filter1)

if(length(Target_cluster1)==0){    ###  6/21
Ideal_Size <- 1.0
Target_cluster1<- which(abs(hcluster_matrix[,2] - Ideal_Size) == min(abs(hcluster_matrix[,2] - Ideal_Size)))

if(length(Target_cluster1)>1){
Target_cluster1<-which(max(hcluster_matrix[Target_cluster1,][,1])==hcluster_matrix[,1])
}
}

Filter_list0[[indexNew1]]<-FilterNew1[which(cutree(hclust(dist(FilterNew1[,6])), k =Trees, h = input$Distancefilter ) == Target_cluster1),]

Information_matrix<-matrix(nrow=Trees,ncol=9)
Information_matrix[,1]<-rep(indexNew, Trees)
Information_matrix[1,2]<-nrow(FilterNew1)
Information_matrix[1,3]<-input$Distancefilter/1000
Information_matrix[1,4]<-Trees
Information_matrix[,5]<-1:nrow(hcluster_matrix)
Information_matrix[,6]<-Information_matrix_Members
Information_matrix[,7]<-round(hcluster_matrix[,1], digits = 3)
Information_matrix[,8]<-round(hcluster_matrix[,2], digits = 3)
Information_matrix[Target_cluster1,9]<-c("Selected")

colnames(Information_matrix)<-c("Genomes","Positions","HeightCut (kb)","TotalClusters","ClusterIndex","Members","MeanSimilarity",paste("TotalLength","/",input$Pickgenome,sep=''),"CandidateCluster")     #03/07/23

Information_Table<-as.data.frame(Information_matrix)
Information_list[[indexNew1]]<-Information_Table


}else{

Filter_list0[[indexNew1]]<-FilterNew0[which(FilterNew0$Genome==indexNew),]

}


indexNew1<-indexNew1+1

}

Filtered_HaplotypeSyn <-as.data.frame(rbindlist(Filter_list0))

write.table(Filtered_HaplotypeSyn, file= BlastSyn,sep= "\t",quote = FALSE,row.names = FALSE);
##############################
if (input$Pickformat == "fasta_seq"){

BlastSynWorking_strand<-read.table(BlastSyn,header=T); ## _Haplotype_syn
Filtered_HaplotypeSyn_Strand0<-BlastSynWorking_strand[which(BlastSynWorking_strand$Genome==input$Pickgenome),c(6,7)][1, ] ## Only the first one
if( (Filtered_HaplotypeSyn_Strand0$sbj_E-Filtered_HaplotypeSyn_Strand0$sbj_St) >0 ){
    strand_direction<-'+';
} else if((Filtered_HaplotypeSyn_Strand0$sbj_E-Filtered_HaplotypeSyn_Strand0$sbj_St) <0){
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

Backup_folder_Gene<-paste(Backup_folder,'Candidate_genes','/',Gene,'/',sep="");

result_files<-list.files(Backup_folder_Gene, pattern = Gene)
file.copy(file.path(paste(Backup_folder,'Candidate_genes','/',Gene,'/',sep=""), result_files), perlArg4_Users_folder) # OK ????

####################### To check genomes
#BlastSyn<-paste(Gene,'_Haplotype_syn',sep = "");
#BlastSynWorking<-read.table(BlastSyn,header=T);
#Test_Genome0<-unique(BlastSynWorking[,4]);

#g_lab <- intersect(genomes,Test_Genome0);
#genomes_r <- intersect(genomes,Test_Genome0);
####################### To check genomes
############ to remove crossover
Gene <- gsub(' ','',input$Gene)

CrossFilter0 <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);

combine_num <- ncol(combn(unique(CrossFilter0$V1),2))

for(index in 1:combine_num){

name1<- combn(unique(CrossFilter0$V1),2)[,index][1]
name2<- combn(unique(CrossFilter0$V1),2)[,index][2]

Genome1 <- CrossFilter0[which(CrossFilter0$V1 == name1 & CrossFilter0$V2 == name2),]
Genome1_1 <- CrossFilter0[which(CrossFilter0$V1 == name1 & CrossFilter0$V2 == name1),]
Genome2 <- CrossFilter0[which(CrossFilter0$V1 == name2 & CrossFilter0$V2 == name1),]
Genome2_2 <- CrossFilter0[which(CrossFilter0$V1 == name2 & CrossFilter0$V2 == name2),]

Genome_1_2 <- rbind(Genome1,Genome2, Genome1_1,Genome2_2)  ## V1==V2 or V1 !=V2

Genome_1_3 <- rbind(Genome1,Genome2) ##  Only V1 != V2

Genome_1_4 <- rbind(Genome1_1,Genome2_2) ##  Only V1 == V2

Self_candidate1<- Genome1_1[which(Genome1_1$V3 !=100), ][,7:8]
Self_candidate2<- Genome2_2[which(Genome2_2$V3 !=100), ][,7:8]

Genome_1_3[,13]<- c(row.names(Genome_1_3))

Genome_1_3_1 <-Genome_1_3[,c(13,7,8)]

colnames(Genome_1_3_1)<-c("ID","V7","V8")

Self_candidate1[,3]<- c(row.names(Self_candidate1))
Self_candidate2[,3]<- c(row.names(Self_candidate2))

Self_candidate1_1 <- Self_candidate1[,c(3,1,2)]
Self_candidate2_1 <- Self_candidate2[,c(3,1,2)]

colnames(Self_candidate1_1)<-c("ID","V7","V8")
colnames(Self_candidate2_1)<-c("ID","V7","V8")

Setdiff_ID1 <- (Genome_1_3_1  %>% semi_join(Self_candidate1_1,by = c("V7","V8"))) $ID
Setdiff_ID2 <- (Genome_1_3_1  %>% semi_join(Self_candidate2_1,by = c("V7","V8"))) $ID

Setdiff_ID <-c(Setdiff_ID1,Setdiff_ID2)

CrossFilter0 <-CrossFilter0[!(row.names(CrossFilter0) %in% Setdiff_ID), ]

}

write.table(CrossFilter0, file=paste(Dir, Gene, '_Haplotype-Self_out_m8_Crossover', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = FALSE) #?

############ to remove crossover

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

CDS_gDNA_blast <- read.table(paste(Dir, Gene, '_ref_mRNA-Haplotype_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);

gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
gDNAs_blast <- subset(gDNAs_blast, gDNAs_blast[,4]> 0)

N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
Anno <- read.table(paste(Dir, Gene, '_Haplotype_anno', sep = ''), sep = '\t', header = T, stringsAsFactors = F);

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repmask <- read.table(paste(Dir, Gene, '_repMask2', sep = ''), header = F, sep = "\t", stringsAsFactors = F);
repmask <- repmask[abs(repmask[,8] - repmask[,7])> 100,]
}

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

query_gene_up<-c(unique(Query_coordinate_2$V1),range(Query_coordinate_2[,2:3])[2], range(Query_coordinate_2[,2:3])[2]+perlArg5_PickUp)
query_gene_down<-c(unique(Query_coordinate_2$V1),range(Query_coordinate_2[,2:3])[1]-perlArg6_PickDown, range(Query_coordinate_2[,2:3])[1])

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
  Near_Gene_coor <<-cbind((range(Query_coordinate_2[,2:3])[2]+perlArg5_PickUp)-Near_gene1$V5, (range(Query_coordinate_2[,2:3])[2]+perlArg5_PickUp)-Near_gene1$V4)
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



source(paste(script_folder, 'BRIDGEcereal_Sub.R', sep=''), local = TRUE)
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, Anno, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction,Query_coordinate, Near_Gene_coor)
##

timer_end <- Sys.time();
job_timer0 <- paste("Time used for your job: ", round(as.numeric(gsub('Time difference of ','',difftime(timer_end,timer_start))),2) ,' Seconds', '.',sep='')
output$job_timer <- renderText({ job_timer0 })


Test_Up_down_stream_remainder <- paste("We suggest update preferred upstream and downstream inputs in this step ...",sep='')
output$Up_down_stream_remainder <- renderText({ Test_Up_down_stream_remainder })

#output$Download <- renderUI({
#actionButton("Download", label = "Prepare *.zip output",style = "background-color:#FFFFFF") # 2/8/23
#})#03/07/23


 }, height = function() {length(input$id)*13+400} )  ## ## renderPlot from *10 to *13 03/06/23 for plot1


output$done <- renderUI({
    actionButton("done", label = "DONE",style = "background-color:#FF6666")
  })


output$clustertree <- renderUI({
    actionButton("clustertree", label = "(3) TREE",,style = "background-color:#3399FF")
  })


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
output$bucket <- NULL
output$table4 <- NULL # 2/28/23

output$plot2 <- renderPlot({

Gene <- gsub(' ','',input$Gene)  ## from shiny input

gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8_Crossover', sep = ''), sep = '\t', header = F, stringsAsFactors = F);   ## 10/10/22 crossover removed !
gDNAs_blast <- subset(gDNAs_blast, gDNAs_blast[,4]> 0)

genomes <- input$id ## from shiny input

####################### To check genomes
BlastSyn<-paste(Dir, Gene,'_Haplotype-Self_out_m8',sep = "");
BlastSynWorking<-read.table(BlastSyn,header=F);
Test_Genome0<-unique(BlastSynWorking$V2);
Test_Genome1 <- intersect(genomes,Test_Genome0);
if('query' %in% Test_Genome1){
 genomes <- Test_Genome1[which(Test_Genome1 != 'query')]  ##
} else {
 genomes <- Test_Genome1
}
####################### To check genomes
n_g <- length(genomes)  
b_matrix <- diag(n_g)
for (g1 in 1:(n_g - 1)) {
 for (g2 in (g1 + 1): n_g ) {
  gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes[g1] & gDNAs_blast[,2] == genomes[g2]);
  q_hits <- c(gDNAs[,7], gDNAs[,8]); s_hits <- c(gDNAs[,9,], gDNAs[,10])
  b_matrix[g1, g2] <- lm(q_hits ~ s_hits)$coeff[2]

  gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes[g2] & gDNAs_blast[,2] == genomes[g1]);
  q_hits <- c(gDNAs[,7], gDNAs[,8]); s_hits <- c(gDNAs[,9,], gDNAs[,10])
  b_matrix[g2, g1] <- lm(q_hits ~ s_hits)$coeff[2]
 }
}
colnames(b_matrix) <- genomes;
rownames(b_matrix) <- genomes;

write.table(round(b_matrix,2), file = paste(Dir, 'b_matrix_ori.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t")
#h_c <- hclust(dist(b_matrix));
#h_c <- hclust( round(dist(b_matrix), 2 ) ); #2/9/23
h_c <- hclust( dist(round(b_matrix,2) ) ); #2/9/23
#write.table(b_matrix,file=paste(Dir, 'b_matrix_distance.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t") # 2/17/23


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

    write.table(input$plot2_click$y ,file=paste(Dir, 'User_tree_cut.txt', sep = ''),row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")  ############ 03/02/23
    b_matrix0 <- cbind(b_matrix, cluster =as.data.frame(memb)) 

    b_matrix_groups <- b_matrix0[,'memb',drop=FALSE]
    b_matrix_groups1<-b_matrix_groups[order(b_matrix_groups$memb), , drop = FALSE]

    b_matrix_groups3 <- cbind(rownames(b_matrix_groups1), data.frame(b_matrix_groups1, row.names=NULL))

   colnames(b_matrix_groups3)<-c('genomes','memb')

   write.table(b_matrix_groups3, file=paste(Dir, 'b_matrix_variety_Table0.txt', sep = ''),row.names=TRUE,col.names=TRUE,quote = FALSE,sep="\t") # 2/27/23

   Ref_row<-b_matrix_groups3[which(b_matrix_groups3$genomes==default_ref),] # 2/9/23
   NonRef_row<-b_matrix_groups3[which(b_matrix_groups3$genomes!=default_ref),] # 2/9/23
   b_matrix_groups3<-rbind(Ref_row,NonRef_row) #2/9/23



  uniq_genome<-list()
  for(i in unique(b_matrix_groups3$memb)){
  uniq_genome[[i]]<-b_matrix_groups3[which(b_matrix_groups3$memb==i),][1,]
  }
  b_matrix_groups2 <- as.data.frame(rbindlist(uniq_genome))

  memb_count<-b_matrix_groups1 %>% group_by(memb) %>% summarize(count=n())
  b_matrix_groups2<- cbind(b_matrix_groups2,memb_count$count) ## 09/26/22
  colnames(b_matrix_groups2)<-c('genomes_rep','main_clusters','haplotypes_rep')
  write.table(b_matrix_groups2,file=paste(Dir, 'b_matrix_groups2.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t")
  write.table(b_matrix_groups2,file=paste(Dir, 'b_matrix_groups2_ori.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t") #2/28/23
info2_text<- paste0(c('Cut tree based on your selected height ~', round(input$plot2_click$y,1),',', 'with color:',color_option[ceiling(input$plot2_click$y) %% 6 +1],',', 'Click on "Plot selected haplotypes" to view haplotypes ...'), collapse= " ")
output$info2 <- renderText({info2_text})

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
                             searchHighlight = TRUE, filter = "top", columnDefs = list(list( className = 'dt-center', targets = "_all"))  )  ) %>% formatStyle(columns=ncol(b_matrix_groups4), target = c("cell"),backgroundColor = c("gold") )
  }) # DT::renderDataTable
############################ 2/9/23

 }) # input$plot2_click

  
  }, height = function() {length(input$id)*13+400}) ## renderplot2 from *10 to *13 03/06/23

 })   ## input$clustertree


#################### 2/28/23
####################
observeEvent( c(input$list_2,input$list_1) ,{

write.table(input$list_2,file=paste(Dir, 'list2.txt', sep = ''),row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")
write.table(input$list_1,file=paste(Dir, 'list1.txt', sep = ''),row.names=FALSE,col.names=FALSE,quote = FALSE,sep="\t")

#Dir<-"/mnt/946c1663-fcbd-4a78-8887-c55f23c5b496/bszhang/User/69_166_59_242_Zm00001eb091250/"

list2<-read.table(paste(Dir, 'list2.txt', sep = ''),header=F)$V1

list2_<-list2 ## 03/03/23

if(file.size( paste(Dir, 'list1.txt', sep = '') )!=0){

list1<-read.table(paste(Dir, 'list1.txt', sep = ''),header=F)$V1

list2<-c(list2,list1)

b_matrix<-read.table(paste(Dir, 'b_matrix_ori.txt', sep = ''),header=TRUE,sep="\t")
h_c <- hclust( dist(round(b_matrix,2) ) )
User_tree_cut<-read.table(paste(Dir, 'User_tree_cut.txt', sep = ''),header=FALSE)$V1
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
b_matrix0<-round(b_matrix,2)
b_matrix_selected0<-b_matrix0[which(match(rownames(b_matrix0) ,list2)!='NA'),which(match(colnames(b_matrix0) ,list2)!='NA')]
h_c2 <- hclust(dist(b_matrix_selected0) )
#as.dendrogram(h_c2) %>% set("labels_cex", 0.9) %>% highlight_branches %>% plot(main = "Title",ylab = "Height",horiz = FALSE);

list_temp <- list1

variety_0<- read.table(paste(Dir, "b_matrix_variety_Table1.txt", sep = ''),header=T,sep = '\t')  ## b_matrix_groups3, b_matrix_variety_Table1.txt

variety_0<-variety_0[ with(variety_0, order(variety_0$genomes) ),  ]  # 03/03/23

b_matrix_groups2_update<-read.table(paste(Dir, "b_matrix_groups2_ori.txt", sep = ''),header=T,sep = '\t')

for(list1 in list_temp){

bucket_filtered0 <-as.data.frame(cbind(rownames(b_matrix_selected0),b_matrix_selected0[,list1]))
bucket_filtered1 <-bucket_filtered0[which(bucket_filtered0$V1!=list1),]
bucket_filtered2<- bucket_filtered1[order(bucket_filtered1$V2,decreasing = TRUE),][1,1]  ## CML247
cluster_removed<-d7[which(d7$cluster_order==list1),2] ## cluster number
num_list_1<-length(which(d7$cluster==cluster_removed)) ## Number of members in the cluster_removed
mem_list_1<-d7[which(d7$cluster==cluster_removed),1] ## members in the cluster_removed

#variety_0<- read.table(paste(Dir, "b_matrix_variety_Table1.txt", sep = ''),header=T,sep = '\t')  ## b_matrix_groups3, b_matrix_variety_Table1.txt

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


### 03/03/23
 d1_order <-list2_
 d2_order <-b_matrix_groups4
 d3_order_<-as.data.frame(matrix(nrow=length(d1_order),ncol=1))
 index_order<-1

 for(name in d1_order ){
  
  old_order0<- grep(name,d2_order[ ,2])

  d3_order_[index_order, 1] <-  d2_order[old_order0, 2]

  index_order<-index_order+1

 }
b_matrix_groups4[, 2]<- d3_order_[,1]
### 03/03/23

### 03/06/23
b_matrix_groups4[,3]<-data.frame(representation=list2_)
b_matrix_groups4<-b_matrix_groups4[ , c('Variety groups','representation','Varieties')]
### 03/06/23

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
                            formatStyle('representation',backgroundColor = styleEqual(list2_, c('yellow') ) ) %>% 
                            formatStyle(columns =ncol(b_matrix_groups4)-1, fontSize = '150%')

  }) # DT::renderDataTable

} else if(file.size( paste(Dir, 'list1.txt', sep = '') )==0){        # if input$list_1!=NULL

b_matrix_groups2_temp<-read.table(paste(Dir, "b_matrix_groups2_ori.txt", sep = ''),header=T,sep = '\t')

write.table(b_matrix_groups2_temp,file=paste(Dir, 'b_matrix_groups2.txt', sep = ''),row.names=FALSE,col.names=TRUE,quote = FALSE,sep="\t")

b_matrix_groups4 <- read.table(paste(Dir, "b_matrix_variety_Table1.txt", sep = ''),header=T,sep = '\t') 

b_matrix_groups4<-b_matrix_groups4[ with(b_matrix_groups4, order(b_matrix_groups4$genomes) ),  ]  # 03/03/23

colnames(b_matrix_groups4)<-c('genome','variety_group')
b_matrix_groups4<-aggregate(b_matrix_groups4$genome~b_matrix_groups4$variety_group, b_matrix_groups4, paste, collapse=", ")
colnames(b_matrix_groups4)<-c('Variety groups','Varieties')

### 03/03/23
 d1_order <-input$list_2
 d2_order <-b_matrix_groups4
 d3_order_<-as.data.frame(matrix(nrow=length(d1_order),ncol=1))
 index_order<-1

 for(name in d1_order ){
  
  old_order0<- grep(name,d2_order[ ,2])

  d3_order_[index_order, 1] <-  d2_order[old_order0, 2]

  index_order<-index_order+1

 }
b_matrix_groups4[, 2]<- d3_order_[,1]
### 03/03/23

### 03/06/23
b_matrix_groups4[, 3]<-data.frame(representation=input$list_2)
b_matrix_groups4<-b_matrix_groups4[ , c('Variety groups','representation','Varieties')]
### 03/06/23

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
                             formatStyle('representation',backgroundColor = styleEqual(input$list_2, c('yellow')) ) %>% 
                             formatStyle(columns =ncol(b_matrix_groups4)-1, fontSize = '150%')
                             
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

CDS_gDNA_blast <- read.table(paste(Dir, Gene, '_ref_mRNA-Haplotype_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
gDNAs_blast <- subset(gDNAs_blast, gDNAs_blast[,4]> 0);

N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
Anno <- read.table(paste(Dir, Gene, '_Haplotype_anno', sep = ''), sep = '\t', header = T, stringsAsFactors = F)

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repmask <- read.table(paste(Dir, Gene, '_repMask2', sep = ''), header = F, sep = "\t", stringsAsFactors = F);
repmask <- repmask[abs(repmask[,8] - repmask[,7])> 100,]
}


output_flag = 0
##

x_lim <- range(gDNAs_blast[,9:10]) + c(0, 2000)

output$plot3 <- renderPlot({

Genome_order <- input$list_2 ## New haplotypes's order
genomes_r <- Genome_order
genomes <- Genome_order
n_g <- length(genomes)
g_lab <- genomes_r;

haplotypes <- 1

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repeats<-1 ## 
}else {
repeats<-0 ##
}


#Query_coordinate<-read.table(paste(Users_folder,'/',"positions.txt",sep=''),header=F)
#Query_coordinate<-cbind(Query_coordinate$V2-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp, Query_coordinate$V3-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp)

# 03/07/23
if( file.exists(paste(Users_folder,'/',"positions.txt",sep='') ) ){

Query_coordinate<-read.table(paste(Users_folder,'/',"positions.txt",sep=''),header=F)
Query_coordinate<-cbind(Query_coordinate$V2-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp, Query_coordinate$V3-range(Query_coordinate[,2:3])[1]+1+perlArg5_PickUp)

} else {

Query_coordinate <<-as.matrix(c(''),nrow=1,ncol=1)
Near_Gene_coor <<- as.matrix(c(''),nrow=1,ncol=1)

}
####################################################
source(paste(script_folder, 'BRIDGEcereal_Sub.R', sep=''), local = TRUE)
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, Anno, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction,Query_coordinate, Near_Gene_coor) ## plot in shiny

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

output$extract_fa <- renderUI({

        if(!is.na(tail(Combined,1)[,1]) & !is.na(tail(Combined,1)[,2])) {
           
            if(Combined[1,1] < Combined[1,2])   {

              actionButton("extract_fa", label = "Extract trimmed fasta",style = "background-color:#66FF66")
                     
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
} ## repmask exists ..
###

observeEvent(input$plot3_hover,{
if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repmask_hover2_reactive <- reactive({
   repmask_hover1[which(with(repmask_hover1, input$plot3_hover$y>=repmask_hover1[,4] & input$plot3_hover$y<=repmask_hover1[,5])), c(1,2,3) ]
     })
temp_hover<-repmask_hover2_reactive() 
repmask_hover3_reactive <- reactive({
   temp_hover[which(with(temp_hover, input$plot3_hover$x>=temp_hover[,2] & input$plot3_hover$x<=temp_hover[,3])), c(1) ]
 })
output$info_TE <- renderText({paste(c("Involved TE:", repmask_hover3_reactive()),collapse = '  ')})
} ## repmask exists ..
 }) ## plot3_hover
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

CoordinateFilter0<-read.table(paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''),header=T) 
CoordinateFilter0<-round(CoordinateFilter0,0)
CoordinateFilter0<-CoordinateFilter0[which(row.names(CoordinateFilter0)!='average'),] # remove average values
Name0<- input$list_2


CDS_gDNA_blast <- read.table(paste(Dir, Gene, '_ref_mRNA-Haplotype_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
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

write.table(CDS_gDNA_blast_left, file=paste(Dir, Gene, '_ref_mRNA-Haplotype_out_m8_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = FALSE)
CDS_gDNA_blast <- CDS_gDNA_blast_left
##########################################

######################## for gDNAs_blast

gDNAs_blast <- read.table(paste(Dir, Gene, '_Haplotype-Self_out_m8', sep = ''), sep = '\t', header = F, stringsAsFactors = F);
CoordinateFilter1 <- subset(gDNAs_blast, gDNAs_blast[,4]> 0)
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
######################## for gDNAs_blast



N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps', sep = ''), sep = '\t', header = T, stringsAsFactors = F);
#### N_Gap filter based on Selected_lines_Coordinates.bed
CoordinateFilter1<-N_Gap

if(length(intersect(Name0,CoordinateFilter1$Genome)) == 0){

write.table(N_Gap, file=paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), sep="\t", quote = FALSE,row.names = FALSE,col.names = TRUE)

N_Gap <- read.table(paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), sep = '\t', header = T, stringsAsFactors = F);

}

if(length(intersect(Name0,CoordinateFilter1$Genome)) != 0){

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

} # if
####
######### gap

#### repeats
if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
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
##

} # for loop

} # repeats
######### repeat

## End of Trim 
output$info_Trim0 <- renderText({ paste(" ",sep='') }) #03/07/23
output$info_Trim <- renderText({ paste("User trimmed graph for ",gsub(' ','',input$Gene),sep='' ) }) #03/07/23

####
Anno <- read.table(paste(Dir, Gene, '_Haplotype_anno', sep = ''), sep = '\t', header = T, stringsAsFactors = F)

N_Gap <- read.table( paste(Dir, Gene, '_Haplotype_N_Gaps_left', sep = ''), header=T ) 

gDNAs_blast <- gDNAs_blast_left

#output_flag = 0
output_flag = 1 # 1/9/23

b_matrix_groups2 <- read.table(paste(Dir, 'Selected_lines_Coordinates.bed', sep = ''),header=T) 

genomes <- input$list_2
genomes_r <- genomes
n_g <- length(genomes)
g_lab <- genomes_r;
#x_lim <- range(CoordinateFilter0)+c(0,2000)
x_lim <- range(CoordinateFilter0)-range(CoordinateFilter0)[1] + 1 + c(0,2000)

haplotypes<-1

output$plot4 <- renderPlot({

if (!file.size(paste(Dir, Gene, '_repMask2', sep = ''))==0) {
repeats<-1 ##
}else {
repeats<-0 ##
}

###### 2/22/23
###### 2/22/23
Query_coordinate<-as.matrix(c(''),nrow=1,ncol=1)
Near_Gene_coor <- as.matrix(c(''),nrow=1,ncol=1) 
####################################################
source(paste(script_folder, 'BRIDGEcereal_Sub.R', sep=''), local = TRUE)
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, Anno, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction,Query_coordinate, Near_Gene_coor) ## plot in shiny

output_flag = 0 # 1/9/23
Plot_SV(genomes_r, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, Anno, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction,Query_coordinate, Near_Gene_coor) # 1/9/23

}, height = function() {length(input$list_2)*13+400}) #03/06/23 add a height for plot4 

})  ## input$submit_trim  Trim

########################## A table showing clustering results
if(length(Information_list)!=0){

output$table1 <-DT::renderDataTable({
datatable(Information_output,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 1: ', htmltools::em('Clustering information based on Blastn results.')
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

colnames(Filtered_HaplotypeSyn_Plotted)<- c('query gene','query start','query end','genome','chromosome','subject start','subject end','DNA Size','Similarity (blastn)')   #2/8/23 

output$table2 <-DT::renderDataTable({
datatable(Filtered_HaplotypeSyn_Plotted,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 2: ', htmltools::em('Blastn results used for plotting.')
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
BlastSynWorking_2 <- Filtered_HaplotypeSyn_Plotted #1/9/23 

colnames(BlastSynWorking_1)<- c('query gene','query start','query end','genome','chromosome','subject start','subject end','DNA Size','Similarity (blastn)')   #2/8/23 

NotShown0 <- anti_join(BlastSynWorking_1,BlastSynWorking_2) # Not shown in plot, other genomic positions.

if(nrow(NotShown0)!=0){

output$table3 <-DT::renderDataTable({
datatable(NotShown0, caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 3: ', htmltools::em('Blastn results not shown in plot.')
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
####################################################################################################################
####################################################################################################################
####################################################################################################################
Plot_SV <- function(genomes, g_lab, repmask, CDS_gDNA_blast, gDNAs_blast, N_Gap, Anno, output_flag,Gene,Ref_genome,haplotypes,repeats,strand_direction, Query_coordinate, Near_Gene_coor) {
# if (output_flag == 1) {png(file = paste(Dir, Gene, '_NAM.png', sep = ''), width= 19 * .5, height= 12 * .75 , pointsize= 10 , units = "in", res = 600)};
 if (output_flag == 1) {png(file = paste(Dir, Gene, '.png', sep = ''), width= 10, height= 11 , pointsize= 10 , units = "in", res = 600)}; # 03/06/23 width=10 ??
 
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
  for (cds_i in 1:length(cds_ids) ) {
    CDSs <- subset(CDS_gDNA_blast, CDS_gDNA_blast[,1] == paste(cds_ids[cds_i], '_mRNA', sep = '') & CDS_gDNA_blast[,2] == genomes[g] );
    arrow_code <- 1;
    if (CDSs[1,9] > CDSs[1, 10]) {arrow_code <- 2}
    if (nrow(CDSs) > 0) {
 #    arrows(max(CDSs[,9:10]) + 100, length(genomes) - g - 0.175, min(CDSs[,9:10]) - 100, length(genomes) - g - 0.175, code = arrow_code, angle = 15, length = .05);
     rect(CDSs[,9], length(genomes) - g - 0.25, CDSs[,10] , length(genomes) - g - 0.1, col = "burlywood",  border = "NA");
    }
   }

  ### for CDS
  for (cds_i in 1:length(cds_ids) ) {
    CDSs <- subset(CDS_gDNA_blast, CDS_gDNA_blast[,1] == paste(cds_ids[cds_i], '_CDS', sep = '') & CDS_gDNA_blast[,2] == genomes[g] );
    arrow_code <- 1;
    if (CDSs[1,9] > CDSs[1, 10]) {arrow_code <- 2}
    if (nrow(CDSs) > 0) {
 #    arrows(max(CDSs[,9:10]) + 100, length(genomes) - g - 0.175, min(CDSs[,9:10]) - 100, length(genomes) - g - 0.175, code = arrow_code, angle = 15, length = .05);
     rect(CDSs[,9], length(genomes) - g - 0.25, CDSs[,10] , length(genomes) - g - 0.1, col = cds_col[cds_i], border ="NA");
    }
   }

  self <- subset(gDNAs_blast,  gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g ])
  sizes <- round((max(self[,7:8]) - min(self[,7:8]) )/ 1000, 2);

  if(haplotypes == 0){
 # legend(max(self[,7:8]),length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)', sep = '')), bty = "n", adj = c(0, 0), cex = .8 )
  legend(max(self[,7:8]),length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)', sep = '')), bty = "n", adj = c(0, 0), cex = .8 )
}

  if(haplotypes == 1){

 if(file.exists(paste(Dir, 'b_matrix_groups2.txt', sep = ''))){
 b_matrix_groups2 <- read.table(paste(Dir, 'b_matrix_groups2.txt', sep = ''),header=T)
 haplotypes0 <- subset(b_matrix_groups2,  b_matrix_groups2$genomes_rep == genomes[g]) ## 09/26/22
 haplotypes1 <- haplotypes0$haplotypes_rep ## 09/26/22
 legend(max(self[,7:8])+80,length(genomes) - g + 0.3, c(g_lab[g], paste('(', sizes, 'kb)',sep = ''), paste('(', haplotypes1, ' varieties)',sep = '') ), bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.0 ) ## 09/26/22
} #03/06/23 +80 ?? cex = 1.0

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
   gIDs <- subset(Anno, Anno$Genome == genomes[g] & Anno$Type == 'gene');
   gCDS <- subset(Anno, Anno$Genome == genomes[g] & Anno$Type == 'CDS');
   if (nrow(gIDs) > 0) {
   arrows(gIDs$Start, rep(length(genomes) - g + 0.05, nrow(gIDs)), gIDs$End, rep(length(genomes) - g + 0.05, nrow(gIDs)), code = gIDs$Strand, angle = 15, length = .05, lwd = .5 )
    }
   if (nrow(gCDS) > 0) { 
    for (k in 1:nrow(gCDS)) {
     rect(gCDS$Start[k], length(genomes) - g + 0.02, gCDS$End[k], length(genomes) - g - 0.05 - 0.02, col = "darksalmon", lwd = .5)
    }
   };

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

if(genomes[g]== Ref_genome){

rect(Query_coordinate[,1], length(genomes) - g - 0.09, Query_coordinate[,2] , length(genomes) - g +0.05, border ='blue')

if(Near_Gene_coor[1,1]!=''){
rect(Near_Gene_coor[,1], length(genomes) - g - 0.09, Near_Gene_coor[,2] , length(genomes) - g +0.05, border ='red', lwd=1.5 ) 
}

}

}
####

 }

 if (output_flag == 1) { dev.off()}
}
####################################################################################################################
####################################################################################################################
####################################################################################################################

