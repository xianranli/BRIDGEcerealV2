library(shiny)
library(Biostrings)
library(shinyjs)
library(brochure)
library(shinyWidgets)
library(DT)
library(data.table)
library(dplyr)
library(seqinr)
library(dendextend)
library(shinyBS)
library(sortable)
library(rjson)
library(shinythemes)

### 03/07/23
########################################################
administrator_path <- '/home/xianran_li/bridgecereal/'; # scinet
#administrator_path <- '/mnt/946c1663-fcbd-4a78-8887-c55f23c5b496/bszhang/'; # Temp

database_folder <- paste(administrator_path,'database','/',sep='');

gff_folder <- paste(administrator_path,"gff",'/',sep='');

script_folder <- paste(administrator_path,"script",'/',sep=''); # scinet
#script_folder <- paste(administrator_path,"ShinyApps/BRIDGEcereal_Scinet",'/',sep=''); # Temp

User_folder <-paste(administrator_path,"User",'/',sep='');

candidate_dir<-paste(administrator_path,'candidate_dir','/',sep='');

Stream_folder <- "https://bridgecereal.scinet.usda.gov/" # 03/02/23 scinet
#Stream_folder <- "http://10.105.85.25/BRIDGEcereal_Scinet/" # 03/02/23 Temp

web_root<-"/" # scinet
#web_root<-"/BRIDGEcereal_Scinet/" # Temp

html_wheat<-'https://plants.ensembl.org/Triticum_aestivum/Search/Results?species=Triticum_aestivum;idx=;q=' #2/8/23
html_maize<-'https://www.maizegdb.org/gene_center/gene/' #2/8/23
html_sorghum<-'https://phytozome-next.jgi.doe.gov/report/gene/Sbicolor_v3_1_1/' #2/8/23
html_rice<-'https://ricerc.sicau.edu.cn/RiceRC/Search/searchBefore?db=all&input=' #2/8/23
html_barley<-'https://wheat.pw.usda.gov/cgi-bin/GG3/report.cgi?class=probe;name=' #2/8/23

source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
source(paste(script_folder,"Instruction.R",sep=''), local = TRUE); 
############################################################ Creating a navlink 
nav_links <- tags$ul(

flowLayout(

  tags$li(
    tags$a(href = paste(web_root,sep=''), "Main", img(width="100", height="32", src=paste(Stream_folder, "BRIDGEcereal_logo.png", sep='') )),
  ),

  tags$li(
    tags$a(href = paste(web_root,'Instruction', sep=''), "Instruction", img(width="70", height="30", src=paste(Stream_folder,"instruction.jpeg",sep=''))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Wheat', sep=''), "Wheat", img(width="120", height="40", src=paste(Stream_folder,"Wheat.jpg",sep=''))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Maize', sep=''), "Maize", img(width="120", height="40", src=paste(Stream_folder,"Maize.jpeg",sep='' ))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Sorghum', sep=''), "Sorghum", img(width="100", height="40", src=paste(Stream_folder,"Sorghum.png",sep=''))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Rice', sep=''), "Rice", img(width="120", height="40", src=paste(Stream_folder,"Rice.png",sep=''))),

  ),

   tags$li(
   tags$a(href = paste(web_root,'Barley', sep=''), "Barley", img(width="120", height="40", src=paste(Stream_folder,"Barley.jpeg",sep=''))),
  ),

   tags$style(
   "li a {font-size: 20px;font-weight: bold;}",
    )

        )

                   )

page_0 <- function(){

  page(
    href = "/",

    ui <-  function(request){

      tagList(
        
         fluidPage(theme = shinytheme("readable")),

         h1("Wellcome to BRIDGEcereal",style="text-align:center"),

         nav_links,

   useShinyjs(),

   #sidebarLayout(

   #sidebarPanel(

   #), # sidebarPanel

mainPanel(

fluidRow(

column(6, offset=2,tags$img(width="504", height="162", src=paste(Stream_folder,"BRIDGEcereal_logo.png",sep=''))),

#column(3, offset=0,tags$p("First paragraph") ),

column(4, offset=0, h5("

  Pan-genomes with high quality de novo assemblies are shifting the paradigm of biology research in genome evolution, speciation, and function annotation. Arrays of new bioinformatic tools, ranging from data storage, annotation, to polymorphism identification and visualization, have been developed to capitalize pan-genome resources. Large insertion and deletion (indel) polymorphisms, potentially altering gene structure or expression, are class of structural variants that need to be catalogued from pan-genomes. However, the nature of indels, unknown size and uneven distribution in different genome assemblies, complicates the identification process. This process remains a challenge and often requires painstaking probing and decision-making from users.

  "),

h5("

Here, we introduce BRIDGE (Blastn Recovered Insertions and Deletions near Gene Explorer) for surveying potential indels for genes of interest with 5 publicly accessible cereal pan-genomes.
  
  ")

   ),

column(12, offset=3,align="center", h5("BRIDGEcereal currently holds 120 genomes:" ,style = "font-size:18px; color:red;")),
column(12, offset=3,align="center", h5("11 Wheat genomes.",style = "font-size:18px; color:red;")),
column(12, offset=3,align="center", h5("38 Maize genomes.",style = "font-size:18px; color:red;")),
column(12, offset=3,align="center", h5("18 Sorghum genomes.",style = "font-size:18px; color:red;")),
column(12, offset=3,align="center", h5("33 Rice genomes.",style = "font-size:18px; color:red;")),
column(12, offset=3,align="center", h5("20 Barley genomes.",style = "font-size:18px; color:red;")),

column(12, offset=3,align="center", h5("")),


column(12, offset=3,align="center", tags$a(href="https://www.biorxiv.org/content/10.1101/2023.02.11.527743v1", target='_blank', h3("Reference: 
  Zhang B, Huang H, Tibbs-Cortes L, Zhang Z, Sanguinet K, Vanous A, Garland-Campbell K, Yu J, Li X.
    Streamline unsupervised machine learning to survey and graph indel-based haplotypes from pan-genomes" ,style = "font-size:18px; color:blue; font-style:italic;") ) ),

column(12, offset=3,align="center", h5("")),
column(12, offset=3,align="center", h5("")),

column(12, offset=3,align="center", h5("Acknowledgements: We thank the USDA-ARS SCINet for computing resource and the collaboration of the USDA-ARS-Partnerships for Data Innovations (PDI,
 https://pdi.scinet.usda.gov/), which provided data stewardship solutions to enable secure data management, storage and sharing. Contact: xianran.li@usda.gov")),
column(5,offset=6, align="center", tags$img(width="240", height="80", src=paste(Stream_folder,"USDA_PDI_Logo.jpg",sep=''))),


) # fluidRow

) # mainPanel

#) # sidebarLayout

      ) # For tagList
    }, # For ui function of page_0
    

# To add server function part for page0

server <- function(input, output, session){


    } # server function of Page_0


  ) # page for Page_0

} # Page_0 function

############ To combine pages together
 brochureApp(
   page_0(),
   Instruction(),
   Species("Wheat","IWGSC","TraesCS4A02G058900",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_wheat), # 'IWGSC' ... defined as default_ref
   Species("Maize","B73","Zm00001eb000140",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_maize),   # 'B73' ... defined as default_ref
   Species("Sorghum","BTx623","Sobic.001G001066",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_sorghum), # 'BTx623' ... defined as default_ref
   Species("Rice","Nipponbare","LOC_Os01g01120",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_rice), # 'Nipponbare' ... defined as default_ref
   Species("Barley","Morex","HORVU.MOREX.r3.1HG0000020",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_barley)     # 'Morex' ... defined as default_ref

)