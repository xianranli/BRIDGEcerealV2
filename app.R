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


### 03/28/23
########################################################
administrator_path <- '/home/xianran_li/bridgecereal/'; # scinet
#administrator_path <- '/mnt/946c1663-fcbd-4a78-8887-c55f23c5b496/bszhang/'; # R studio and our server

database_folder <- paste(administrator_path,"database",'/',sep='');

gff_folder <- paste(administrator_path,"gff",'/',sep='');

#script_folder <- paste(administrator_path,"ShinyApps/BRIDGEcereal_Scinet",'/',sep=''); # our server
script_folder <- paste(administrator_path,"script",'/',sep=''); # R studio and scinet

User_folder <-paste(administrator_path,"User",'/',sep='');

candidate_dir<-paste(administrator_path,'candidate_dir','/',sep=''); # scinet and our server
#candidate_dir<-paste(administrator_path,'Test_Candidate','/',sep=''); # R studio

Stream_folder <- "https://bridgecereal.scinet.usda.gov/" # 03/02/23 scinet
#Stream_folder <- "http://10.105.85.25/BRIDGEcereal_Scinet/" # R studio and our server

#web_root<-"/BRIDGEcereal_Scinet/" # our server
web_root<-"/" # R studio and scinet


html_wheat<-'https://plants.ensembl.org/Triticum_aestivum/Search/Results?species=Triticum_aestivum;idx=;q=' #2/8/23
html_maize<-'https://www.maizegdb.org/gene_center/gene/' #2/8/23
html_sorghum<-'https://phytozome-next.jgi.doe.gov/report/gene/Sbicolor_v3_1_1/' #2/8/23
html_rice<-'https://ricerc.sicau.edu.cn/RiceRC/Search/searchBefore?db=all&input=' #2/8/23
html_barley<-'https://wheat.pw.usda.gov/cgi-bin/GG3/report.cgi?class=probe;name=' #2/8/23

source(paste(script_folder,"BRIDGEcereal_Instruction.R",sep=''), local = TRUE);
source(paste(script_folder,"BRIDGEcereal_Species.R",sep=''), local = TRUE); 

########################################################


############################################################ Creating a navlink 
nav_links <- tags$ul(

flowLayout( 

  tags$li(
    tags$a(href = paste(web_root,sep=''), "Main", img(width="200", height="64", src=paste(Stream_folder, "BRIDGEcereal_logo.png", sep='') )),
  
  ),

  tags$li(
    tags$a(href = paste(web_root,'Instruction', sep=''), "Instruction", img(width="140", height="64", src=paste(Stream_folder,"instruction.jpeg",sep=''))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Wheat', sep=''), "Wheat", img(width="180", height="60", src=paste(Stream_folder,"Wheat.jpg",sep=''))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Maize', sep=''), "Maize", img(width="180", height="60", src=paste(Stream_folder,"Maize.jpeg",sep='' ))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Sorghum', sep=''), "Sorghum", img(width="180", height="72", src=paste(Stream_folder,"Sorghum.png",sep=''))),
  ),

   tags$li(
   tags$a(href = paste(web_root,'Rice', sep=''), "Rice", img(width="180", height="60", src=paste(Stream_folder,"Rice.png",sep=''))),

  ),

   tags$li(
   tags$a(href = paste(web_root,'Barley', sep=''), "Barley", img(width="180", height="60", src=paste(Stream_folder,"Barley.jpeg",sep=''))),
  ),

   tags$style(
   
   "li a {font-size:38px; 
          font-weight:bold;
          list-style-type: square;

    }",
    
    ), 

    align = "center",

        ), ## 03/13/23 just a ","
    

    ## 03/13/23
    tags$head( tags$style(" ul {font-size:0;}") ),

                   )


BRIDGEcereal_main <- function(){

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



column(12, offset=3, align="center", h3("

  Large insertion and deletion (indel) polymorphisms, potentially altering gene structure or expression, are class of structural variants that need to be catalogued from pan-genomes. 
  However, the nature of indels, unknown size and uneven distribution in different genome assemblies, complicates the identification process.
  "),

h3("

  This process remains a challenge and often requires painstaking probing and decision-making from users.

  "),

h3("

Here, we introduce BRIDGE (Blastn Recovered Insertions and Deletions near Gene Explorer) for surveying potential indels for genes of interest with 5 publicly accessible cereal pan-genomes.
  
  ")

   ),

column(12, offset=3,align="center", h3("BRIDGEcereal currently holds 120 genomes:" ,style = "font-size:32px; color:red;")),
column(12, offset=3,align="center", h3("11 Wheat genomes.",style = "font-size:32px; color:red;")),
column(12, offset=3,align="center", h3("38 Maize genomes.",style = "font-size:32px; color:red;")),
column(12, offset=3,align="center", h3("18 Sorghum genomes.",style = "font-size:32px; color:red;")),
column(12, offset=3,align="center", h3("33 Rice genomes.",style = "font-size:32px; color:red;")),
column(12, offset=3,align="center", h3("20 Barley genomes.",style = "font-size:32px; color:red;")),

column(12, offset=3,align="center", h3("")),


column(12, offset=3,align="center", tags$a(href="https://www.biorxiv.org/content/10.1101/2023.02.11.527743v1", target='_blank', h3("Reference: 
Zhang B, Huang H, Tibbs-Cortes LE, Vanous A, Zhang Z, Sanguinet K, Garland-Campbell KA, Yu J, Li X. 
Streamline unsupervised machine learning to survey and graph indel-based haplotypes from pan-genomes.
 bioRxiv. 2023:2023-02. doi: 10.1101/2023.02.11.527743" ,
 style = "font-size:32px; color:blue; font-style:italic;") ) ),

#column(12, offset=3,align="center", h3("")),
#column(12, offset=3,align="center", h3("")),

#column(12, offset=3,align="center", h3("Acknowledgements: We thank the USDA-ARS SCINet for computing resource and the collaboration of the USDA-ARS-Partnerships for Data Innovations (PDI,
# https://pdi.scinet.usda.gov/), which provided data stewardship solutions to enable secure data management, storage and sharing.")),

column(12, offset=3,align="center", h3("Contact: xianran.li@usda.gov") ),

#column(9,offset=3, align="center", tags$img(width="240", height="80", src=paste(Stream_folder,"USDA_PDI_Logo.jpg",sep=''))),
column(6,offset=6, align="center", tags$img(width="510", height="60", src=paste(Stream_folder,"USDA_PDI_Logo.jpg",sep=''))),



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

  BRIDGEcereal_main(),

  BRIDGEcereal_Instruction(Stream_folder),

  BRIDGEcereal_Species("Wheat","IWGSC","TraesCS4A02G058900",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_wheat), # 'IWGSC' ... defined as default_ref
  BRIDGEcereal_Species("Maize","B73","Zm00001eb000140",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_maize),   # 'B73' ... defined as default_ref
  BRIDGEcereal_Species("Sorghum","BTx623","Sobic.001G001066",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_sorghum), # 'BTx623' ... defined as default_ref
  BRIDGEcereal_Species("Rice","Nipponbare","LOC_Os01g01120",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_rice), # 'Nipponbare' ... defined as default_ref
  BRIDGEcereal_Species("Barley","Morex","HORVU.MOREX.r3.1HG0000020",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_barley)     # 'Morex' ... defined as default_ref

#  for(sp in All_species){Species(sp,database_folder,gff_folder,script_folder,User_folder)}

# To add many other pages

)



