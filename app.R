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

library(mailtoR) # email to ...

### 03/31/23
########################################################
administrator_path <- '/home/xianran_li/bridgecereal/'; # scinet
#administrator_path <- '/mnt/compbiolab/bszhang/'; # R studio and our server

database_folder <- paste(administrator_path,"database",'/',sep='');

gff_folder <- paste(administrator_path,"gff",'/',sep='');

#script_folder <- paste(administrator_path,"ShinyApps/BRIDGEcereal_Scinet",'/',sep=''); # our server
script_folder <- paste(administrator_path,"script",'/',sep=''); # R studio and scinet

User_folder <-paste(administrator_path,"User",'/',sep='');

candidate_dir<-paste(administrator_path,'candidate_dir','/',sep=''); # scinet and our server
#candidate_dir<-paste(administrator_path,'Test_Candidate','/',sep=''); # R studio

Stream_folder <- "https://bridgecereal.scinet.usda.gov/" # 03/02/23 scinet
#Stream_folder <- "http://10.105.85.25/BRIDGEcereal_Scinet/" # R studio and our server

#   web_root<-"/BRIDGEcereal_Scinet/" # our server
  web_root<-"/" # R studio and scinet

QA_folder <-paste(administrator_path,"QA",'/',sep=''); #6/14/23 need a new folder mkdir QA

if( !file.exists( QA_folder ) ){
 dir.create(QA_folder)
}

html_wheat<-'https://plants.ensembl.org/Triticum_aestivum/Search/Results?species=Triticum_aestivum;idx=;q=' #2/8/23
html_maize<-'https://www.maizegdb.org/gene_center/gene/' #2/8/23
html_sorghum<-'https://phytozome-next.jgi.doe.gov/report/gene/Sbicolor_v3_1_1/' #2/8/23
html_rice<-'https://ricerc.sicau.edu.cn/RiceRC/Search/searchBefore?db=all&input=' #2/8/23
html_barley<-'https://wheat.pw.usda.gov/cgi-bin/GG3/report.cgi?class=probe;name=' #2/8/23

html_soybean<-'https://phytozome-next.jgi.doe.gov/report/gene/Gmax_Wm82_a4_v1/'

html_PearlMillet<-'https://phytozome-next.jgi.doe.gov/report/gene/Gmax_Wm82_a4_v1/' # ??

#html_tomato<-'https://solgenomics.net/search/locus/'

source(paste(script_folder,"BRIDGEcereal_Instruction.R",sep=''), local = TRUE);
source(paste(script_folder,"BRIDGEcereal_Species.R",sep=''), local = TRUE); 

source(paste(script_folder,"BRIDGEcereal_Reference.R",sep=''), local = TRUE);

source(paste(script_folder,"BRIDGEcereal_CLIPS.R",sep=''), local = TRUE);


#source(paste(script_folder,"BRIDGEcereal_QA.R",sep=''), local = TRUE);

source(paste(script_folder,"BRIDGEcereal_QA_View.R",sep=''), local = TRUE); #6/14/23
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

   tags$li(
   tags$a(href = paste(web_root,'Reference', sep=''), "Reference", img(width="100", height="60", src=paste(Stream_folder,"book.png",sep=''))), # 4000*2250
  ),

#   tags$li(
#   tags$a(href = paste(web_root,'Soybean', sep=''), "Soybean", img(width="120", height="70", src=paste(Stream_folder,"Soybean.png",sep=''))),
#  ),

#   tags$li(
#   tags$a( href = paste(web_root,'Soybean', sep='') ),
#  ),


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


        h2("BRIDGEcereal: survey and graph indels variation in pan-genomes",style="text-align:center"),

        nav_links,


useShinyjs(),


#sidebarLayout(

#sidebarPanel(


#), # sidebarPanel


mainPanel(

fluidRow(

#column(12, offset=3,align="center", h3("BRIDGEcereal: An interactive webapp to cluster and graph indels in pan-genomes")),

#includeMarkdown( paste(script_folder,"ReadMe.MD",sep='')),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="left", h3("Rationale:",style = "font-size: 24px; font-style: normal; font-weight: bold;")),

#column(12, offset=3,align="left", h3("Large indel polymorphisms are important for crop improvement, but identifying large indels is challenging.",
#  style = "font-size: 24px; font-style: normal; font-weight: lighter;") ),
#column(12, offset=3,align="left", h3("A pan-genome is essential to comprehensively catalog large indels.",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
#column(12, offset=3,align="left", h3("Utilizing a pan-genome requires specialized bioinformatics skills.",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
#column(12, offset=3,align="left", h3("BRIDGEcereal provides a user-friendly interface to efficiently identify large indels from pan-genomes of major cereal crops, accelerating gene discovery for important traits.",
#  style = "font-size: 24px; font-style: normal; font-weight: lighter;")),

column(12, offset=3,align="left", 
tags$div(
icon("star", lib = "glyphicon"),
"Large indel polymorphisms are important for crop improvement, but identifying large indels is challenging.",
 ),style = "font-size: 26px; font-style: normal; font-weight: lighter;" ),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="left", 
tags$div(
icon("star", lib = "glyphicon"),
"A pan-genome is essential to comprehensively catalog large indels.",
 ),style = "font-size: 26px; font-style: normal; font-weight: lighter;" ),
column(12, offset=3,align="center", h3("")),

column(12, offset=3,align="left", 
tags$div(
icon("star", lib = "glyphicon"),
"Utilizing a pan-genome requires specialized bioinformatics skills.",
 ),style = "font-size: 26px; font-style: normal; font-weight: lighter;" ),
column(12, offset=3,align="center", h3("")),

column(12, offset=3,align="left", 
tags$div(
icon("star", lib = "glyphicon"),
"BRIDGEcereal provides a user-friendly interface to efficiently identify large indels from pan-genomes of major cereal crops, accelerating gene discovery for important traits.",
 ),style = "font-size: 26px; font-style: normal; font-weight: lighter;" ),


column(12, offset=3,align="left", h3("Approach:",style = "font-size: 24px; font-style: normal; font-weight: bold;")),

#column(5, offset=3,align="left", h3("BRIDGEcereal webapp is built based on two novel unsupervised machine learning algorithms:",style = "font-size: 24px; font-style: normal; font-weight: lighter;") ),

column(12, offset=3,align="left", 
tags$div(
"BRIDGEcereal webapp is built based on two novel unsupervised machine learning algorithms:",
tags$a(href="https://github.com/xianranli/CHOICE_CLIPS", target='_blank', "CHOICE and CLIPS.") ),style = "font-size: 24px; font-style: normal; font-weight: lighter;" ),

#column(12, offset=3,align="left", tags$a(href="https://github.com/xianranli/CHOICE_CLIPS", target='_blank', h4("Click here to further explore demo code for CHOICE and CLIPS ...", style = "font-size: 24px; font-style: normal; font-weight: lighter;") ) ),
column(12, offset=3,align="left", tags$a(href=paste(web_root,'CLIPS', sep=''), target='_blank', h4("Click here to interact with CLIPS algorithm ...", style = "font-size: 24px; font-style: normal; font-weight: lighter;") ) ),


#column(12, offset=3,align="left", h4("Solution: CLIPS, Clustering via Large-Indel Permuted Slopes, groups these segments sharing the same set of indels into haplotypes. 
#  For a pair of segments, the slope estimate will be one when large indels are absent, but will devirate from one when large indels are present.",style = "font-size: 21px; font-style: normal; font-weight: lighter;")),
#column(12, offset=3,align="left", h3("This webapp, based on CHOICE and CLIPS, is developed to tackle these two challenges.",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),

#column(12, offset=3,align="left", tags$a(href="https://github.com/xianranli/CHOICE_CLIPS", target='_blank', h4("Click here to further explore demo code for CHOICE and CLIPS ...", style = "font-size: 24px; font-style: normal; font-weight: lighter;") ) ),

column(12, offset=3,align="left", h3("Applications:",style = "font-size: 24px; font-style: normal; font-weight: bold;")),
column(12, offset=3,align="left", h3("1, Narrow down the candidate gene list in the QTL/GWAS interval.",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
column(12, offset=3,align="left", h3("2, Explore the full spectrum of natural variations of the target gene.",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
#column(12, offset=3,align="left", h3("3, To quickly survey how many clusters for (large) indels variation in a pan-genome.",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
#column(12, offset=3,align="left", h3("4, ",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),

#column(12, offset=3,align="left", h3("Key features of BRIDGEcereal:",style = "font-size: 24px; font-style: normal; font-weight: bold;")),
#column(12, offset=3,align="left", h3("Easy to use with just a few clicks; ~30-40 seconds to get result back; Plot preferred haplotypes in an interactive way.", style = "font-size: 24px; font-style: normal; font-weight: lighter;" )),
#column(12, offset=3,align="left", h3("CHOICE and CLIPS are two unsupervised learning algorithms developed to implement BRIDGEcereal webapp, 
#  which is constructed to survey and graph large indel based haplotype graph from publicly accessible pan-genomes for a gene of interest.", style = "font-size: 24px; font-style: normal; font-weight: lighter;" )),

#column(12, offset=3,align="left", h3("Algorithm illustration:", style = "font-size: 24px; font-style: normal; font-weight: bold;")),
#column(12,offset=3, align="center", tags$img(width="1068.8", height="484", src=paste(Stream_folder,"CHOICE_CLIPS.png",sep=''))), # 0.4 (2672*1210)
#column(12, offset=3,align="center", h3("")),
#column(12,offset=3, align="center", tags$img(width="1296", height="283", src=paste(Stream_folder,"CHOICE.png",sep=''))), # 0.3 (4323*945)
#column(12,offset=3, align="center", tags$img(width="1177", height="283", src=paste(Stream_folder,"CLIPS.png",sep=''))), # 0.3 (3925*946)

column(12, offset=3,align="left", h3("Contact:",style = "font-size: 24px; font-style: normal; font-weight: bold;")),  #6/14/23

column(12, offset=3,align="left", textAreaInput("Feedback", "Any troubles in using BRIDGEcereal? Please enter your gene ID, describe your question briefly, and your email.", 
  "", width = "800px", height ="150px" )), #6/14/23

column(12, offset=3,align="left", actionButton("Submit_Q", label = "Submit Your Questions",class = "btn-warning")), #6/14/23

#column(12, offset=3,align="left", tags$a(href=paste(web_root,'QA', sep=''), target='_blank', 
#    h4("Click here to submit any questions you have in using BRIDGEcereal", style = "font-size: 24px; font-style: normal; font-weight: lighter;") ) ),

column(12, offset=3,align="center", h3("")), #6/14/23


column(12, offset=3,align="center", h3("")),
#column(12, offset=3,align="center", h3("Contact: xianran.li@usda.gov OR xianran.li@wsu.edu",style = "font-size: 24px; font-style: normal; font-weight: lighter;") ),
column(12, offset=3,align="center", 
h3(
     mailtoR(email = c("xianran.li@usda.gov"),
          text = "Emails: xianran.li@usda.gov",       #6/14/23
          subject = "Questions about BRIDGEcereal"),
style = "font-size: 24px; font-style: normal; font-weight: lighter;",
#use_mailtoR(),

     mailtoR(email = c("xianran.li@wsu.edu"),
          text = "xianran.li@wsu.edu",
          subject = "Questions about BRIDGEcereal"),
style = "font-size: 24px; font-style: normal; font-weight: lighter;",
use_mailtoR(),

) ),

#column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", tags$a(href='http://compbiolab.org/', target='_blank', h4("Visit Li lab", 
    style = "font-size: 24px; font-style: normal; font-weight: lighter;") ) ), #6/14/23
#column(12, offset=3,align="center", h3("")),

#column(9,offset=3, align="center", tags$img(width="240", height="80", src=paste(Stream_folder,"USDA_PDI_Logo.jpg",sep=''))),
column(6,offset=6, align="center", tags$img(width="510", height="60", src=paste(Stream_folder,"USDA_PDI_Logo.jpg",sep=''))),


#column(12, offset=3, align="center", h3("
#  Large insertion and deletion (indel) polymorphisms, potentially altering gene structure or expression, are class of structural variants that need to be catalogued from pan-genomes. 
#  However, the nature of indels, unknown size and uneven distribution in different genome assemblies, complicates the identification process.
#  "),
#h3("
#  This process remains a challenge and often requires painstaking probing and decision-making from users.
#  "),
#h3("
#Here, we introduce BRIDGE (Blastn Recovered Insertions and Deletions near Gene Explorer) for surveying potential indels for genes of interest with 5 publicly accessible cereal pan-genomes.
#  ")
#   ),

#column(12, offset=3,align="center", h3("BRIDGEcereal currently holds 120 genomes:" ,style = "font-size:32px; color:red;")),
#column(12, offset=3,align="center", h3("11 Wheat genomes.",style = "font-size:32px; color:red;")),
#column(12, offset=3,align="center", h3("38 Maize genomes.",style = "font-size:32px; color:red;")),
#column(12, offset=3,align="center", h3("18 Sorghum genomes.",style = "font-size:32px; color:red;")),
#column(12, offset=3,align="center", h3("33 Rice genomes.",style = "font-size:32px; color:red;")),
#column(12, offset=3,align="center", h3("20 Barley genomes.",style = "font-size:32px; color:red;")),
#column(12, offset=3,align="center", h3("")),

#column(12, offset=3,align="center", tags$a(href="https://www.biorxiv.org/content/10.1101/2023.02.11.527743v1", target='_blank', h3("Reference: 
#Zhang B, Huang H, Tibbs-Cortes LE, Vanous A, Zhang Z, Sanguinet K, Garland-Campbell KA, Yu J, Li X. 
#Streamline unsupervised machine learning to survey and graph indel-based haplotypes from pan-genomes.
# bioRxiv. 2023:2023-02. doi: 10.1101/2023.02.11.527743" ,
# style = "font-size:32px; color:blue; font-style:italic;") ) ),

#column(12, offset=3,align="center", h3("")),
#column(12, offset=3,align="center", h3("")),

#column(12, offset=3,align="center", h3("Acknowledgements: We thank the USDA-ARS SCINet for computing resource and the collaboration of the USDA-ARS-Partnerships for Data Innovations (PDI,
# https://pdi.scinet.usda.gov/), which provided data stewardship solutions to enable secure data management, storage and sharing.")),

) # fluidRow

) # mainPanel


#) # sidebarLayout


      ) # For tagList
    }, # For ui function of page_0

# To add server function part for page0

server <- function(input, output, session){

#6/14/23
observeEvent(input$Submit_Q, {

#output$User_QA <- renderText({ input$Feedback })

num_q <- length(list.files(QA_folder))

current_num <- num_q +1

writeLines(input$Feedback , paste(QA_folder, current_num ,'_','user_question.txt',sep=''), sep="\n")

shinyjs::disable(id = "Submit_Q")
shinyjs::disable(id = "Feedback")

showModal( modalDialog(

  title = "We have received your submission! Thank you! ",

  easyClose = TRUE,
      
  footer = tagList(

     modalButton("Cancel"),

  )

))


  })
#6/14/23


    } # server function of Page_0


  ) # page for Page_0

} # Page_0 function


############ To combine pages together

 brochureApp(

  BRIDGEcereal_main(),

  BRIDGEcereal_Instruction(Stream_folder),

  BRIDGEcereal_Species("Wheat","IWGSC","TraesCS4A02G058900",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_wheat,Stream_folder), # 'IWGSC' ... defined as default_ref
  BRIDGEcereal_Species("Maize","B73","Zm00001eb000140",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_maize,Stream_folder),   # 'B73' ... defined as default_ref
  BRIDGEcereal_Species("Sorghum","BTx623","Sobic.001G001066",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_sorghum,Stream_folder), # 'BTx623' ... defined as default_ref
  BRIDGEcereal_Species("Rice","Nipponbare","LOC_Os01g01120",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_rice,Stream_folder), # 'Nipponbare' ... defined as default_ref
  BRIDGEcereal_Species("Barley","Morex","HORVU.MOREX.r3.1HG0000020",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_barley,Stream_folder),     # 'Morex' ... defined as default_ref

  BRIDGEcereal_Reference(Stream_folder), # 5/24/23

  BRIDGEcereal_CLIPS(),

  #BRIDGEcereal_QA(QA_folder),

  BRIDGEcereal_QA_View(QA_folder),   #6/14/23

  BRIDGEcereal_Species("Soybean","Wm82","Glyma.01G000100",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_soybean,Stream_folder),

#  BRIDGEcereal_Species("PearlMillet","Tift_23D2B1-P1-P5","Pgl_GLEAN_10002244",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_PearlMillet,Stream_folder)
  

  BRIDGEcereal_Species("Maize_ISU","B73","Zm00001eb000140",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_maize,Stream_folder)

#  BRIDGEcereal_Species("Tomato","Heinz1706","Solyc01g005210",database_folder,gff_folder,script_folder,User_folder,candidate_dir,html_tomato,Stream_folder) 

#  for(sp in All_species){Species(sp,database_folder,gff_folder,script_folder,User_folder)}

# To add many other pages

)



