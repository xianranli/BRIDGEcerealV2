### 03/07/23
Instruction <- function(){

  page(

    href = "/Instruction",

    ui <-  function(request){

      tagList(
        
         fluidPage(theme = shinytheme("readable")),

         h1("This is BRIDGEcereal instruction page",style="text-align:center"),

         nav_links,

   useShinyjs(),

   #sidebarLayout(

   #sidebarPanel(

   #), # sidebarPanel

  mainPanel(

   fluidRow(

     column(12, offset=3,align="center", h3("BRIDGEcereal instructions:")),

############
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Gene_CDS", "Input option 1 (default): Use gene ID and CDS"),
     ) ),

     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#chromosome", "Input option 2: Upload chromosome"),
     ) ),

     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#fasta_seq", "Input option 3: Paste sequence"),
      ) ),

     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Snapshot", "Input UI part"),
     ) ),
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Figures", "Output Figures"),
     ) ),
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Tables", "Output Tables"),
     ) ),

 #    column(12, offset=3,align="left",
 #    tags$li(
 #    tags$a(href = "#Files", "Output Files"),
 #    ) ),

 #    column(12, offset=3,align="left",
 #    tags$li(
 #    tags$a(href = "#Demo_mp4_1", "Demo video"),
 #    ) ),
############

     column(12,offset=3,align="center",   h3(
        id = "Gene_CDS",
        p(
          tags$em("Input option 1 (default): Use gene ID and CDS"),
        ),style = "font-size:18px; color:blue;"), 
      ),

     #column(12, offset=3,align="center", h5("Input option 1 (default): Use gene ID and CDS" ,style = "font-size:18px; color:blue;")),

    column(12, offset=3,align="left", h5("

1). Input the gene model ID in the Gene Name box, then click “(1) Check Gene ID” button to fill the Boxes for Reference and Chromosome.
 A hyperlink, connected to the corresponding crop’s website/database, will be available (in blue color) for the query gene ID. 
 User may use this link to look up more information on this gene.
     ")),

    column(12, offset=3,align="left", h5("
2). Modify the value for Upstream and Downstream box to set the search boundaries (100kb as max input). 
Default input (length/size) of Upstream /Downstream is 10% of target gene size. 
Select or unselect the Genomes to be included in the analysis (All pan-genomes as default selections). 
Click “(2) Submit” button to start the process; the alignment with all selected genomes will be plotted in the top right panel.
    ")),

    column(12, offset=3,align="left", h5("
***User may update query gene's Upstream and Downstream sizes after the Figure1 is revealed.
 Input preferred size in Upstream (Downstream) box, BRIDGEcereal will automatically update new results and Figures.
 No need to click the Submit button again.***
    ")),

    column(12, offset=3,align="left", h5("
3). Click the “(3) Tree” button to plot the phylogenetic tree clustering genomes based on shared indels.
 Click the tree plot (top right) to determine the number of haplotypes based on User’s tree-cut.
    ")),

    column(12, offset=3,align="left", h5("
4). Click the “(4) Plot selected haplotypes” button to plot the alignment among genomes representing each haplotype. 
    ")),

    column(12, offset=3,align="left", h5("
5). To trim the unwanted regions from the final haplotype presentation, single click on the third panel to set the left boundary, then double click to set the right boundary. 
If coordinates of both boundaries are set, the “(5) Trim” button will be clickable. 
    ")),

    column(12, offset=3,align="left", h5("
6). If the search boundaries (Upstream /Downstream) need to be modified, go to step 2 and run step 2-5 again.
    ")),
################

    column(12,offset=3,align="center",   h3(
        id = "chromosome",
        p(
          tags$em("Input option 2: Upload chromosome"),
        ),style = "font-size:18px; color:blue;"), 
      ),

    column(12, offset=3,align="center", h4("
Users want to compare their sequenced chromosome (or large DNA fragment/contig) with pan-genomes on specific gene.
    ")),

    column(12, offset=3,align="left", h5("
1). Input the gene model ID in the Gene Name box, then click “(1) Check Gene ID” button to fill the Boxes for Reference and Chromosome.
 A hyperlink, connected to the corresponding crop’s website/database, will be available (in blue color) for the query gene ID. 
 User may use this link to look up more information on this gene.
    ")),

    column(12, offset=3,align="left", h5("
User needs to prepare their ***.fa and ***.fa.gz files following this instruction:
Rename query chromosome .fa file (Wheat example):
    ")),

    column(12, offset=3,align="left", verbatimTextOutput("fa_format") ),


    column(12, offset=3,align="left", h5("Here, chr should be in lowercase!")),


    column(12, offset=3,align="left", h5("
For other crops (chromosome1): Maize (>chr1); Sorghum (>chr1); Rice (>chr1); Barley (>chr1H)
    ")),

    column(12, offset=3,align="left", h5("
For contig or large fragment containing your interested gene, just use chromosome as fasta file’s first line. 
For example, your sequenced wheat contig/fragment contains a gene, 
which is known located at chromosome1A in the default reference genome IWGSC, your first fasta line should be written as >chr1A
    ")),


    column(12, offset=3,align="left", h5("
Rename this file as: 
    ")),

    column(12, offset=3,align="left", verbatimTextOutput("file_format") ),

    column(12, offset=3,align="left", h5("
Then Run (to get Parent1_chr1A.fa.gz):
    ")),

    column(12, offset=3,align="left", verbatimTextOutput("file_bgzip") ),

    column(12, offset=3,align="left", h5("
2). To upload the compressed Parent1_chr1A.fa.gz file (max size=300 MB), click “Browse” button and upload your file, 
and BRIDGEcereal will let you know your upload progress ... (Time depends on your bandwidth speed).
    ")),
    column(12, offset=3,align="left", h5("
3). When you see the progress bar (blue) showing “Upload complete”, then click “(2) Submit (large file)“ Button (yellow color) to run BRIDGEcereal. 
User’s “Parent1” genome together with other pan-genomes will be plotted in Figure1.
    ")),

###############
    column(12,offset=3,align="center",   h3(
        id = "fasta_seq",
        p(
          tags$em("Input option 3: Paste sequence"),
        ),style = "font-size:18px; color:blue;"), 
      ),

    column(12, offset=3,align="left", h4("
Users want to blast a fasta sequence (or a transcript sequence) against pan-genomes
    ")),

    column(12, offset=3,align="left", h5("
1). Input your sequence ID name, such as YourID, in the Gene Name box.
    ")),

    column(12, offset=3,align="left", h5("
DO NOT click Button “(1) Check Gene ID”! 
    ")),

    column(12, offset=3,align="left", h5("
2). Manually select the Reference and Chromosome, select “fasta_seq” in the “CDS” box, then paste the sequence in the “Your fasta sequence” box. 
For your fasta sequence, the first line should be >YourID, the same name as the input shown in the Gene Name box.
    ")),

    column(12, offset=3,align="left", h5("
3). Click “(2) Submit” button to start the process. User’s “YourID” query sequence together with other pan-genomes will be plotted in Figure1.  
    ")),

###############
    column(12,offset=3,align="center",   h4(
        id = "Snapshot",
        p(
          tags$em("Input UI part"),
        ),style = "font-size:18px; color:blue;"), 
      ),

    column(12,offset=3,align="center", plotOutput("Output_UI",width = "100%",height = "650px") ),

    column(12,offset=3,align="center",   h4(
        id = "Figures",
        p(
          tags$em("Output Figures"),
        ),style = "font-size:18px; color:blue;"), 
      ),

    column(12,offset=3,align="center", plotOutput("Output_Fig",width = "100%",height = "650px") ),

    column(12,offset=3,align="center",   h4(
        id = "Tables",
        p(
          tags$em("Output Tables"),
        ),style = "font-size:18px; color:blue;"), 
      ),

    column(12,offset=3,align="center", plotOutput("Output_Table",width = "100%",height = "650px") ),

#column(12, offset=3,align="center", textOutput('Demo_text'), tags$head(tags$style("#Demo_text{color: blue;font-size: 18px;font-style: italic;}"))),
#column(12, offset=3,align="center", tags$iframe(width="900", height="600", src="http://10.105.85.25/BRIDGEcereal_Scinet/Demo.mp4", frameborder="0", allowfullscreen=NA)),

  #  column(12,offset=3,align="center",   h4(
  #      id = "Files",
  #      p(
  #        tags$em("Output Files"),
  #      ),style = "font-size:18px; color:blue;"), 
  #    ),

  #  column(12, offset = 3,align="center", DT::dataTableOutput("table_files"),style='padding-top:5px; padding-bottom:5px'),

  #  column(12,offset=3,align="center",   h4(
  #      id = "Demo_mp4_1",
  #      p(
  #        tags$em("Demo video"),
  #      ),style = "font-size:18px; color:blue;"), 
  #    ),

) # fluidRow

   ) # mainPanel

#) # sidebarLayout

      ) # For tagList
    }, # For ui function of page_1
    

# To add server function part for page1

server <- function(input, output, session){

#output$Demo_text <- renderText({paste("Short demo video:",sep="")})

 output$fa_format <- renderText({paste(">chr1A","ATGCATGC...",sep="\n")})

 output$file_format <- renderText({paste("Parent1_chr1A.fa (the second one as Parent2_chr1A.fa)",sep="")})

 output$file_bgzip <- renderText({paste("bgzip Parent1_chr1A.fa",sep="")})


 output$Output_UI <- renderPlot({

  plot(c(1:100),c(1:100),type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n',bty="n")

  mtext("Snapshot of UI part", side = 3,cex=2.0,col='black',at=c(50),line = 0)

  rect(25,90,75,95,border = 'black')
  arrows(80, 100, x1 = 75, y1 = 95, length = 0.25,lwd=2.5)
  rect(40,82,60,87,border = 'black',col='orange')
  arrows(65, 88, x1 = 60, y1 = 85, length = 0.25,lwd=2.5)

  text(x=50,y=92,"Input user's gene ID",cex=2.0,col='blue')
  text(x=50,y=85,"Click (1) Check Gene ID",cex=1.2,col='blue')

  rect(25,75,75,80,border = 'black')
  rect(25,69,75,74,border = 'black')

  text(x=50,y=77,"Reference genome filled in automatically",cex=1.3,col='black')
  text(x=50,y=71,"Chromosome filled in automatically",cex=1.3,col='black')
  arrows(50, 68, x1 = 50, y1 = 60, length = 0.25,lwd=7.0)
  rect(25,55,75,59,border = 'black')

  text(x=50,y=57,"Input your Upstream (kb), or use default length",cex=1.2,col='blue')
  rect(25,50,75,54,border = 'black')
  text(x=50,y=52,"Input your Downstream (kb), or use default length",cex=1.2,col='blue')

  rect(25,44,75,48,border = 'black')
  text(x=50,y=46,"By default, all Pan-genomes selected",cex=1.2,col='black')

  rect(25,39,75,43,border = 'red',lwd=3.0)
  text(x=50,y=41,"Distance filter between mapped clusters (Slider)",cex=1.2,col='red')
  rect(25,34,75,38,border = 'red',lwd=3.0)
  text(x=50,y=36,"Expected CDS size compared to Reference (Slider Range)",cex=1.2,col='red')

  rect(40,5,60,11,border = 'black',col='orange')
  text(x=50,y=7.5,"Click (2) Submit",cex=1.8,col='blue')
  arrows(35, 13, x1 = 40, y1 = 11, length = 0.25,lwd=3.5)

  text(x=50,y=30,"The two parameters in red color are used by CHOICE algorithm.",cex=1.5,col='black')
  text(x=50,y=25,"Users may skip selection in their first run ...",cex=1.5,col='black')

  rect(20.5,2,80.5,102,border = 'black',lwd=2.5)

  })


 output$Output_Fig <- renderPlot({

  plot(c(1:100),c(1:100),type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n')

  lines(c(-5,105),c(60,60))
  lines(c(53,53),c(60,105))
  lines(c(-5,105),c(30,30))

  mtext("Layout of BRIDGEcereal Output Figures", side = 3,cex=2.0,col='black',at=c(50),line = 1)

  text(x=25,y=100,"Figure1 (Output of Button (2) )",cex=2.0,col='blue')
  text(x=25,y=90,"Graphs of genes across pan-genomes",cex=2.2,col='black')
  text(x=25,y=80,"CHOICE algorithm",cex=3.0,col='red')

  text(x=75,y=100,"Figure2 (Output of Button (3) )",cex=2.0,col='blue')
  text(x=75,y=95,"Figure2 is derived from Figure1",cex=2.0,col='purple')
  text(x=75,y=90,"Tree/cluster",cex=2.2,col='black')
  text(x=75,y=80,"CLIPS algorithm",cex=3.0,col='red')
  text(x=78,y=70,"Users can interact with this tree",cex=2.0,col='black',font=4)
  text(x=78,y=65,"Tree cut on y-axis (Single click)",cex=2.0,col='black',font=4)

  text(x=50,y=55,"Figure3 (Output of Button (4) )",cex=2.0,col='blue')
  text(x=50,y=50,"Figure3 is derived from Figure2",cex=2.0,col='purple')
  text(x=50,y=45,"Users can interact with this graph",cex=2.0,col='black',font=4)
  text(x=50,y=40,"Trimming the graph",cex=2.0,col='black',font=4)
  text(x=50,y=35,"Single click at left (top of Fig3); Double click at right (top of Fig3)",cex=2.0,col='black',font=4)

  text(x=50,y=20,"Figure4 (Output of Button (5) )",cex=2.0,col='blue')
  text(x=50,y=10,"Figure4 is derived from Figure3",cex=2.0,col='purple')

  })


output$Output_Table <- renderPlot({

  plot(c(1:100),c(1:100),type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n')

  lines(c(-5,105),c(75,75))
  lines(c(-5,105),c(50,50))
  lines(c(-5,105),c(25,25))

  mtext("Arrangement of BRIDGEcereal Output Tables", side = 3,cex=2.0,col='black',at=c(50),line = 1)

  text(x=50,y=95,"Variety Table (From CLIPS algorithm and User's tree cut)",cex=2.5,col='red')
  text(x=50,y=85,"Varieties in each group based on User's tree cut",cex=3.5,col='purple')

  text(x=50,y=70,"Table1 (From CHOICE algorithm)",cex=2.0,col='blue')
  text(x=50,y=65,"Summary of all mapped positions on target chromosome",cex=2.0,col='black')
  text(x=50,y=60,"and candidate gene selected for downstream analysis",cex=2.0,col='black')

  text(x=50,y=45,"Table2 (From Table1's candidate cluster(gene))",cex=2.0,col='blue')
  text(x=50,y=40,"Candidate gene selected for plotting all figures",cex=2.0,col='black')

  text(x=50,y=20,"Table3 (From Table1's Non-candidate clusters)",cex=2.0,col='blue')
  text(x=50,y=15,"Not used for BRIDGEcereal downstream analysis",cex=2.0,col='black')
  text(x=50,y=10,"BUT, maybe helpful for troubleshooting",cex=2.0,col='black')
  text(x=50,y=5,"And, provide all information about mapped positions for query gene",cex=2.0,col='black')

  })

#output_files<-data.frame(Files=c("XXX_XXX"),Purpose=c("Plotting"))

#  colnames(output_files)<-c("Output File name","Purpose")

#output$table_files <-DT::renderDataTable({
#  datatable(output_files,caption = htmltools::tags$caption(
#    style = 'caption-side: bottom; text-align: center;',
#    'BRIDGEcereal output files', htmltools::em('')
#  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
#              class="cell-border stripe",
#              options = list(dom = "Blfrtip",
#                             buttond = list("copy", list(extend = "collection",
#                                                         buttons = c("csv"),
#                                                         text = "Downloads")), pageLength=20, autoWidth = TRUE,
#                             searchHighlight = TRUE, filter = "top", columnDefs = list(list( className = 'dt-center', targets = "_all"))  )  ) %>% formatStyle(columns=ncol(output_files), target = c("cell"),backgroundColor = c("gold") )
#      }) # DT::renderDataTable

    } # server function of Page_1


  ) # page for Page_1

} # Page_1 function

############ To combine pages together