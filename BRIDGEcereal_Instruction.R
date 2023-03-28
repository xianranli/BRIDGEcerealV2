### 03/28/23
BRIDGEcereal_Instruction <- function(Stream_folder){

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
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Files", "Output Files"),
     ) ),

     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Demo_mp4_1", "Demo video"),
     ) ),

     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#CLIPS", "CLIPS algorithm demonstration"),
     ) ),





############

     column(12,offset=3,align="center",   h3(
        id = "Gene_CDS",
        p(
          tags$em("Input option 1 (default): Use gene ID and CDS"),
        ),style = "font-size:32px; color:blue;"), 
      ),

     #column(12, offset=3,align="center", h5("Input option 1 (default): Use gene ID and CDS" ,style = "font-size:18px; color:blue;")),

    column(12, offset=3,align="left", h4("

1). Input the gene model ID in the Gene Name box, then click “(1) Check Gene ID” button to fill the Boxes for Reference and Chromosome.
 A hyperlink, connected to the corresponding crop’s genomic database, will be available (in blue color) for the query gene ID.
     ")),

    column(12, offset=3,align="left", h4("
2). Modify the value for Upstream and Downstream box to set the search boundaries (100kb as max input). 
Default input (kb) of Upstream/Downstream is 10% of target gene size. 
Select or unselect the Genomes to be included in the analysis (By default, all pan-genomes will be selected). 
Click “(2) Submit” button to start the process; the alignment with selected genomes will be plotted in the top right panel (as Figure 1).
    ")),

    column(12, offset=3,align="left", h4("
***Update query gene's Upstream and Downstream sizes (kb) after the Figure 1 is revealed.
 Input preferred size in Upstream (Downstream) box, BRIDGEcereal will automatically update the new results and the Figure 1.
 No need to click the “(2) Submit” button again.***
    ")),

    column(12, offset=3,align="left", h4("
***The blue boxes in reference genome indicate gene's coding sequences, and red boxes reveal other genes close to candidate gene in the defined genomic region***
    ")),

    column(12, offset=3,align="left", h4("
3). Click the “(3) Tree” button to plot the phylogenetic tree clustering genomes based on shared indels (Figure 2).
 Click the tree plot (top right) to determine the number of haplotypes based on your tree-cut.
    ")),
    
    column(12, offset=3,align="left", h4("
***If your tree-cut is not satisfied, click “(3) Tree” button again, and do a tree-cut again at your preferred branches.***
    ")),

    column(12, offset=3,align="left", h4("
4). Click the “(4) Plot selected haplotypes” button to plot the alignment among genomes representing each haplotype (Figure 3). 
    ")),

    column(12, offset=3,align="left", h4("
***Rearrange preferred plot order in the left bucket (order of plot), or drag unwanted haplotypes from the order of plot bucket to an empty bucket,
 the figure 3 will be updated automatically based on new input.***
    ")),

    column(12, offset=3,align="left", h4("
5). To trim the unwanted regions from the final haplotype presentation, single click on the third panel (Figure 3) to set the left boundary, then double click to set the right boundary. 
If coordinates of both boundaries are set, the “(5) Trim” button will be clickable. Click this button to view Figure 4.
    ")),

    column(12, offset=3,align="left", h4("
(Optional). If you need to extract DNA sequence within the trimmed region, just click the button “Extract trimmed fasta”, a fasta file with representative haplotypes and their
DNA sequences will be saved to a final downloadable .zip file.
    ")),

    column(12, offset=3,align="left", h4("
***After trimming, click the “Save .zip file to ...” button to save a .zip file to your preferred folder, and the trmmied Figure 4 in PNG format should be in there.
  If the saved PNG figure is not satisfied or need to be improved, please use PNG parameters in the left (sliderbars for width, height and pointsize) to adjust it,
  This output/save process is automatical, and you will see an updated Figure 4 (PNG) in a new .zip file folder ***
    ")),

        column(12, offset=3,align="left", h4("
(Optional). Click red “Done” button to delete your submitted job and related files.
    ")),


#    column(12, offset=3,align="left", h4("
#6). If the search boundaries (Upstream /Downstream) need to be modified, go to step 2 and run step 2-5 again.
#    ")),

################

    column(12,offset=3,align="center",   h3(
        id = "chromosome",
        p(
          tags$em("Input option 2: Upload chromosome"),
        ),style = "font-size:32px; color:blue;"), 
      ),

    column(12, offset=3,align="center", h4("
Users want to compare their sequenced chromosome (or large DNA fragment/contig) with pan-genomes on specific gene.
    ")),

    column(12, offset=3,align="left", h4("
1). Input the gene model ID in the Gene Name box, then click “(1) Check Gene ID” button to fill the Boxes for Reference and Chromosome.
 A hyperlink, connected to the corresponding crop’s genomic database, will be available (in blue color) for the query gene ID.
    ")),

    column(12, offset=3,align="left", h4("
Prepare your ***.fa and ***.fa.gz files following this instruction:
Rename query chromosome .fa file (Wheat example):
    ")),

    column(12, offset=3,align="left", verbatimTextOutput("fa_format") ),


    column(12, offset=3,align="left", h4("Here, chr should be in lowercase!")),


    column(12, offset=3,align="left", h4("
For other crops (chromosome1): Maize (>chr1); Sorghum (>chr1); Rice (>chr1); Barley (>chr1H)
    ")),

    column(12, offset=3,align="left", h4("
For contig or large fragment containing your interested gene, just use chromosome as fasta file’s first line. 
For example, your sequenced wheat contig/fragment contains a gene, 
which is known located at chromosome1A in the default reference genome IWGSC, your first fasta line should be written as >chr1A
    ")),


    column(12, offset=3,align="left", h4("
Rename this file as: 
    ")),

    column(12, offset=3,align="left", verbatimTextOutput("file_format") ),

    column(12, offset=3,align="left", h4("
Then Run (to get Parent1_chr1A.fa.gz):
    ")),

    column(12, offset=3,align="left", verbatimTextOutput("file_bgzip") ),

    column(12, offset=3,align="left", h4("
2). To upload the compressed Parent1_chr1A.fa.gz file (max size=300 MB), click “Browse” button and upload your file, 
and BRIDGEcereal will let you know your upload progress ... (Time depends on your bandwidth speed).
    ")),
    column(12, offset=3,align="left", h4("
3). When you see the progress bar (blue) showing “Upload complete”, then click “(2) Submit (large file)“ Button (yellow color) to run BRIDGEcereal. 
Your “Parent1” genome together with other pan-genomes will be plotted in Figure 1.
    ")),

###############
    column(12,offset=3,align="center",   h3(
        id = "fasta_seq",
        p(
          tags$em("Input option 3: Paste sequence"),
        ),style = "font-size:32px; color:blue;"), 
      ),

    column(12, offset=3,align="left", h4("
Users want to blast a fasta sequence (or a transcript sequence) against pan-genomes
    ")),

    column(12, offset=3,align="left", h4("
1). Input your sequence ID name, such as YourID, in the Gene Name box.
    ")),

    column(12, offset=3,align="left", h4("
DO NOT click Button “(1) Check Gene ID”! 
    ")),

    column(12, offset=3,align="left", h4("
2). Manually select the Reference and Chromosome, select “fasta_seq” in the “CDS” box, then paste the sequence in the “Your fasta sequence” box. 
For your fasta sequence, the first line should be >YourID, the same name as the input shown in the Gene Name box.
    ")),

    column(12, offset=3,align="left", h4("
3). Click “(2) Submit” button to start the process. User’s “YourID” query sequence together with other pan-genomes will be plotted in Figure 1.  
    ")),

###############
    column(12,offset=3,align="center",   h4(
        id = "Snapshot",
        p(
          tags$em("Input UI part"),
        ),style = "font-size:32px; color:blue;"), 
      ),

    column(12,offset=3,align="center", plotOutput("Output_UI",width = "100%",height = "650px") ),

    column(12,offset=3,align="center",   h4(
        id = "Figures",
        p(
          tags$em("Output Figures"),
        ),style = "font-size:32px; color:blue;"), 
      ),

    column(12,offset=3,align="center", plotOutput("Output_Fig",width = "100%",height = "650px") ),

    column(12,offset=3,align="center",   h4(
        id = "Tables",
        p(
          tags$em("Output Tables"),
        ),style = "font-size:32px; color:blue;"), 
      ),

    column(12,offset=3,align="center", plotOutput("Output_Table",width = "100%",height = "650px") ),

#column(12, offset=3,align="center", textOutput('Demo_text'), tags$head(tags$style("#Demo_text{color: blue;font-size: 18px;font-style: italic;}"))),
#column(12, offset=3,align="center", tags$iframe(width="900", height="600", src="http://10.105.85.25/BRIDGEcereal_Scinet/Demo.mp4", frameborder="0", allowfullscreen=NA)),

    column(12,offset=3,align="center",   h4(
        id = "Files",
        p(
          tags$em("Output Files in .zip"),
        ),style = "font-size:32px; color:blue;"), 
      ),

    column(12, offset = 3,align="center", DT::dataTableOutput("table_files"),style='padding-top:5px; padding-bottom:5px'),

    column(12,offset=3,align="center",   h4(
        id = "Demo_mp4_1",
        p(
          tags$em("Demo video"),
        ),style = "font-size:32px; color:blue;"), 
      ),


    column(12, offset=3, align="center", tags$iframe(width="800", height="400", src=paste(Stream_folder,"Demo_2.mp4", sep=''), frameborder="0", allowfullscreen=NA)),


    column(12,offset=3,align="center",   h4(
        id = "CLIPS",
        p(
          tags$em("CLIPS algorithm demonstration"),
        ),style = "font-size:32px; color:blue;"), 
      ),
################################################################
  #  column(12, offset=0,align="center", h3("CLIPS algorithm simulation" ,style = "font-size:32px; color:black;")),
    
    column(1, offset=3,align="center",numericInput(inputId = "HSP1_size", 
            label = "HSP1 size (bp)",
            value = 400)),
    column(1, offset=3,align="center",numericInput(inputId = "HSP2_size", 
            label = "HSP2 size (bp)",
            value = 500)),
    column(1, offset=3,align="center",numericInput(inputId = "indel_size", 
            label = "indel size (bp)",
            value = 1000)),

    column(1, offset=3,align="center",numericInput(inputId = "Variety1_start", 
            label = "Variety A start (bp)",
            value = 1)),
    column(1, offset=3,align="center",numericInput(inputId = "Variety2_start", 
            label = "Variety B start (bp)",
            value = 1)),

    column(1, offset=3,align="center",numericInput(inputId = "Variety3_start", 
            label = "Variety C start (bp)",
            value = 501)),


    column(12, offset=2,align="center",actionButton("simu", label = "CLIPS demonstration",class = "btn-warning")),
    
    column(12, offset=2, align="center", textOutput('CLIPS_text') ),
     tags$head(tags$style("#CLIPS_text{color: blue;
                                 font-size: 24px;
                                 font-style: Arial;
                                 }"
                         )
     ),

    column(12, offset=2,align="center", plotOutput("plot", width = 1200, height = 800)),
################################################################



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
  text(x=50,y=25,"May skip selection in your first run",cex=1.5,col='black')

  rect(20.5,2,80.5,102,border = 'black',lwd=2.5)

  })


 output$Output_Fig <- renderPlot({

  plot(c(1:100),c(1:100),type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n')

  lines(c(-5,105),c(60,60))
  lines(c(53,53),c(60,105))
  lines(c(-5,105),c(30,30))

  mtext("Layout of BRIDGEcereal Output Figures", side = 3,cex=2.0,col='black',at=c(50),line = 1)

  text(x=25,y=100,"Figure 1 (Output of Button (2) )",cex=2.0,col='blue')
  text(x=25,y=90,"Graphs of genes across pan-genomes",cex=2.2,col='black')
  text(x=25,y=80,"CHOICE algorithm",cex=3.0,col='red')

  text(x=75,y=100,"Figure 2 (Output of Button (3) )",cex=2.0,col='blue')
  text(x=75,y=95,"Figure2 is derived from Figure1",cex=2.0,col='purple')
  text(x=75,y=90,"Tree/cluster",cex=2.2,col='black')
  text(x=75,y=80,"CLIPS algorithm",cex=3.0,col='red')
  text(x=78,y=70,"You can interact with this tree",cex=2.0,col='black',font=4)
  text(x=78,y=65,"Tree cut on y-axis (Single click)",cex=2.0,col='black',font=4)

  text(x=50,y=55,"Figure 3 (Output of Button (4) )",cex=2.0,col='blue')
  text(x=50,y=50,"Figure 3 is derived from Figure 2",cex=2.0,col='purple')
  text(x=50,y=45,"You can interact with this graph",cex=2.0,col='black',font=4)
  text(x=50,y=40,"Trimming the graph",cex=2.0,col='black',font=4)
  text(x=50,y=35,"Single click at left (top of Fig 3); Double click at right (top of Fig 3)",cex=2.0,col='black',font=4)

  text(x=50,y=20,"Figure 4 (Output of Button (5) )",cex=2.0,col='blue')
  text(x=50,y=10,"Figure 4 is derived from Figure 3",cex=2.0,col='purple')

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
  text(x=50,y=65,"Summary of all mapped positions (HSPs) on target chromosome",cex=2.0,col='black')
  text(x=50,y=60,"and candidate gene selected for downstream analysis",cex=2.0,col='black')

  text(x=50,y=45,"Table2 (From Table1's candidate cluster(gene))",cex=2.0,col='blue')
  text(x=50,y=40,"Candidate gene selected for plotting all figures",cex=2.0,col='black')

  text(x=50,y=20,"Table3 (From Table1's Non-candidate clusters)",cex=2.0,col='blue')
  text(x=50,y=15,"Not used for BRIDGEcereal downstream analysis",cex=2.0,col='black')
  text(x=50,y=10,"BUT, maybe helpful for troubleshooting",cex=2.0,col='black')
  text(x=50,y=5,"And, provide all information about mapped positions (HSPs) for query gene",cex=2.0,col='black')

  })

  output_files<-data.frame(Files=c("GeneID.png","GeneID_Blast_Original","GeneID_Haplotype_syn","GeneID_CDS.fa","GeneID_Haplotype.fa","GeneID_Haplotype_N_Gaps","GeneID_Haplotype-Self_out_m8",
    "GeneID_ref_CDS-Haplotype_out_m8","GeneID_repMask2","GeneID_User_Selected.fa (optional)"),

                 Purpose=c("Trimmed figure (Figure 4) in png format ","blast output (original), all HSPs on target chromosome","CHOICE processed blast output, filtered HSPs on target chromosome",
                    "Gene's CDS fasta file","genomic sequences (.fa) extracted from pan-genomes","Gaps found in pan-genomes","blast output of genomic DNA in pan-genomes (pairwise)",
                    "blast output of CDS and pan-genomes","blast output of repeats","output of User selected DNA (.fa) based on trimmed coordinates"))

  colnames(output_files)<-c("output files in compressed .zip","details")

  output$table_files <-DT::renderDataTable({
  datatable(output_files,caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'BRIDGEcereal output files', htmltools::em('')
  ), filter = 'top', extensions = 'Buttons',selection = list(target = 'row+column'),
              class="cell-border stripe",
              options = list(dom = "Blfrtip",
                             buttond = list("copy", list(extend = "collection",
                                                         buttons = c("csv"),
                                                         text = "Downloads")), pageLength=20, autoWidth = TRUE,
                             searchHighlight = TRUE, filter = "top", columnDefs = list(list( className = 'dt-center', targets = "_all"))  )  ) %>% 
                            # formatStyle(columns=1:ncol(output_files), target = c("cell"),backgroundColor = c("grey") ) %>% 
                             formatStyle(c("output files in compressed .zip"), target = 'row',backgroundColor = styleEqual(c("GeneID.png","GeneID_Blast_Original","GeneID_Haplotype_syn","GeneID_CDS.fa","GeneID_Haplotype.fa","GeneID_Haplotype_N_Gaps","GeneID_Haplotype-Self_out_m8",
                              "GeneID_ref_CDS-Haplotype_out_m8","GeneID_repMask2","GeneID_User_Selected.fa (optional)"), c(rep('gold',9),rep("grey90",1) ) ) )  %>% 
                             formatStyle(columns =1:ncol(output_files), fontSize = '150%')
                              
      }) # DT::renderDataTable





################################################################
d1<-reactive({ 
    
    req(input$Variety1_start, input$Variety2_start, input$HSP1_size, input$HSP2_size, input$indel_size)

    data.frame(Variety_A=c(input$Variety1_start, input$Variety1_start+input$HSP1_size, input$Variety1_start+input$HSP1_size+1, input$Variety1_start+input$HSP1_size+1+input$HSP2_size),
               Variety_B=c(input$Variety2_start, input$Variety2_start+input$Variety1_start+input$HSP1_size-input$Variety1_start,
                            input$Variety2_start+input$Variety1_start+input$HSP1_size-input$Variety1_start+input$indel_size+1,
                            (input$Variety2_start+input$Variety1_start+input$HSP1_size-input$Variety1_start+input$indel_size+1)+(input$Variety1_start+input$HSP1_size+1+input$HSP2_size)-(input$Variety1_start+input$HSP1_size+1) ) )
               
            })

d2<-reactive({
    
    req(input$Variety1_start, input$Variety2_start, input$HSP1_size, input$HSP2_size, input$indel_size)
    
    q1<-input$Variety1_start
    q2<-input$Variety1_start+input$HSP1_size
    q3<-q2+1
    q4<-q3+input$HSP2_size

    s1<-input$Variety2_start
    s2<-input$Variety2_start+q2-q1
    s3<-s2+input$indel_size+1
    s4<-s3+q4-q3

    data.frame(V1=c("Variety_A","Variety_A","Variety_A","Variety_B","Variety_B","Variety_B"),
               V2=c("Variety_A","Variety_B","Variety_B","Variety_B","Variety_A","Variety_A"),
               V3=rep(0,6),V4=rep(0,6),V5=rep(0,6),V6=rep(0,6),
               V7=c(q1,q1,q3,1,s1,s3),V8=c(q4,q2,q4,s4,s2,s4),
               V9=c(q1,s1,s3,1,q1,q3), V10=c(q4,s2,s4,s4,q2,q4),
               V11=rep(0,6),V12=rep(0,6))
    
            })

d3<-reactive({
    
    req(input$Variety1_start, input$Variety2_start, input$HSP1_size, input$HSP2_size, input$indel_size)
    
    q1<-input$Variety1_start
    q2<-input$Variety1_start+input$HSP1_size
    q3<-q2+1
    q4<-q3+input$HSP2_size

    s1<-input$Variety3_start
    s2<-input$Variety3_start+input$HSP1_size
    s3<-s2+input$indel_size+1
    s4<-s3+input$HSP2_size

    data.frame(V1=c("Variety_A","Variety_A","Variety_A","Variety_C","Variety_C","Variety_C"),
               V2=c("Variety_A","Variety_C","Variety_C","Variety_C","Variety_A","Variety_A"),
               V3=rep(0,6),V4=rep(0,6),V5=rep(0,6),V6=rep(0,6),
               V7=c(q1,q1,q3,1,s1,s3),V8=c(q4,q2,q4,s4,s2,s4),
               V9=c(q1,s1,s3,1,q1,q3), V10=c(q4,s2,s4,s4,q2,q4),
               V11=rep(0,6),V12=rep(0,6))
    
            })

########################

observeEvent(input$simu, {


CLIPS_text_<-paste("Modify numbers in the above to dynamically view indel patterns and calculated slops",sep=' ');
output$CLIPS_text <- renderText({ CLIPS_text_ });


output$plot <- renderPlot({

###
#par( mfrow=c(2,2))
layout( matrix(c(1,2,3,4), 2, 2, byrow = TRUE),heights = c(2, 2), widths = c(2, 2) )

###
gDNAs_blast<-d2()
genomes<-c("Variety_A","Variety_B")
x_lim <- range(gDNAs_blast[,9:10])+c(0,2000)
g_lab<-genomes

plot(-100, -100, xlim = x_lim, ylim = c(-0.5,length(genomes) + 0), xlab = '', ylab = '', xaxt = "n", yaxt = "n", bty = "n");

for (g in 1:length(genomes)) {

  if (g < length(genomes)) {
   gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g + 1]);
   for (k in 1:nrow(gDNAs)) {
    polygon(c(gDNAs[k,7:8], gDNAs[k,c(10,9)]), length(genomes) - g - c(0.3, .3, 0.9, 0.9), col = adjustcolor( "gray", alpha.f = 0.5), border = "NA");
    
    legend(gDNAs[k,7]-100, length(genomes) - g + 0.2, paste('HSP',k, sep=''), bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.5 )
    
   }
  }
  #legend(max(x_lim)-2000, length(genomes) - g + 0.20,  paste(unlist(strsplit(g_lab[g],'_'))[1],unlist(strsplit(g_lab[g],'_'))[2],sep=' ') , bty = "n", adj = c(0, 0), text.col ="blue", cex = 1.5 )
  if(g==1){
  legend(max(x_lim)-2000, length(genomes) - g + 0.20,  paste(unlist(strsplit(g_lab[g],'_'))[1],unlist(strsplit(g_lab[g],'_'))[2],sep=' ') , bty = "n", adj = c(0, 0), text.col ="blue", cex = 1.5 )
  }
  if(g==2){
  legend(max(x_lim)-2000, length(genomes) - g + 0.20,  paste(unlist(strsplit(g_lab[g],'_'))[1],unlist(strsplit(g_lab[g],'_'))[2],sep=' ') , bty = "n", adj = c(0, 0), text.col ="black", cex = 1.5 )
  }

  self <- subset(gDNAs_blast,  gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g ])
   
  #print(self)
  
  for (k in 1:nrow(self)) {
   if (self[k,7] == self[k,9] & self[k,8] == self[k,10]) {
    rect(self[k,7] - 10, length(genomes) - g, self[k,8] + 10, length(genomes) - g - 0.05, col = "darksalmon", border = "NA");
   } 
  }

}

##
gDNAs_blast<-d3()
genomes<-c("Variety_A","Variety_C")
x_lim <- range(gDNAs_blast[,9:10])+c(0,2000)
g_lab<-genomes

plot(-100, -100, xlim = x_lim, ylim = c(-0.5,length(genomes) + 0), xlab = '', ylab = '', xaxt = "n", yaxt = "n", bty = "n");

for (g in 1:length(genomes)) {

  if (g < length(genomes)) {
   gDNAs <- subset(gDNAs_blast, gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g + 1]);
   for (k in 1:nrow(gDNAs)) {
    polygon(c(gDNAs[k,7:8], gDNAs[k,c(10,9)]), length(genomes) - g - c(0.3, .3, 0.9, 0.9), col = adjustcolor( "gray", alpha.f = 0.5), border = "NA");
    
    legend(gDNAs[k,7]-100, length(genomes) - g + 0.2, paste('HSP',k, sep=''), bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.5 )

   }
  }
  
 # legend(max(x_lim)-2000, length(genomes) - g + 0.20,  paste(unlist(strsplit(g_lab[g],'_'))[1],unlist(strsplit(g_lab[g],'_'))[2],sep=' ') , bty = "n", adj = c(0, 0), text.col = "blue", cex = 1.5 )
  if(g==1){
  legend(max(x_lim)-2000, length(genomes) - g + 0.20,  paste(unlist(strsplit(g_lab[g],'_'))[1],unlist(strsplit(g_lab[g],'_'))[2],sep=' ') , bty = "n", adj = c(0, 0), text.col ="blue", cex = 1.5 )
  }
  if(g==2){
  legend(max(x_lim)-2000, length(genomes) - g + 0.20,  paste(unlist(strsplit(g_lab[g],'_'))[1],unlist(strsplit(g_lab[g],'_'))[2],sep=' ') , bty = "n", adj = c(0, 0), text.col ="red", cex = 1.5 )
  }

  self <- subset(gDNAs_blast,  gDNAs_blast[,1] == genomes[g] & gDNAs_blast[,2] == genomes[g])
  
  #print(self)

  for (k in 1:nrow(self)) {
   if (self[k,7] == self[k,9] & self[k,8] == self[k,10]) {
    rect(self[k,7] - 10, length(genomes) - g, self[k,8] + 10, length(genomes) - g - 0.05, col = "darksalmon", border = "NA");
   } 
  }

}
##

gDNAs_blast<-d2()
gDNAs <- gDNAs_blast[which(gDNAs_blast$V1=='Variety_A' & gDNAs_blast$V2=='Variety_B'),]
q_hits <- as.numeric(unlist(c(gDNAs[1,c(7,8)],gDNAs[2,c(7,8)])))
s_hits <- as.numeric(unlist(c(gDNAs[1,c(9,10)],gDNAs[2,c(9,10)])))

x_lim<-c(0,max(q_hits))
y_lim<-c(0,max(s_hits))

gDNAs_blast<-d3()
gDNAs_ <- gDNAs_blast[which(gDNAs_blast$V1=='Variety_A' & gDNAs_blast$V2=='Variety_C'),]
q_hits_ <- as.numeric(unlist(c(gDNAs_[1,c(7,8)],gDNAs_[2,c(7,8)])))
s_hits_ <- as.numeric(unlist(c(gDNAs_[1,c(9,10)],gDNAs_[2,c(9,10)])))

x_lim_<-c(0,max(q_hits_))
y_lim_<-c(0,max(s_hits_))

x_lim_new<-c(0, max(x_lim[2],x_lim_[2]) )
y_lim_new<-c(0, max(y_lim[2],y_lim_[2]) )

plot(q_hits,s_hits,cex.axis=1.5, xlab='',ylab='',pch = 19,cex = 2.5,xlim=x_lim_new,ylim=y_lim_new)
mtext("Variety A", side=1, line=3.0, cex=1.5,col='blue')
mtext("Variety B", side=2, line=2.5, cex=1.5,col='black')
abline(lm(s_hits ~ q_hits),col='red')
mtext( paste("Estimated slope is:",round(summary(lm(s_hits ~ q_hits))$coefficients[2,1],6),sep=' '),side=3, line=1.0, cex=2.0,col='red')
text(x=q_hits,y=s_hits,labels = c(1,2,3,4),pos=2,cex = 2.5)
##

plot(q_hits_,s_hits_,cex.axis=1.5, xlab='',ylab='',pch = 19,cex = 2.5,xlim=x_lim_new,ylim=y_lim_new)
mtext("Variety A", side=1, line=3.0, cex=1.5,col='blue')
mtext("Variety C", side=2, line=2.5, cex=1.5,col='red')
abline(lm(s_hits_ ~ q_hits_),col='red')
mtext( paste("Estimated slope is:",round(summary(lm(s_hits_ ~ q_hits_))$coefficients[2,1],6),sep=' '),side=3, line=1.0, cex=2.0,col='red')
text(x=q_hits_,y=s_hits_,labels = c(1,2,3,4),pos=2,cex = 2.5)


  })

 })
################################################################



    } # server function of Page_1


  ) # page for Page_1

} # Page_1 function

############ To combine pages together