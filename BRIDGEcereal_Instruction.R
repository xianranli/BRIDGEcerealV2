### 03/31/23
BRIDGEcereal_Instruction <- function(Stream_folder){

  page(

    href = "/Instruction",

    ui <-  function(request){

      tagList(
        
         fluidPage(theme = shinytheme("readable")),

         h2("BRIDGEcereal instructions",style="text-align:center"),

         nav_links,

   useShinyjs(),

   #sidebarLayout(

   #sidebarPanel(

   #), # sidebarPanel

  mainPanel(

   fluidRow(

  #   column(12, offset=3,align="center", h3("BRIDGEcereal instructions:",style = "font-size: 32px; font-style: normal; font-weight: bold;")),

############
     column(12, offset=2,align="left",
     tags$li(
     tags$a(href = "#Workflow", "BRIDGEcereal workflow") ,
     ) ),

     column(12, offset=2,align="left",
     tags$li(
     tags$a(href = "#Gene_CDS", "Use gene ID and CDS"),
     ) ),

     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Step1", "Step 1: Your gene ID", style = "font-size: 24px;"),
     ) ),
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Step2", "Step 2: Adjust search boundries and submit", style = "font-size: 24px;"),
     ) ),
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Step3", "Step 3: Clustering indels variation and cutting the tree", style = "font-size: 24px;"),
     ) ),
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Step4", "Step 4: Plot selected haplotypes", style = "font-size: 24px;"),
     ) ),
     column(12, offset=3,align="left",
     tags$li(
     tags$a(href = "#Step5", "Step 5: Trimming the unwanted regions to obtain final figure", style = "font-size: 24px;"),
     ) ),

     column(12, offset=2,align="left",
     tags$li(
     tags$a(href = "#Demo_mp4_1", "Demo video"),
     ) ),

     column(12, offset=2,align="left",
     tags$li(
     tags$a(href = "#Instructions", "BRIDGEcereal instructions"),
     ) ),

    # column(12, offset=2,align="left",
    # tags$li(
    # tags$a(href = "#CLIPS", "CLIPS algorithm demonstration"),
    # ) ),


   tags$style("li a {font-size:38px; font-weight:bold; list-style-type: circle;}"),
   
   tags$head( tags$style(" li {font-size:0;}") ),

############
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),

column(12,offset=3,align="center",   h3(
        id = "Workflow",
        p(
          tags$em("BRIDGEcereal workflow"),
        ),style = "font-size:24px; color:blue;"), 
      ),

column(12, offset=3,align="left", h4("
Red outline denotes steps with parameter selected based on user’s discretion. 
The brackets indicate the steps involved in CHOICE and CLIPS algorithms.
     ",style = "font-size: 24px; font-style: normal; font-weight: lighter;")),
column(12, offset=3,align="left", h4("
Hover your mouse over boxed text to view details. 
     ",style = "font-size: 24px; font-style: normal; font-weight: bold;")),

column(4, offset=3,align="left", plotOutput("plotWorkflow",click = NULL,dblclick = NULL,hover = "plotWorkflow_hover", width = "120%",height = 'auto')),
column(4,offset=1, align="center", textOutput("info_Workflow")), #03/07/23
     tags$head(tags$style("#info_Workflow{
                                 font-size: 32px;
                                 font-style: normal;
                                 font-weight: lighter;
                                 }"
                         )
     ),

#uiOutput("LinkButton"),



column(12, offset=3,align="center", h3("")),

column(12,offset=3,align="center",   h3(
        id = "Gene_CDS",
        p(
          tags$em("Use gene ID and CDS"),
        ),style = "font-size:28px; color:blue;"), 
      ),

     #column(12, offset=3,align="center", h5("Input option 1 (default): Use gene ID and CDS" ,style = "font-size:18px; color:blue;")),




column(12,offset=2,align="left",   h3(
        id = "Step1",
        p(
          tags$em("Step 1"),
        ),style = "font-size:22px; color:blue;"), 
      ),


column(3,offset=2, align="center", tags$img(width="812", height="487", src=paste(Stream_folder,"Steps_1.png",sep='')) ),
column(3, offset=4,align="center", h4("
1). Input the gene model ID in the Gene Name box, then click “(1) Check Gene ID” button to fill the Boxes for Reference and Chromosome.
 A hyperlink, connected to the corresponding crop’s genomic database, will be available (in blue color) for the query gene ID.
     ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),
    ),


column(12, offset=3,align="center", h3("")),
column(12,offset=2,align="left",   h3(
        id = "Step2",
        p(
          tags$em("Step 2"),
        ),style = "font-size:22px; color:blue;"), 
      ),

column(3,offset=2, align="center", tags$img(width="754", height="496", src=paste(Stream_folder,"Steps_2.png",sep=''))),
column(3, offset=4,align="center", h4("
2). Modify the value for Upstream and Downstream box to set the search boundaries (100kb as max input). 
Default input (kb) of Upstream/Downstream is 10% of target gene size. 
Select or unselect the Genomes to be included in the analysis (By default, all pan-genomes will be selected). 
Click “(2) Submit” button to start the process; the alignment with selected genomes will be plotted in the top right panel (as Panel 1).
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;"),

h4("
For more information about the two default parameters, please refer to the 'Two default parameters explained' part in 'BRIDGEcereal instructions'.
    ",style = "font-size: 22px; font-style: normal; font-weight: bold;"),


),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),
column(6, offset=2,align="center", h3("Panel 1:")),
column(3,offset=2, align="center", tags$img(width="725", height="325", src=paste(Stream_folder,"Steps_3.png",sep=''))),
column(3, offset=4, align="center", h4("
***Update query gene's Upstream and Downstream sizes (kb) after the Panel 1 is revealed.
 Input preferred size in Upstream (Downstream) box, BRIDGEcereal will automatically update the new results and the Panel 1.
 No need to click the “(2) Submit” button again.***
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),


#    column(12, offset=3,align="left", h4("
#***The blue boxes in reference genome indicate gene's coding sequences, and red boxes reveal other genes close to candidate gene in the defined genomic region***
#    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),
column(12, offset=3,align="center", h3("")),
column(12,offset=2,align="left",   h3(
        id = "Step3",
        p(
          tags$em("Step 3"),
        ),style = "font-size:22px; color:blue;"), 
      ),

column(6, offset=2 ,align="center", h3("Panel 2:")),
column(3,offset=2, align="center", tags$img(width="805", height="514", src=paste(Stream_folder,"Steps_4.png",sep=''))),
column(3, offset=4,align="center", h4("
3). Click the “(3) Tree” button to plot the phylogenetic tree clustering genomes based on shared indels (Panel 2).
 Click the tree plot (top right) to determine the number of haplotypes based on your tree-cut. 
  ***If your tree-cut is not satisfied, click “(3) Tree” button again, and do a tree-cut again at your preferred branches.***
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),
#column(12, offset=3,align="center", h3("Figure 2:")),


column(12, offset=3,align="center", h3("")),
column(12,offset=2,align="left",   h3(
        id = "Step4",
        p(
          tags$em("Step 4"),
        ),style = "font-size:22px; color:blue;"), 
      ),

    column(12, offset=2,align="left", h4("
4). Click the “(4) Plot selected haplotypes” button to plot the alignment among genomes representing each haplotype (Panel 3). 
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("Panel 3:")),
column(6,offset=3, align="center", tags$img(width="1490", height="342", src=paste(Stream_folder,"Steps_5.png",sep=''))),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),


column(3,offset=3, align="center", tags$img(width="751", height="209", src=paste(Stream_folder,"Steps_6.png",sep=''))),
column(3, offset=3,align="center", h4("
***Rearrange preferred plot order in the left bucket (order of plot), or drag unwanted haplotypes from the order of plot bucket to an empty bucket,
 the panel 3 will be updated automatically based on new input.***
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),


column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),


column(12,offset=2,align="left",   h3(
        id = "Step5",
        p(
          tags$em("Step 5"),
        ),style = "font-size:22px; color:blue;"), 
      ),

column(12, offset=2,align="left", h4("
5). To trim the unwanted regions from the final haplotype presentation, single click on the third panel (Panel 3) to set the left boundary, then double click to set the right boundary. 
If coordinates of both boundaries are set, the “(5) Trim” button will be clickable. Click this button to view Panel 4.
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),
column(6,offset=3, align="center", tags$img(width="1492", height="455", src=paste(Stream_folder,"Steps_7.png",sep=''))),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("Panel 4:")),
column(6,offset=3, align="center", tags$img(width="1561", height="487", src=paste(Stream_folder,"Steps_8.png",sep=''))),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),


column(12, offset=2,align="left", h4("
(Optional). If you need to extract DNA sequence within the trimmed region, just click the button “Extract trimmed fasta”, a fasta file with representative haplotypes and their
DNA sequences will be saved to a final downloadable .zip file.
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

column(12, offset=2,align="left", h4("
***After trimming, click the “Save .zip file to ...” button to save a .zip file to your preferred folder, and the trimmied Panel 4 in PNG format should be in there.
  If the saved PNG figure is not satisfied or need to be improved, please use PNG parameters in the left (sliderbars for width, height and pointsize) to adjust it,
  This output/save process is automatical, and you will see an updated Panel 4 (PNG) in a new .zip file folder ***
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),

column(12, offset=2,align="left", h4("
(Optional). Click red “Done” button to delete your submitted job and related files.
    ",style = "font-size: 22px; font-style: normal; font-weight: lighter;")),


#    column(12, offset=3,align="left", h4("
#6). If the search boundaries (Upstream /Downstream) need to be modified, go to step 2 and run step 2-5 again.
#    ")),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),

    column(12,offset=3,align="center",   h4(
        id = "Demo_mp4_1",
        p(
          tags$em("Demo video"),
        ),style = "font-size:28px; color:blue;"), 
      ),


column(12, offset=3, align="center", tags$iframe(width="1200", height="800", src=paste(Stream_folder,"Demo_2.mp4", sep=''), frameborder="0", allowfullscreen=NA)),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),

column(12,offset=3,align="center",   h3(
        id = "Instructions",
        p(
          tags$em("BRIDGEcereal instructions"),
        ),style = "font-size:24px; color:blue;"), 
      ),

#column(12, offset=3, align="center", tags$iframe(width="1400", height="800", src=paste(Stream_folder,"BRIDGEcereal_instructions.pdf", sep=''), frameborder="0", allowfullscreen=NA)),
column(12, offset=3, align="center", tags$a("Click here to get the BRIDGEcereal_instructions.pdf", href=paste(Stream_folder,"BRIDGEcereal_instructions.pdf", sep=''),target='_blank',style = "font-size: 36px;" ) ),

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),


  #  column(12,offset=3,align="center",   h4(
  #      id = "CLIPS",
  #      p(
  #        tags$em("CLIPS algorithm demonstration"),
  #      ),style = "font-size:28px; color:blue;"), 
  #    ),
################################################################




) # fluidRow

   ) # mainPanel

#) # sidebarLayout

      ) # For tagList
    }, # For ui function of page_1
    

# To add server function part for page1

server <- function(input, output, session){

#output$Demo_text <- renderText({paste("Short demo video:",sep="")})


 output$plotWorkflow <- renderPlot({
  plot(c(1:100),c(1:100),type= "n", xlab = "", ylab = "",xaxt='n',yaxt='n')
 # plot(c(1:100),c(1:100),type= "n", xlab = "", ylab = "")

  rect(20,95,80,102,border = 'red',lwd=3.5)
  text(x=50,y=100,"A gene model ID or",cex=1.5,col='black')
  text(x=50,y=97,"a transcript sequence",cex=1.5,col='black')
  arrows(x0=50, y0=95, x1 =50, y1 = 90, length = 0.25,lwd=2.5)
  
  rect(20,83,80,90,border = 'black',lwd=2.5)
  text(x=50,y=87,"Targeted chromosome",cex=1.5,col='black')
  arrows(x0=50, y0=83, x1 =50, y1 = 78, length = 0.25,lwd=2.5)

  rect(20,71,80,78,border = 'black',lwd=2.5)
  text(x=50,y=75,"Assemblies to be included",cex=1.5,col='black')
  arrows(x0=50, y0=71, x1 =50, y1 = 66, length = 0.25,lwd=2.5)

  rect(85,71,102,78,border = 'red',lty=2, lwd=3.5)
  text(x=93,y=76,"Uploading",cex=1.0,col='black')
  text(x=93,y=74,"chromosome",cex=1.0,col='black')
  arrows(x0=85, y0=74, x1 =80, y1 = 74, length = 0.25,lwd=2.5)


  rect(20,59,80,66,border = 'red',lwd=3.5)
  text(x=50,y=64,"Up- and down-",cex=1.5,col='black')
  text(x=50,y=61,"stream search boundary",cex=1.5,col='black')
  arrows(x0=50, y0=59, x1 =50, y1 = 54, length = 0.25,lwd=2.5)
  
  rect(20,47,80,54,border = 'black',lwd=2.5)
  text(x=50,y=52,"Visualizing pattern from selected",cex=1.5,col='black')
  text(x=50,y=49,"assemblies with the same length",cex=1.5,col='black')
  arrows(x0=50, y0=47, x1 =50, y1 =42, length = 0.25,lwd=2.5)


  rect(20,35,80,42,border = 'black',lwd=2.5)
  text(x=50,y=39,"Clustering assemblies",cex=1.5,col='black')
  arrows(x0=50, y0=35, x1 =50, y1 =30, length = 0.25,lwd=2.5)


  rect(20,23,80,30,border = 'red',lwd=3.5)
  text(x=50,y=28,"Determining the number",cex=1.5,col='black')
  text(x=50,y=25,"of haplotypes",cex=1.5,col='black')
  arrows(x0=50, y0=23, x1 =50, y1 =20, length = 0.25,lwd=2.5)

  polygon(x=c(20,50,80,50,20),y=c(15,20,15,10,15),border = 'red',lwd=3.5)
  text(x=50,y=17,"New",cex=1.5,col='black')
  text(x=50,y=14,"boundary",cex=1.5,col='black')
  arrows(x0=50, y0=10, x1 =50, y1 =6, length = 0.25,lwd=2.5)

  text(x=85,y=17,"Yes",cex=1.5,col='black')
  
  text(x=55,y=8,"No",cex=1.5,col='black')

  rect(20,-1,80,6,border = 'red',lwd=3.5)
  text(x=50,y=4,"Ordering and trimming haplotypes",cex=1.5,col='black')
  text(x=50,y=1,"for final legible representation",cex=1.5,col='black')

  lines(c(80,90),c(15,15))
  lines(c(90,90),c(15,62))
  arrows(x0=90, y0=62, x1 =80, y1 =62, length = 0.25,lwd=2.5)

  lines(c(15,18),c(98,98))
  lines(c(15,18),c(60,60))
  lines(c(15,15),c(60,98))
  text(x=11,y=74,srt = 90, "CHOICE",cex=2.0,col='black')

  lines(c(15,18),c(58,58))
  lines(c(15,18),c(23,23))
  lines(c(15,15),c(23,58))
  text(x=11,y=37,srt = 90, "CLIPS",cex=2.0,col='black')


  }, height = function() {1000})


observeEvent(input$plotWorkflow_hover,{

req(input$plotWorkflow_hover)

x_axis<-reactive({
    
    x_info<-input$plotWorkflow_hover$x
            })

y_axis<-reactive({

    y_info<-input$plotWorkflow_hover$y
            })

x_axis1<-x_axis()
y_axis1<-y_axis()

if(y_axis1>=95 & y_axis1<=102 & x_axis1>=20 & x_axis1<=80){

  output$info_Workflow <- renderText({paste("Input the gene ID, then click button (1). 
    The referene and chromosome boxes will be filled in automatically. If you want to submit a CDS sequence, please 
    refer to the 'Paste sequence' part in 'BRIDGEcereal instructions'.",sep='') })

} else if(y_axis1>=83 & y_axis1<=90 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Filled in automatically when you have a gene ID",sep='') })

} else if(y_axis1>=71 & y_axis1<=78 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Filled in automatically when you have a gene ID",sep='') })

} else if(y_axis1>=59 & y_axis1<=66 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Filled in automatically when you have a gene ID. 
    By default, 10% of query gene size will be filled in for up- and down-stream of query gene. 
    Modify them based on your preferred sizes.",sep='') })

} else if(y_axis1>=47 & y_axis1<=54 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Click (2) Submit button to start the analysis, and a pan-genome 
   graph will be plotted in panel 1. The (3) Tree button (blue) will be clickable after the process.",sep='') })

} else if(y_axis1>=35 & y_axis1<=42 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Click (3) Tree button (blue) to cluster assemblies, and then panel 2 will be plotted.",sep='') })

} else if(y_axis1>=23 & y_axis1<=30 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Do the tree-cut on the panel 2 based on your preferred height, determining 
    the number of haplotypes for the next steps.",sep='') })

} else if(y_axis1>=10 & y_axis1<=20 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("You may update/reset up- and down-stream boundaries after panel 1 or panel 2 is revealed.",sep='') })

} else if(y_axis1>=-1 & y_axis1<=6 & x_axis1>=20 & x_axis1<=80){

   output$info_Workflow <- renderText({paste("Click (4) PLot selected haplotypes button to plot panel 3. Reorder the representatives in the 'Order of plot', and trimming haplotypes at 
    preferred positions.",sep='') })

} else if(y_axis1>=71 & y_axis1<=78 & x_axis1>=85 & x_axis1<=102){

   output$info_Workflow <- renderText({paste("Please refer to the section 'Upload a contig or a chromosome' in 'BRIDGEcereal instructions'.",sep='') })

} else {

  output$info_Workflow <- renderText({paste("",sep='') })
}



 })







    } # server function of Page_1


  ) # page for Page_1

} # Page_1 function

############ To combine pages together