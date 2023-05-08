### 03/31/23
BRIDGEcereal_CLIPS <- function(){

  page(

    href = "/CLIPS",

    ui <-  function(request){

      tagList(
        
         fluidPage(theme = shinytheme("readable")),

         h2("CLIPS algorithm demo",style="text-align:center"),

         nav_links,

   useShinyjs(),

   #sidebarLayout(

   #sidebarPanel(

   #), # sidebarPanel

  mainPanel(

   fluidRow(

############
    
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


   # column(12, offset=2,align="center",actionButton("simu", label = "CLIPS demonstration",class = "btn-warning")),
    
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

#observeEvent(input$simu, {


CLIPS_text_<-paste("Modify numbers in the above to dynamically view indel patterns and calculated slopes",sep=' ');
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

 #})
################################################################



    } # server function of Page_1


  ) # page for Page_1

} # Page_1 function

############ To combine pages together