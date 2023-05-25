# 05/01/23
# BRIDGEcereal_Reference

BRIDGEcereal_Reference <- function(Stream_folder){

page(
    href = "/Reference",


    ui <-  function(request){

      tagList(
        

        fluidPage(theme = shinytheme("readable")),


        h2("This is BRIDGEcereal reference page",style="text-align:center"),

        nav_links,


useShinyjs(),


#sidebarLayout(

#sidebarPanel(


#), # sidebarPanel


mainPanel(

fluidRow(

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h4("BRIDGEcereal currently holds 120 genomes:" ,style = "font-size:24px; color:black;")),

column(12, offset=3,align="center", h4("Wheat",style = "font-size:24px; color:black;")),

column(12, offset=3, align="left",tags$a(href="https://www.nature.com/articles/s41586-020-2961-x", style = "color:black; font-size:20px", target='_blank',  h5("1, 
Walkowiak, S., Gao, L., Monat, C., Haberer, G., Kassa, M.T., Brinton, J., Ramirez-Gonzalez, R.H., Kolodziej, M.C., Delorean, E., Thambugala, D., et al. (2020). 
Multiple wheat genomes reveal global variation in modern breeding. Nature 588:277-283.
  ",style = "font-size: 20px; font-style: normal; font-weight: lighter;") ) ),


column(12, offset=3,align="center", h4("Maize",style = "font-size:24px; color:black;")),

column(12, offset=3, align="left", tags$a(href="https://www.science.org/doi/full/10.1126/science.abg5289?rfr_dat=cr_pub++0pubmed&url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org", style = "color:black; font-size:20px",target='_blank',h5("2, 
Hufford, M.B., Seetharam, A.S., Woodhouse, M.R., Chougule, K.M., Ou, S., Liu, J., Ricci, W.A., Guo, T., Olson, A., Qiu, Y., et al. (2021). 
De novo assembly, annotation, and comparative analysis of 26 diverse maize genomes. Science 373:655-662.
  ",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),

tags$a(href="https://www.nature.com/articles/s41588-022-01283-w", style = "color:black; font-size:20px",target='_blank',h5("3, 
Wang, B., Hou, M., Shi, J., Ku, L., Song, W., Li, C., Ning, Q., Li, X., Li, C., Zhao, B., et al. (2023). 
De novo genome assembly and analyses of 12 founder inbred lines provide insights into maize heterosis. Nat Genet 55:312-323.
  ",style = "font-size: 20px; font-style: normal; font-weight: lighter;") ) ) ),

column(12, offset=3,align="center", h4("Sorghum",style = "font-size:24px; color:black;")),

column(12, offset=3, align="left", tags$a(href="https://www.nature.com/articles/s41477-021-00925-x", style = "color:black; font-size:20px",target='_blank',h5("4, 
Tao, Y., Luo, H., Xu, J., Cruickshank, A., Zhao, X., Teng, F., Hathorn, A., Wu, X., Liu, Y., Shatte, T., et al. (2021). 
Extensive variation within the pan-genome of cultivated and wild sorghum. Nat Plants 7:766-773.
  ",style = "font-size: 20px; font-style: normal; font-weight: lighter;")) ),

column(12, offset=3,align="center", h4("Rice",style = "font-size:24px; color:black;")),

column(12, offset=3, align="left", tags$a(href="https://www.sciencedirect.com/science/article/pii/S009286742100581X", style = "color:black; font-size:20px",target='_blank',h5("5, 
Qin, P., Lu, H., Du, H., Wang, H., Chen, W., Chen, Z., He, Q., Ou, S., Zhang, H., Li, X., et al. (2021). 
Pan-genome analysis of 33 genetically diverse rice accessions reveals hidden genomic variations. Cell 184:3542-3558.e16.
  ",style = "font-size: 20px; font-style: normal; font-weight: lighter;")) ),

column(12, offset=3,align="center", h4("Barley",style = "font-size:24px; color:black;")),

column(12, offset=3, align="left", tags$a(href="https://www.nature.com/articles/s41586-020-2947-8",style = "color:black; font-size:20px", target='_blank', h5("6, 
Jayakodi, M., Padmarasu, S., Haberer, G., Bonthala, V.S., Gundlach, H., Monat, C., Lux, T., Kamal, N., Lang, D., Himmelbach, A., et al. (2020). 
The barley pan-genome reveals the hidden legacy of mutation breeding. Nature 588:284-289.
  ",style = "font-size: 20px; font-style: normal; font-weight: lighter;")) ),


####################### 5/24/23
column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),

column(12, offset=3,align="center", h4("Cereal genomic databases" ,style = "font-size:24px; color:black;")),
column(12, offset=3,align="center", h3("")),

column(12, offset=3, align="center", 

  tags$a(href="https://www.maizegdb.org/", style = "color:black; font-size:20px", target='_blank',
 # h5("MaizeGDB",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="147", height="63", src=paste(Stream_folder,"MaizeGDB.JPG",sep='') ) ),

  tags$a(href="https://graingenes.org/GG3/", style = "color:black; font-size:20px", target='_blank',
#  h5("GraingGenes",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="203", height="62", src=paste(Stream_folder,"GrainGenes.png",sep='') ) ),

  tags$a(href="https://www.sorghumbase.org/", style = "color:black; font-size:20px", target='_blank',
 # h5("SorghumBase",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="66", height="67", src=paste(Stream_folder,"sorghumbase.jpg",sep='') ) ),
  
  tags$a(href="https://phytozome-next.jgi.doe.gov/", style = "color:black; font-size:20px", target='_blank',
#  h5("Phytozome",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="240", height="35", src=paste(Stream_folder,"logo-JGI-phytozome.svg",sep='') ) ),

  tags$a(href="https://plants.ensembl.org/index.html", style = "color:black; font-size:20px", target='_blank',
#  h5("EnsemblPlants",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="242", height="52", src=paste(Stream_folder,"ensembl_plants.png",sep='') ) ),

  tags$a(href="http://rice.uga.edu/", style = "color:black; font-size:20px", target='_blank',
 # h5("Rice Genome Annotation Project",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="475", height="50", src=paste(Stream_folder,"RGAP.png",sep='') ) ),

  tags$a(href="https://www.gramene.org/", style = "color:black; font-size:20px", target='_blank',
 # h5("Gramene",style = "font-size: 20px; font-style: normal; font-weight: lighter;"),
  img(width="150", height="85", src=paste(Stream_folder,"gramene_logo.svg",sep='') ) ),

   ),
####################### 5/24/23

#column(12, offset=3,align="center", h3("")),
#column(12, offset=3,align="center", h3("")),


) # fluidRow

) # mainPanel


#) # sidebarLayout


      ) # For tagList
    }, # For ui function

server <- function(input, output, session){

    } #

  ) # 

}