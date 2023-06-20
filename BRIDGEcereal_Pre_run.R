# 03/31/23
# Pre_run()

Pre_run <- function(default_choice,gff_folder,gff_folder_Species,User_folder,Backup_folder, html_Speciesx, User_ip) {

# hide button Save for now
#observe({ toggle(id="Save", condition=!is.null(input$Download))})
observe({ toggle(id="Save", condition=!is.null(input$done))}) #03/07/23
#observe({ toggle(id="PNG", condition=!is.null(input$submit_trim))})


## To track login times
num_visit0 <- as.numeric(read.table(paste(script_folder, "Visit_Times.txt",sep=''),header=F));
num_visit1 <- paste("There have been",num_visit0,"visitors!",sep=" ");
output$visits <- renderText({ num_visit1 });
num_visitNew<- num_visit0+1;
write.table(num_visitNew,paste(script_folder, "Visit_Times.txt",sep=''),row.names=FALSE,col.names=FALSE,quote = FALSE);
## To track login times

## To count submitted jobs (genes)
#num_files0 <- length(list.files(Backup_folder)); # number of jobs submitted ! USE backup_folder

#num_files0 <- length(list.files(paste(database_folder,Speciesx,'/',default_ref, sep=''))); # number of jobs submitted ! USE backup_folder
num_files0 <- length(list.files(paste(Backup_folder, 'Candidate_genes', sep = '')) ); # number of jobs submitted ! USE backup_folder
num_files1<-paste("There're",num_files0,"genes or jobs have been submitted on this page! You can try yours.",sep=' ');
output$infogene <- renderText({ num_files1 });
##

####
observeEvent(input$Check_ID, {



if( (gsub(' ','',input$Gene) != "") & ( length(grep(' ', gsub(' ','',input$Gene)))==0 )  ){
 
 key_table <- paste(gff_folder,Speciesx,'/','species_genekey.txt',sep='');
 Gene_Key <- read.table(key_table,header=F)


for(key in Gene_Key$V1){


 if( length(grep(key, gsub(' ','',input$Gene)))==0 ) {

        Test_GeneName2 <- paste("No coordinate information for this query ID! Please double-check your input.",sep='');
        output$coordinates_test <- renderText({ Test_GeneName2 });
        shinyjs::disable(id = "submit")   #6/13/23
    
    }


}


 for(key in Gene_Key$V1){

 if(length(grep(key, gsub(' ','',input$Gene)))==1){

 matched_g <- Gene_Key[which(Gene_Key$V1==key),2]
         updatePickerInput(session = session, inputId = "Pickgenome", selected = matched_g)

AllGenomes_key_info <- paste(gff_folder,Speciesx,'/','All_gene_Working_infor.gff3',sep='');

#ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )
#ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip
#ip_address<-unlist(strsplit(ip_address,', '))[1]
#ip_address <- gsub( '\\.', '_', ip_address )

ip_address <- User_ip

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
          updateSliderInput(inputId="Distancefilter", value = ( as.numeric(matched_gene1[3])-as.numeric(matched_gene1[2]) ) ) ## 03/03/23
           } 


        default_up_down_stream<-round(((as.numeric(matched_gene1[3])-as.numeric(matched_gene1[2]))*0.1)/1000,2) # 2/9/23

        updateTextInput(inputId='Upstream' ,value=as.numeric(default_up_down_stream) ) # 2/9/23
        updateTextInput(inputId='Downstream' ,value=as.numeric(default_up_down_stream) ) # 2/9/23
        
        matched_gene1_strand<-matched_gene1[4]

        matched_gene1<-paste(matched_gene1[1],':',prettyNum(c(matched_gene1[2]),big.mark=",",scientific=FALSE),'-',prettyNum(c(matched_gene1[3]),big.mark=",",scientific=FALSE), ' ', paste(' (',matched_gene1[4],')',' strand.',sep='') ,sep='') #6/13/23
        
        Test_GeneName2 <- paste(c("The query gene is located at: ", matched_gene1), collapse= " ")
        output$coordinates_test <- renderText({ Test_GeneName2 });


       output$html <- renderUI({

         tags$a(href=paste(html_Speciesx,gsub(' ','',input$Gene),sep=''), target='_blank',paste('Click to view more information about gene',gsub(' ','',input$Gene),sep=' '), style = "font-size:16px; color:blue; font-style:italic;");

       })



        if(matched_gene1_strand =='-'){
        output$strand_notice<- renderText({ paste("In BRIDGEcereal, if you need promotor region of this gene (as gene in negative strand in this case), please enter preferred size (kb) in the Downstream box",sep='') });
        }


        }

system_delete<-paste('rm',matched_gene_file,sep=' ')
system(system_delete)

shinyjs::enable(id = "submit")   #6/12/23 for potential naming error!!!

} else if(file.size(matched_gene_file)==0){

#system_delete<-paste('rm',matched_gene_file,sep=' ')
system_delete<-paste('rm -r',Users_folder,sep=' ')
system(system_delete)

####6/12/23 for potential naming error!!!
Test_GeneName2 <- paste("No coordinate information for this query ID! Please double-check your input.",sep=''); #6/12/23 for potential naming error!!!

output$coordinates_test <- renderText({ Test_GeneName2 }); #6/12/23 for potential naming error!!!

shinyjs::disable(id = "submit")   #6/12/23 for potential naming error!!!

output$html <- renderUI({ #6/12/23 for potential naming error!!!

        tags$a(href='', target='_blank',paste('',sep=''), style = "font-size:22px; color:red; font-style:italic;"); #6/12/23 for potential naming error!!!

       }) #6/12/23 for potential naming error!!!
####6/12/23 for potential naming error!!!

    }

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
      
    # shinyjs::disable(id = "Upstream")  # 4/4/23
    # shinyjs::disable(id = "Downstream") # 4/4/23


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
observeEvent( input$upload1, {
         
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
#ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )
#ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip
#ip_address<-unlist(strsplit(ip_address,', '))[1]
#ip_address <- gsub( '\\.', '_', ip_address )

ip_address <- User_ip
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
          
          upload_name <- input$upload1$name
          Parent<-"Parent1"
          source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
          For_upload_file(Users_folder, upload_name, Parent)


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
#ip_address <- gsub( '\\.', '_', fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip )
#ip_address<-fromJSON(readLines("https://jsonip.com/?callback=",warn=FALSE))$ip
#ip_address<-unlist(strsplit(ip_address,', '))[1]
#ip_address <- gsub( '\\.', '_', ip_address )
ip_address <- User_ip
User_Gene<-gsub(' ','',input$Gene)
User_folder0<- paste(ip_address,'_',User_Gene,sep='')
Users_folder<-paste(User_folder, User_folder0 , sep='')
#############
#############
       
         file.copy(input$upload2$datapath, paste0(Users_folder,'/',input$upload2$name) )
          
          upload_name <- input$upload2$name
          Parent<-"Parent2"
          source(paste(script_folder,"BRIDGEcereal_Sub.R",sep=''), local = TRUE);
          For_upload_file(Users_folder, upload_name, Parent)        


    updatePickerInput(session = session, inputId = "id",
                        choices = c(default_choice,'Parent1','Parent2'),
                        selected = c(default_choice,'Parent1','Parent2'))


    Test_upload2 <- paste("Completion of Parent2 upload. You can proceed ...",sep='')
    output$upload2_test <- renderText({ Test_upload2 })

    shinyjs::disable(id = "submit")

    output$Largefile <- renderUI({

    actionButton("Largefile", label = "(2) Submit (large file)",style="color: FF66B2; background-color: #FFFF99; border-color: #c34113; border-radius: 10px; border-width: 2px")

  })    

        })


}   # function

#################################################################
#################################################################