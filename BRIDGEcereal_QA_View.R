# 6/14/23
# BRIDGEcereal_Q&A

BRIDGEcereal_QA_View <- function(QA_folder) {  

page(
    href = "/QA_View",


    ui <-  function(request){

      tagList(
        

        fluidPage(theme = shinytheme("readable")),


        h2("This is BRIDGEcereal Q&A View page",style="text-align:center"),

        nav_links,

useShinyjs(),


sidebarLayout(

sidebarPanel(

uiOutput("User_Q"),

), # sidebarPanel


mainPanel(

fluidRow(

column(12, offset=3,align="center", h3("")),
column(12, offset=3,align="center", h3("")),


column(12,textOutput("User_QA") ),

tags$style(type="text/css", "#User_QA {white-space: pre-wrap;
          color: blue;
          font-size: 28px;
          font-style: arial;
           }"),

#uiOutput("User_Q_Text"),

#column(12,htmlOutput("User_QA") ),
#htmlOutput("modelParameters"),

) # fluidRow

) # mainPanel


) # sidebarLayout


      ) # For tagList
    }, # For ui function

server <- function(input, output, session){


#num_q <- length(list.files(QA_folder))
num_q <- list.files(QA_folder)

output$User_Q <- renderUI({
    
column(12,

pickerInput(
  inputId = "User_Q_", 
  label = "All questions", 
  choices = c('', num_q),
  selected = c(''), ## by default
  options = list(
    'actions-box' = TRUE, 
    size = 15,
    'selected-text-format' = "count > 1"
  ), 
  multiple = FALSE,
)

)

  })

#observeEvent( req(input$User_Q_) , {
#Q_infor <- readLines( paste(QA_folder, input$User_Q_, sep='') )
#output$User_QA <- renderText({ Q_infor })
#})

observeEvent( req(input$User_Q_) , {

Q_infor_ <- readLines( paste(QA_folder, input$User_Q_, sep='') )

output$User_QA <- renderText({ Q_infor_ })

#output$User_QA <- renderUI(

#        HTML(
#          paste(
#            c("<pre>", capture.output( Q_infor_  ), "</pre>"),
#            collapse = "<br>"
#          )
#        )
#      )

})


    } #

  ) # 

}