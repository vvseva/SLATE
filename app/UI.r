
rm(list = ls())

# DATA
library(readr)
#UiB_output <- read_csv("fake.csv")
UiB_output <- read_csv("fake2.csv")
key_skills <- read_csv("key_skills.csv")
UiB_output = UiB_output[,-1]


#packages
library(shiny)
library(shinysky)
library(DT)
library(dplyr)
library(shinydashboard)
library(flexdashboard)
library(elasticsearchr)
library(stringr)
library(shinythemes)
library(visNetwork)
library(jsonlite)
library("elastic")

# 
# library(shinylogs)


### User data?
# 
# modUI <- function(id) {}
# mod <- function(input, output, session) {
#   parentSession <- .subset2(session, "parent")
#   parentSession$userData$flag <- TRUE
# }




## UI starts here

ui <-  fluidPage(
    
    ## small magic with fonts and squares
    theme = shinytheme("united"),
    
    # UI
    # use_tracking(),
    
    ## browser tab icon
    fluidPage(
        list(tags$head(HTML('<link rel="icon", href="UiB-emblem500px_white.png", 
                                   type="image/png" />'))),
        
        div(style="padding: 0px 0px; width: '100%'",
            titlePanel(
                title="", windowTitle="UiB course RecSys"
            )
        ))
    ,
    ### TOns of style
        tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
              
        #search input
        tags$style(type="text/css","#skl { top: 50% !important; left: 50% !important;
        margin-top: 100px !important; margin-left: -250px
                       !important; color: #cf3c3a; font-size: 20px;
                   font-style: italic; padding:200px; background-color:#cf3c3a;}"),
    
        #search department
        tags$style(type="text/css","#dep { top: 50% !important; left: 50% !important;
        margin-top: 100px !important; margin-left: -250px
                       !important; color: #cf3c3a; font-size: 20px; 
                   font-style: italic; padding:50px; background-color:#cf3c3a;}"),
    
        # ceter page a little if needed
        tags$style(type="text/css", "body {padding-left: 0px;
                           padding-right: 0px;}"),
                
        ### I am not 100% sure
        tags$head(tags$style("#text1{color: #cf3c3a;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"),
                  # botton
                  tags$style(HTML("
                                .btn {
                                color:white; text-align: left;
                                border-color:#cf3c3a; background-color:#cf3c3a;}
                                
                                .btn:hover{
                                border-color:#cf3c3a;background-color:white; 
                                color:#cf3c3a; font-weight:bold;
                                }")),
                  #slider
                  tags$style(HTML("
                    .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                    .js-irs-0 .irs-bar, .js-irs-0 .irs-from, .js-irs-0 .irs-to
                    {background: #cf3c3a;
                                    border-top: 1px solid #cf3c3a ;
                                    border-bottom: 1px solid #cf3c3a ;}")),
                  ),
            
        ## tabs. tabs for sure    
        tags$style(HTML(" 
        .navbar { background-color:  #db3f3d;}
        .navbar {min-height:50px !important;}'
        .navbar .navbar-header {float: left}
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white; background-color: #cf3c3a;}
        .navbar-default .navbar-nav > li > a:hover {color: white;
        text-decoration:underline; background-color: #db3f3d;}
                  ")),
  ################################################################
  #####################################################################
  #############################################################
  #####################################################################
  ################################################################
    
    ## hat of the page
    navbarPage(
      title = div(
                  img(src='UiB-emblem500px_white.png', style = '
                  height: 55px;
                  margin: -15px;
                      ' 
                      # height = "42px", width = "227px", padding = "-10px"
                      ),
                  
                  ),
                         collapsible = TRUE,
                       # #   inverse = TRUE,
                         position = "static-top",
                       # #   #footer = includeHTML("./www/include_footer.html"),
                         header = tags$style(
                             ".navbar-right {
                       float: right !important;
                       padding: 0px;
                       height: 70px;
                       }",
                             "body {padding-bottom: 10px;}"),
      

    ## first page: Elastic search
    
        tabPanel("Elastic course search",  
            fluidRow(
                column(width = 6,        
                          # h3("selected skill:"),
                          h2(textOutput("broken")),
                          # DT::dataTableOutput("broken_table"),
                          h2(textOutput("urlText")),
                           
                           # select2Input("cts",
                           #              "",
                           #              choices=c(unique(UiB_output$code))
                           #              ),
                           #h2(textOutput("text")),
                           
                            )
                       ),

            fluidRow(
                  column(3, sliderInput("sliderETCS", label = h3("ETCS Range"), min = 0, 
                              max = 60, value = c(10, 15))
                         #,style = "background-color:#db3f3d;"
                  ),
                  column(2, radioButtons("radio_semester", h3("Semester"),
                                         choices = list("Autumn" = "Autumn", "Spring" = "Spring",
                                                        "Spring, Autumn" = "Spring, Autumn"),
                                         selected = "Spring, Autumn")
                         #,style = "background-color:#db3f3d;"
                         ),
                  column(2,
                         br(),
                         br(),
                         br(),
                         select2Input("dep", "Department", 
                                      choices= UiB_output$department %>% na.omit() %>% unique()
                                      ,type = c("input")))
                  ),

              
            fluidRow(
                column(8,
                 h3("Selected courses:"),
                 DT::dataTableOutput("courses")
                ),
                # column(2, offset = 0, 
                #        br(),
                #        br(),
                #        br(),
                #        br(),
                #        br(),
                #        br(),
                #        br(),
                #        # br(),
                #        # actionButton("Bottone",label="Add"),
                #        # actionButton("Bottone2",label="Clear")
                #        ),
                column(2,
                       br(),
                       br(),
                       br(),
                       br(),
                       flexdashboard::gaugeOutput("plt1")
                       #,style = "background-color:#db3f3d;"
                ),
              ),
              
            fluidRow(
                  column(width = 8,
                         h3("Search for a skill, or for anything else:"),
                         select2Input("skl","",choices=c(unique(key_skills$keyword))
                                      ,type = c("input", "select"))
                  ),
                  
                  column(3,
                         br(),
                         br(),
                         actionButton("Bottone",label="Add"),
                         actionButton("Bottone2",label="Clear"),
                          actionButton("Bottone3",label="ADD LOGS"))
                  ),
            br(),
            DT::dataTableOutput("syllabuses")
            
      
            
            # fluidRow(8,
            #          h3("text at the bottom"))
              
            ),
    
    ############
    ##########word 2 vec
    ###########
    
        tabPanel("Word2Vec",
                 fluidRow(
                     h3("Text about current Bert")
                 )), 
    
    
    #################
    ##############Networks
    #################
    
        tabPanel("Tag Explorer",
                 fluidRow(
                     h3("new text and some kind of etwork"),
                     
                     visNetworkOutput("network", width = "100%", height = "80vh")
                 )),
    
    
        tabPanel("User input",
                 fluidRow(
                   h3("Text of ")
                 )
                 )
        ),

    tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2020 Copyright:
                           <a href='https://www.slate.uib.no/'> SLATE UiB</a>
                           <br/>
                           <a href=mailto: vvsuschevskiy@gmail.com? subject='mail me'> Vsevolod Suschevskiy </a>
                           </div>
                           <!-- Copyright -->
                           </footer>
                           <!-- Footer -->"))
            
)
