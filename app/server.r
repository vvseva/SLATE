
server <- function(input, output, session) {
  
  #track_usage(storage_mode = store_json(path = "/logs"))
  
  
  observeEvent(input$Bottone3,{
    
    session_test = data.frame(token = session$token, time = as.character(Sys.time()))
    

    
    connect = connect(host="REMOVED DUE TO PRIVACY",
                      user="elastic",
                      pwd="REMOVED DUE TO PRIVACY",
                      port="REMOVED DUE TO PRIVACY",
                      transport_schema = "REMOVED DUE TO PRIVACY")
    
    docs_bulk(connect, session_test, index="session_test")
    
  })
  
  
# sr2 =query( str_glue('{{
#         "match" : {{
#             "text" : {{
#                 "query" : "{query}"
#             }}
#         }}
#     }}', query = str_c(session_test, collapse = " ")))

# dfn=elastic("REMOVED DUE TO PRIVACY",
#            "testslb1") %search% (sr2)


    consider = data.frame(code = NA, credit = NA)
    # output$courses <- renderText({
    #     input$syllabuses_rows_selected
    #     })
    

    output$broken <- renderText({
      # as.character(consider$code)
      #session
      
      session$token
    })
    
    
    output$urlText <- renderText({
      paste(sep = "",
            "protocol: ", session$clientData$url_protocol, "\n",
            "hostname: ", session$clientData$url_hostname, "\n",
            "pathname: ", session$clientData$url_pathname, "\n",
            "port: ",     session$clientData$url_port,     "\n",
            "search: ",   session$clientData$url_search,   "\n"
      )
    })
    
    # output$broken_table = DT::renderDataTable(
    #     consider_RCT()
    # )
    
    
    semester_RTC <-reactive({
        if(input$radio_semester == "Spring, Autumn" ){
            c("Spring, Autumn", "Spring", "Autumn")
        } else {
            input$radio_semester
        }

        
    })
    
    syllabuses_RCT <- reactive({
       # a = consider$code
      
        if (is.null(input$skl) == TRUE){
            UiB_output %>% 
                filter(credit >= input$sliderETCS[1] & credit <= input$sliderETCS[2]) %>% 
                filter(semester %in% semester_RTC()) %>% 
                filter(department %in% department_RTC()) %>% 
                filter(!code %in% consider) %>% 
                na.omit()
        }
        
        else{
            sr =query( str_glue('{{
        "match" : {{
            "text" : {{
                "query" : "{query}"
            }}
        }}
    }}', query = str_c(input$skl, collapse = " ")))
            
df=elastic("REMOVED DUE TO PRIVACY",
                         "testslb1") %search% (sr)
            search_codes = head(df$code, 20)
            
            df = UiB_output %>% 
                filter(code %in% search_codes) %>% 
                filter(credit >= input$sliderETCS[1] & credit <= input$sliderETCS[2]) %>% 
                filter(semester %in% semester_RTC()) %>% 
                filter(department %in% department_RTC()) %>% 
                filter(!code %in% consider) %>% 
                na.omit()
            
            df[match(search_codes, df$code),] %>% 
                na.omit()
        }
    })
    
    output$courses = DT::renderDataTable(
        
        consider_RCT()
         ,extensions = 'Buttons'
         ,options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'excel', 'pdf', 'print')
                        )
    )

    
    observeEvent(input$Bottone2, {
        consider <<- data.frame(code = NA, credit = NA)
    })
    
    
    
    consider_RCT <- reactive({
            a = input$Bottone
            b = input$Bottone2
            
            # variabile <- isolate(syllabuses_RCT()[input$syllabuses_rows_selected, 2:3, drop = FALSE])
            # variabile
            
            # variable1 = isolate(input$syllabuses_rows_selected)
            #variable1
            
            variable2 = isolate(syllabuses_RCT()[input$syllabuses_rows_selected, 2:3, drop = FALSE])
            # variable2
            consider <<- rbind(consider, variable2)
            consider %>% unique() %>% na.omit()
            
    })

    credits_RCT <- reactive({
        if (sum( consider_RCT()[,2] > 0 )) {
            sum( consider_RCT()[,2] )
        }
        else (0)
    })
    
    department_RTC <- reactive({
        if(length(input$dep) > 0 ){
            input$dep
        } else{
            c(unique(UiB_output$department))
        }
        
    })
    
    output$plt1 <- flexdashboard::renderGauge({
        gauge(credits_RCT(), min = 0, max = 50, symbol = ' ECTS', label = paste("Test Label"),
              gaugeSectors(
            success = c(30, 45), warning = c(0,29), danger = c(41, 50), colors = c("green", "red", "orange")
        ))
    })
    
    
    #  syllabuses_RCT_end <- reactive({
    #    a = input$Bottone
    #    b = input$Bottone2
    #    
    #    isolate(consider_RCT()$code)
    # })
    
    output$syllabuses = DT::renderDataTable(
        DT::datatable(syllabuses_RCT(), escape=F
                      , options=list(columnDefs = list(list(visible=FALSE, targets=c(2, 7)))
                                     )
                      )
    )
    
    ### SECOND TAB
    ### NETWORKS
    
    output$network <- renderVisNetwork({
        # minimal example
        nodes <- data.frame(id = 1:nrow(head(key_skills, 20)), label = head(key_skills, 20)$keyword, value = rnorm(nrow(head(key_skills, 20)), 20, sd = 5))
        edges <- data.frame(from = c(sample(c(1:nrow(head(key_skills, 20))), 30, replace = T)),
                            to = c(sample(c(1:nrow(head(key_skills, 20))), 30, replace = T))
        )
        
        visNetwork(nodes, edges) %>% 
            visNodes(color = "#db3f3d") %>% 
            visInteraction(hover = TRUE) %>%
            visEvents(hoverNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}")
    })
    
}
