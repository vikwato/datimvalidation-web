library(shiny)


shinyServer(function(input, output) {
  shinyjs::disable("validate")
  shinyjs::hide("downloadData")
  ready <- reactiveValues(ok = FALSE)
  
  observeEvent(input$file1, {
    shinyjs::enable("validate")
    ready$ok <- FALSE
  }) 
  
  observeEvent(input$validate, {
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })  
  
  disableUI<-function(){
    shinyjs::disable("type")
    shinyjs::disable("ou")
    shinyjs::disable("ou_scheme")
    shinyjs::disable("de_scheme")
    shinyjs::disable("id_scheme")
    shinyjs::disable("ds_type")
    shinyjs::disable("file1")
    shinyjs::disable("header")
    shinyjs::disable("validate")
    shinyjs::disable("downloadData")
  }
  
  enableUI<-function(){
    shinyjs::enable("type")
    shinyjs::enable("ou")
    shinyjs::enable("ou_scheme")
    shinyjs::enable("de_scheme")
    shinyjs::enable("id_scheme")
    shinyjs::enable("ds_type")
    shinyjs::enable("file1")
    shinyjs::enable("header")
    #shinyjs::enable("validate")
    shinyjs::enable("downloadData")
  }
  
  
  validate<-function() {
    
    if (!ready$ok) {return(NULL)}
    #Lock the UI
    disableUI()
    inFile <- input$file1
    messages<-""
    
    if (is.null(inFile)) return(NULL)
    
    messages<-list()
    
    withProgress(message = 'Validating file', value = 0,{
      
      incProgress(0.1, detail = ("Loading metadata"))
      ds<-getCurrentMERDataSets(type = input$ds_type)
      datimvalidation::loadSecrets("dish.json")      
      incProgress(0.1, detail = ("Parsing data"))
      d <- tryCatch(
        datimvalidation::d2Parser(
          filename = inFile$datapath,
          organisationUnit = input$ou,
          type = input$type,
          dataElementIdScheme = input$de_scheme,
          orgUnitIdScheme = input$ou_scheme,
          idScheme = input$id_scheme,
          csv_header = input$header
        ),
        warning = function(w) {
          w
        }
      )
      #Reset the button to force upload again
      shinyjs::reset("file1")
      
      if (inherits(d, "simpleWarning")) {
        output$messages <- renderUI({
          tags$strong(d$message)})
        enableUI()
        return(NULL) 
      } else {
        messages<-append("No problems found during file parsing.",messages)
      }
      incProgress(0.1, detail = ("Checking data element/orgunit associations"))
      de_check <-
        checkDataElementOrgunitValidity(
          data = d,
          datasets = ds,
          organisationUnit = input$ou,
          return_violations = TRUE
        ) 
      
      if (inherits(de_check, "data.frame")) {
        
        output$messages <-  renderUI({
          tags$strong(
            paste(
              NROW(de_check),
              "invalid data element/orgunit associations found!"
            )
          )
        })
        enableUI()
        shinyjs::show("downloadData")
        return( de_check ) 
      } else {
        messages<-append("Data element/orgunit associations are valid.", messages)
      }
      
      #Data element disagg check
      incProgress(0.1, detail = ("Checking data element/disagg associations"))
      ds_ou_check <-
        checkDataElementDisaggValidity(d, datasets = ds, return_violations = TRUE)
      
      if (inherits(ds_ou_check, "data.frame")) {
        output$messages <-  renderUI({
          tags$strong(
            paste(
              NROW(ds_ou_check),
              "invalid data element/disagg associations found!"
            )
          )
        })
        enableUI()
        shinyjs::show("downloadData")
        return( ds_ou_check ) 
      } else {
        messages<-append("Data element/disagg associations are valid.",messages)
      }
      
      #Value type compliance check
      incProgress(0.1, detail = ("Checking value type compliance."))
      
      vt_check <- checkValueTypeCompliance(d)
      
      if (inherits(vt_check, "data.frame") & NROW(vt_check) > 0) {
        output$messages <-  renderUI({
          tags$strong(
            paste(
              paste( NROW(vt_check)," invalid values found.")
            )
          )
        })
        enableUI()
        shinyjs::show("downloadData")
        return( vt_check ) 
      } else {
        messages<-append("Value types are valid.",messages)
      }
      
      #Negative value check
      incProgress(0.1, detail = ("Checking negative numbers."))
      
      neg_check <- checkNegativeValues(d)
      
      if (inherits(neg_check, "data.frame")) {
        output$messages <-  renderUI({
          tags$strong(
            paste(
              paste( NROW(neg_check)," negatve values found.")
            )
          )
        })
        enableUI()
        shinyjs::show("downloadData")
        return( neg_check )
      } else {
        messages<-append("No negative values found.",messages)
      }
      
      #Mechanism check
      incProgress(0.1, detail = ("Checking mechanisms."))
      
      mech_check <-
        checkMechanismValidity(data = d, organisationUnit = input$ou,return_violations=TRUE)
      
      if (inherits(mech_check, "data.frame")) {
        
        output$messages <-  renderUI({
          tags$strong(
            paste(
              paste( NROW(mech_check)," invalid mechanisms found.")
            )
          )
        })
        enableUI()
        shinyjs::show("downloadData")
        return( mech_check ) 
      } else {
        messages<-append("All mechanisms are valid.",messages)
      }
      
      incProgress(0.1, detail = ("Validating data"))
      
      if ( Sys.info()['sysname'] == "Linux") {
        
       ncores <- parallel::detectCores() - 1
       doMC::registerDoMC(cores=ncores)
       is_parallel<-TRUE
       
      } else {
        is_parallel<-FALSE
      }
      vr_rules<-validateData(d,organisationUnit = input$ou,
                             datasets = ds,
                             parallel = is_parallel)
      
      #If there are any validation rule violations, put them in the output
      if  ( NROW(vr_rules) > 0 )  {
        
        output$messages <-  renderUI({
          tags$strong(
            paste(
              paste( NROW(vr_rules)," validation rule violations found!")
            )
          )
        })
        enableUI()
        shinyjs::show("downloadData")
        return( vr_rules[,c("name","ou_name","period","mech_code","formula")] ) } 
      
      else {
        messages<-append( "No validation rule violations found", messages)
        
        output$messages <-renderUI({
          lapply(messages,function(x) tags$li(x))
        })
        enableUI()
        return(data.frame(status="OK"))
      }
    })
    
    
  }
  
  validation_results <- reactive({ validate() })
  
  output$contents <- renderDataTable({ validation_results() })
  
  output$downloadData <- downloadHandler(
    filename = "validation_results.csv",
    content = function(file) {
      write.csv( validation_results(), file, row.names = FALSE)
    }
  )
})
