library(shiny)


shinyServer(function(input, output) {

  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    messages<-""
    
    if (is.null(inFile)) return(NULL)
    
    messages<-list()
    
    withProgress(message = 'Validating file', value = 0,{
      
      incProgress(0.1, detail = ("Loading metadata"))
      ds<-getCurrentMERDataSets(type = input$ds_type)
      datimvalidation::loadSecrets("/opt/dhis2/dish.json")      
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
      
      if (inherits(d, "simpleWarning")) {
        output$messages <- renderUI({
          tags$strong(d$message)})
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
              "invalid data element/orgunit associations found! Returning first 100."
            )
          )
        })
          
        return( head(de_check,100) ) 
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
              "invalid data element/disagg associations found! Displaying first 100."
            )
          )
        })
        return(head(ds_ou_check,100)) 
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
              paste( NROW(vt_check)," invalid values found. Displaying first 100.")
            )
          )
        })
        return( head(vt_check,100) ) 
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
              paste( NROW(neg_check)," negatve values found. Displaying first 100.")
            )
          )
        })
        
        return(head(neg_check,100))
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
              paste( NROW(mech_check)," invalid mechanisms found. Displaying first 100.")
            )
          )
        })
        
        return(head(mech_check,100)) 
      } else {
        messages<-append("All mechanisms are valid.",messages)
      }
      
      incProgress(0.1, detail = ("Validating data"))
      vr_rules<-validateData(d,organisationUnit = input$ou,datasets = ds)
      
      
      #If there are any validation rule violations, put them in the output
      if  ( NROW(vr_rules) > 0 )  {
        
        output$messages <-  renderUI({
          tags$strong(
            paste(
              paste( NROW(vr_rules)," validation rule violations found!. Displaying first 100.")
            )
          )
        })

      return(head(vr_rules[,c("name","ou_name","period","mech_code","formula")],100)) } 
      
      else {
        messages<-append( "No validation rule violations found", messages)

        output$messages <-renderUI({
          lapply(messages,function(x) tags$li(x))
        })
        return(data.frame(status="OK"))
      }
    })
    
  })
  
})
