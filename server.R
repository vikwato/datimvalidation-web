#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
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
    
    
    withProgress(message = 'Validating file', value = 0,{
      
      incProgress(0.1, detail = ("Loading metadata"))
      ds<-getCurrentMERDataSets(type = input$ds_type)
      
      incProgress(0.2, detail = ("Parsing data"))
      d <- tryCatch(
        datimvalidation::d2Parser(
          inFile$datapath,
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
        output$messages <- renderPrint({ d$message })
        return(NULL) 
      } else {
        messages<-"No problems found during file parsing."
      }
      incProgress(0.3, detail = ("Checking data element/orgunit associations"))
      de_check <-
        checkDataElementOrgunitValidity(
          data = d,
          datasets = ds,
          organisationUnit = input$ou,
          return_violations = TRUE
        ) 
      
      if (inherits(de_check, "data.frame")) {
        output$messages <- renderPrint({ 
          
          paste( NROW(de_check),"invalid data element/orgunit associations found! Returning first 100.") })
        return( head(de_check,100) ) 
      } else {
        messages<-append("Data element/orgunit associations are valid.",messages)
      }
      
      #Data element disagg check
      incProgress(0.4, detail = ("Checking data element/disagg associations"))
      ds_ou_check <-
        tryCatch(
          checkDataElementDisaggValidity(d,datasets = ds),
          warning = function(w) {
            w
          }
        )
      
      if (inherits(ds_ou_check, "simpleWarning")) {
        output$messages <- renderPrint({ ds_ou_check$message
        })
        return(NULL) 
      } else {
        messages<-append("Data element/disagg associations are valid.",messages)
      }
      
      #Value type compliance check
      incProgress(0.5, detail = ("Checking value type compliance."))
      
      vt_check <-
        tryCatch(
          checkNegativeValues(d),
          warning = function(w) {
            w
          }
        )
      
      if (inherits(vt_check, "simpleWarning")) {
        output$messages <- renderPrint({ vt_check$message
        })
        return(NULL) 
      } else {
        messages<-append("Value types  are valid.",messages)
      }
      
      #Negative value check
      incProgress(0.6, detail = ("Checking negative numbers."))
      
      z_check <-
        tryCatch(
          checkNegativeValues(d),
          warning = function(w) {
            w
          }
        )
      
      if (inherits(z_check, "simpleWarning")) {
        output$messages <- renderPrint({ z_check$message
        })
        return(NULL) 
      } else {
        messages<-append("No negative values found.",messages)
      }
      
      #Mechanism check
      incProgress(0.7, detail = ("Checking mechanisms."))
      
      mech_check <-
        tryCatch(
          checkMechanismValidity(data = d, organisationUnit = input$ou),
          warning = function(w) {
            w
          }
        )
      
      if (inherits(mech_check, "simpleWarning")) {
        output$messages <- renderPrint({ mech_check$message
        })
        return(NULL) 
      } else {
        messages<-append("All mechanisms are valid.",messages)
      }
      
      incProgress(0.9, detail = ("Validating data"))
      vr_rules<-validateData(d,organisationUnit = input$ou,datasets = ds)
      
      
      #If there are any validation rule violations, put them in the output
      if  ( NROW(vr_rules) > 0 )  {
        
        messages<-append("Validation rule violations found!",messages)
        
        output$messages <- renderPrint({print( messages ) })
        
        vr_rules[,c("name","ou_name","period","mech_code","formula")] } 
      
      else {
        return(NULL)
        messages<-append( "No validation rule violations found", messages)
        output$messages <- renderPrint({ messages })
      }
    })
    
  })
  
})
