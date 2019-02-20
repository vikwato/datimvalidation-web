library(shiny)
source("./utils.R")
fluidPage(sidebarLayout(
  sidebarPanel(
    shinyjs::useShinyjs(),
    selectInput("type", "Type:",
                c(
                  "CSV" = "csv",
                  "JSON" = "json",
                  "XML" = "xml"
                )),
    selectInput("ou", "Operating Unit", ous),
    selectInput(
      "de_scheme",
      "Data element ID scheme:",
      c(
        "ID" = "id",
        "Code" = "code",
        "Name" = "name"
      ),
      selected = "id"
    ),
    selectInput(
      "ou_scheme",
      "Orgunit ID scheme:",
      c(
        "ID" = "id",
        "Code" = "code",
        "Name" = "name"
      ),
      selected = "id"
    ),
    selectInput(
      "id_scheme",
      "ID scheme:",
      c(
        "ID" = "id",
        "Code" = "code",
        "Name" = "name"
      ),
      selected = "id"
    ),
    selectInput(
      "ds_type",
      "Dataset type:",
      c("Results" = "RESULTS", "Targets" = "TARGETS")
    ),
    checkboxInput("header", "CSV Header", TRUE),
    fileInput(
      "file1",
      "Choose data file:",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        "application/json",
        "application/xml",
        ".csv",
        ".json",
        ".xml"
      )
    ),
    tags$hr(),
    actionButton("validate","Validate"),
    downloadButton("downloadData", "Download validation results")
  ),
  mainPanel(tabsetPanel(
    type = "tabs",
    tabPanel("Output", dataTableOutput("contents")),
    tabPanel("Messages",   tags$ul(uiOutput('messages')))
  ))
))
