#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(DT)
source('R/custom_inputs.R')
source('R/fun_fasta2df.R')
source('R/fun_df2fasta.R')
source('R/fun_modals.R')
#shinyWidgets::shinyWidgetsGallery()

# Setup the bslib theme object
my_theme <- bs_theme(base_font = bslib::font_google("Inter"), bg="#EEF7FF", fg="#000", primary="#007AFF")

#Max file upload in bytes
max_size <- 10*1024^2
filesize <- "10 MB"

ui <- shiny::fluidPage(
  title = "Filter FASTA files",
  theme = my_theme,
  style = 'padding: 0;',
  lang = "en",
  includeCSS('www/fileDownload.css'),
  useShinyjs(),
  #return id when delete button is clicked
  tags$head(tags$script(
    HTML(
      "$(document).on('click', '.deletebutton', function () {
                                Shiny.onInputChange('del_clicked',this.id);
                             });"
    )
  )),
  sidebarLayout(
    sidebarPanel(
    fileInput("upload", 
              "Upload FASTA file",
              multiple = FALSE,
              accept = c(".fasta", ".FASTA", ".txt")
              ),
    hr(),
    textInputIcon("search_filter1", label=NULL, icon=icon("search")),
    div(style="display: flex; margin-top: -15px; padding-bottom: 15px;",
    helpText(actionLink("optAdvanced", "Advanced"), "|",
             actionLink("optList", "List"), "|",
             actionLink("optRange", "Range"), "|"
             )
    ),
    #numericRangeInput("seqWidth", "Width", c(NA, NA)),
    uiOutput("uifilters"),
    DT::dataTableOutput("uploadedfiles", width = "100%"),
    actionButton("filter", "Search", class="btn-primary", style="color:#fff")
  ),
  mainPanel(
    DT::dataTableOutput("fastaTable", width = "100%"),
  )
)
)
server <- function(input, output, session) {
  
    #Reactive dataframe to store values    
    r <- reactiveValues()
    #Reactive counter for del button IDs
    r$count <- 0
    #Reactive counter for total filesize
    r$filesize <- 0
    
    #Store data in reactive DF
    observeEvent(input$upload, priority = 20, {
      file_path <- input$upload[["datapath"]]
      file_ext <- tolower(tools::file_ext(file_path))
      file_name <- input$upload[["name"]]
      file_size <- input$upload[["size"]]
      
        if (file_ext %in% c('FASTA', 'fasta', 'txt') 
            && r$filesize <= file_size
            && (substr(readLines(file_path, n=1),1,1) == ">")) {
          #Attach delete button to upload data
          #Store upload data in DF
          r$fasta <- fasta2df(file_path)
          shinyjs::enable("filter")
        } else if (!file_ext %in% c('FASTA', 'fasta', 'txt')){
          errorModal(message = sprintf("Invalid file format: %s", file_names[i]))
        } else if (file_size >= max_size){
          errorModal(message = sprintf("Error uploading file. Max filesize: %s", filesize))
        } else if((substr(readLines(file_path, n=1),1,1) != ">")){
          errorModal(message = "Expected > at beginning of line")
        }
      
    })
  
  shinyjs::disable("Search")
  
  output$fastaTable <- DT::renderDataTable({
    req(input$filter)
    req(r$fasta)
    datatable(
      r$fasta,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        #dom = 't',
        autoWidth = TRUE,
      ))
  })
  # #Merge button
  # shinyjs::disable("download")
  # output$download <- downloadData <- downloadHandler(
  #   filename = function() {
  #     paste(input$upload[["name"]], ".csv", sep="")
  #   },
  #   content = function(file) {
  #     df <- fastas2df(input$upload[["datapath"]])
  #     write.table(df, file, sep=input$sep, col.names = input$header)
  #   }
  # )
}
# Run the application 
options(shiny.maxRequestSize = max_size)
shinyApp(ui = ui, server = server)
