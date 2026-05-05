library(shiny)
library(shinydashboard)
library(Peptides)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)
library(stringr)
source('R/custom_inputs.R')
source('R/fun_get_params.R')
source('R/fun_fasta2df.R')
source('R/fun_df2fasta.R')
source('R/fun_modals.R')
source('R/fun_splitIDs.R')
source('R/fun_searchQuery.R')
#shinyWidgets::shinyWidgetsGallery()

# Setup the bslib theme object
my_theme <- bs_theme(base_font = "Inter", bg="#EEF7FF", fg="#000", primary="#007AFF")

#Max file upload in bytes
max_size <- 10*1024^2
filesize <- "10 MB"

ui <- shiny::fluidPage(
  title = "FASTA protein parameters",
  theme = my_theme,
  style = 'padding: 0;',
  lang = "en",
  useShinyjs(),
  #return id when delete button is clicked
  #Set inputs to NULL
  tags$script(src="iframeSizer.contentWindow.min.js"),
  tags$head(tags$script(
    HTML(
      "$(document).on('click', '.deletebutton', function () {
                                Shiny.onInputChange('del_clicked',this.id);
                             });"
    ))),
  tags$head(tags$script(
    HTML(
      "$(document).on('click', '.advdeletebutton', function () {
                               Shiny.onInputChange('adv_del_clicked',this.id);
                             });"
      ),
  #Bind Enter key to search input  
  keyBinding("search_filter", "search", "Enter"),
  )),
  verticalLayout(
    div(class="alert-box alert alert-success fixed-top", role="alert", style="display: none;",
        "Sequence copied to clipboard"),
    fluidRow(id="input-container",
             column(width=12, style="border-radius: 20px;",
    fileInput("upload", 
              "Upload FASTA file",
              multiple = FALSE,
              accept = c(".fasta", ".FASTA", ".txt"),
              width = "100%"
              ),
    shinyjs::hidden(
    div(id="param-container", 
        checkboxGroupInput("params", HTML("<b>Parameter</b>"), 
                           choices = c("Absorbance","Extinction", "Length", "Mw", "pI", "Charge", "Hydrophobicity")),
        actionButton("calculate", "Calculate", class="btn-primary", style="color:#fff"),
      )),
             )
  ),
  shinyjs::hidden(
  fluidRow(id="table-container",
           column(width=12, style="padding: 15px; border-radius: 20px;",
                    div(id="downloadbtn-container", 
                        style="display: flex; 
                        justify-content: flex-end;
                        column-gap: 10px;
                        position: relative;
                        margin-bottom: -30px;
                        z-index: 999;",
                  downloadButton("download_csv", "Download .csv", 
                                 class="btn-primary", 
                                 style="color:#fff", 
                                 icon = shiny::icon("download"))
                  ),
                        DT::dataTableOutput("fastaTable", width = "100%")
                  )))
    
)
)

server <- function(input, output, session) {
  
    #Reactive dataframe to store values    
    r <- reactiveValues()
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
          r$fasta <- fasta2df(file_path) %>% 
            mutate(
              sub_seq =
                sprintf(
                '<div 
                  style=\"display: flex; justify-content: space-between;\"
                  <p>%s...</p>
                  %s
                </div>', 
                substr(seq, 1, 20),
                clipboard_button("clipboard", seq)
                )
              )
          
          shinyjs::show("param-container")
          r$fasta_calculated <- r$fasta
        } else if (!file_ext %in% c('FASTA', 'fasta', 'txt')){
          errorModal(message = sprintf("Invalid file format: %s", file_name))
        } else if (file_size >= max_size){
          errorModal(message = sprintf("Error uploading file. Max filesize: %s", filesize))
        } else if((substr(readLines(file_path, n=1),1,1) != ">")){
          errorModal(message = "Expected > at beginning of line")
        }
    })
    
    observe({
      if(length(input$params) != 0){
        shinyjs::enable("calculate")
      } else{
        shinyjs::disable("calculate")
      }
    })
  
    observeEvent(input$calculate, {
      r$fasta_calculated <- r$fasta
      r$fasta_calculated <- param_to_DF(r$fasta, input$params)
      shinyjs::show("table-container")
    })
    
  #Table output
  output$fastaTable <- DT::renderDataTable({
    req(r$fasta_calculated)
    datatable(
      r$fasta_calculated %>% rename("Sequence" = "sub_seq", "Name" = "names") %>% select(-seq, -width),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        #dom = 't',
        searching = FALSE,
        autoWidth = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#FFF', 'background': '#007AFF'});",
          "}"
        )
      ))
  })
  
  # Download filtered CSV
  output$download_csv <- downloadData <- downloadHandler(
    filename = function() {
      paste(input$upload[["name"]], "_filtered.csv", sep="")
    },
    content = function(file) {
      write.table(r$fasta_calculated[, append(c("names", "seq"), input$params)], file, sep=",", row.names = FALSE)
    }
  )
  # Download filtered FASTA
  output$download_fasta <- downloadData <- downloadHandler(
    filename = function() {
      paste(input$upload[["name"]], "_filtered.fasta", sep="")
    },
    content = function(file) {
      df2fasta(r$fasta_calculated[, c("names", "seq")], file)
    }
  )
}
# Run the application 
options(shiny.maxRequestSize = max_size,shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
