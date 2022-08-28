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
library(stringr)
source('R/custom_inputs.R')
source('R/fun_fasta2df.R')
source('R/fun_df2fasta.R')
source('R/fun_modals.R')
source('R/fun_splitIDs.R')
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
    ),
  #Bind Enter key to search input  
  keyBinding("search_filter", "search", "Enter"),
  )),
  verticalLayout(
    fluidRow(id="input-container", style="background-color: #FFF; padding: 15px;",
             column(width=12, style="padding: 15px; border-radius: 20px;",
    fileInput("upload", 
              "Upload FASTA file",
              multiple = FALSE,
              accept = c(".fasta", ".FASTA", ".txt"),
              width = "100%"
              ),
    shinyjs::hidden(
    div(id="search-container", style="display: flex; align-items: flex-start; margin-bottom: -25px; margin-top: 50px",
        div(
    textInputIcon("search_filter", label=NULL, width="100%", icon=icon("magnifying-glass")),
    div(style="margin-top: -20px;",
    helpText(actionLink("optAdvanced", "Advanced"), "|",
             actionLink("optList", "List"), "|",
             actionLink("optRange", "Range")))),
    actionButton("search", "Search", class="btn-primary", style="color:#fff"),
    )
  )
             )
  ),
  fluidRow(id="table-container", style="background-color: #FFF; padding: 15px;",
           column(width=12, style="padding: 15px; border-radius: 20px;",
                  shinyjs::hidden(
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
                                 icon = shiny::icon("download")),
                  downloadButton("download_fasta", "Download .fasta", 
                                 class="btn-primary", 
                                 style="color:#fff", 
                                 icon = shiny::icon("download"))
                  )),
    DT::dataTableOutput("fastaTable", width = "100%"))),
    div(class="alert-box alert alert-success", role="alert", style="display: none;",
        "Sequence copied to clipboard")
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
          shinyjs::show("search-container")
          shinyjs::show("downloadbtn-container")
          r$fasta_filtered <- r$fasta
        } else if (!file_ext %in% c('FASTA', 'fasta', 'txt')){
          errorModal(message = sprintf("Invalid file format: %s", file_name))
        } else if (file_size >= max_size){
          errorModal(message = sprintf("Error uploading file. Max filesize: %s", filesize))
        } else if((substr(readLines(file_path, n=1),1,1) != ">")){
          errorModal(message = "Expected > at beginning of line")
        }
      
    })
  
  #Filter FASTA on keyword
  observeEvent(input$search, priority = 20,{
    search_filter <- isolate(input$search_filter)
    r$fasta_filtered <- r$fasta
    if(search_filter != ""){
      r$fasta_filtered <- search_across(r$fasta, search_filter)
    }
  })
  
  #Filter FASTA list option
  observeEvent(input$optList, priority = 20, {
    showModal(listModal())
    shinyjs::disable("listSearch")
  })
  #Enable list search button
  observe({
          if(input$id_list != "" && !is.null(input$id_list)){
            shinyjs::enable("listSearch")
            r$id_list <- split_IDs(isolate(input$id_list), collapse = FALSE)
            updateActionButton(session, "listSearch", label = sprintf("Search %s IDs", length(r$id_list)))
          } else {
            updateActionButton(session, "listSearch", label = "Search IDs")
            shinyjs::disable("listSearch")
            r$id_list <- NULL
          }
          })
  #Search IDs
  observeEvent(input$listSearch, priority = 20,{
    id_list <- paste(r$id_list, collapse = "|")
    r$fasta_filtered <- r$fasta
    if(id_list != ""){
      r$fasta_filtered <- r$fasta %>% filter(str_detect(names,id_list))
      removeModal()
    }
  })
  
  #Table output
  output$fastaTable <- DT::renderDataTable({
    req(r$fasta_filtered)
    datatable(
      r$fasta_filtered %>% rename("Sequence" = "sub_seq", "Name" = "names") %>% select(-seq, -width),
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
      write.table(r$fasta_filtered[, c("names", "seq")], file, sep=",", row.names = FALSE)
    }
  )
  # Download filtered FASTA
  output$download_fasta <- downloadData <- downloadHandler(
    filename = function() {
      paste(input$upload[["name"]], "_filtered.fasta", sep="")
    },
    content = function(file) {
      df2fasta(r$fasta_filtered[, c("names", "seq")], file)
    }
  )
  
  
  

}
# Run the application 
options(shiny.maxRequestSize = max_size)
shinyApp(ui = ui, server = server)
