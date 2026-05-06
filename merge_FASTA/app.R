#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(DT)
source('R/custom_inputs.R')
source('R/fun_fasta2df.R')
source('R/fun_df2fasta.R')
source('R/fun_modals.R')

# Setup the bslib theme object
my_theme <- bs_theme(base_font = "Inter", bg="#EEF7FF", fg="#000", primary="#007AFF")

#Max file upload in bytes
max_size <- 10*1024^2
filesize <- "10 MB"

ui <- shiny::fluidPage(
  title = "Merge FASTA files",
  theme = my_theme,
  style = 'padding: 0;',
  lang = "en",
  includeCSS('www/fileDownload.css'),
  includeCSS('www/shinyfasta-app.css'),
  tags$script(src="iframeSizer.contentWindow.min.js"),
  useShinyjs(),
  #return id when delete button is clicked
  tags$head(tags$script(
    HTML(
      "$(document).on('click', '.deletebutton', function () {
                                Shiny.onInputChange('del_clicked',this.id);
                             });"
    )
  )),
  fileInputArea(
    "upload",
    label = "Drag files here",
    buttonLabel = sprintf("max upload size %s", filesize),
    icon = "upload.svg",
    multiple = TRUE,
    accept = c(".fasta", ".FASTA", ".txt")
  ),
  checkboxInput("duplicates", "Remove duplicate entries", value=TRUE),
  div(id="file-container", width="100%", DT::dataTableOutput("uploadedfiles", width = "100%")),
  downloadButton("merge", "Merge", 
                 class="btn-primary", 
                 style="color:#fff", 
                 icon = shiny::icon("download")
                 ),
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
      
      file_paths <- input$upload[["datapath"]]
      file_exts <- tolower(tools::file_ext(file_paths))
      file_names <- input$upload[["name"]]
      file_sizes <- input$upload[["size"]]

      for (i in 1:length(file_exts)) {
        #Update max file size
        r$filesize <- r$filesize + file_sizes[i]
        if (file_exts[i] %in% c('FASTA', 'fasta', 'txt') 
            && r$filesize <= max_size
            && (substr(readLines(file_paths[i], n=1),1,1) == ">")) {
          #Attach delete button to upload data
          r$count <- r$count + 1
          upload <- input$upload[i, ]
          id <- sprintf("delbutton-%s", r$count)
          upload$del <- del_button(id, "deletebutton")
          upload$id <- id
          #Store upload data in DF
          r$upload <- rbind(r$upload, upload)
          shinyjs::enable("merge")
        } else if (!file_exts[i] %in% c('FASTA', 'fasta', 'txt')){
          errorModal(message = sprintf("Invalid file format: %s", file_names[i]))
        } else if (r$filesize >= max_size){
          errorModal(message = sprintf("Error uploading file. Max filesize: %s", filesize))
        } else if((substr(readLines(file_paths[i], n=1),1,1) != ">")){
          errorModal(message = "Expected > at beginning of line")
        }
      }
    })
  
  #Delete Rows when delete button is clicked
  observeEvent(input$del_clicked, priority = 20, {
    r$upload <- r$upload[!(r$upload$id == input$del_clicked),]
    r$filesize <- sum(r$upload[["size"]])
    if(nrow(r$upload) == 0){
      shinyjs::disable("merge")
    }
    })
    
  output$uploadedfiles <- DT::renderDataTable({
    input$del_clicked
    req(input$upload)
    req(r$upload)
    datatable(
    r$upload[,c("name","size","del")] %>% mutate(size = paste(round(size/1000, 0), "KB")),
    rownames = FALSE,
    escape = FALSE,
    width="100%",
    selection = "none",
    options = list(
      dom = 't',
      ordering = FALSE,
      orderable = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'display': 'none'});",
        "}"
        )
    ))
  })
  
  #Merge button
  shinyjs::disable("merge")
  output$merge <- downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), "_merged_FASTA", ".fasta", sep="")
    },
    content = function(file) {
      df <- fastas2df(r$upload[["datapath"]], input$duplicates)
      df2fasta(df, file)
    }
  )
}
# Run the application 
options(shiny.maxRequestSize = max_size)
shinyApp(ui = ui, server = server)
