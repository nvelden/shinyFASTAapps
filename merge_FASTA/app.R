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
library(shinyBS)
library(DT)
source('R/custom_inputs.R')
source('R/fun_fasta2df.R')
source('R/fun_df2fasta.R')

# Setup the bslib theme object
my_theme <- bs_theme(base_font = bslib::font_google("Inter"), bg="#EEF7FF", fg="#000", primary="#007AFF")

#Max file upload in bytes
max_size <- 1000000

ui <- shiny::fluidPage(
  title = "File input area for Shiny (Bootstrap 5)",
  theme = my_theme,
  style = 'padding: 0;',
  lang = "en",
  includeCSS('www/fileDownload.css'),
  #return id when delete button is clicked
  tags$head(tags$script(HTML("$(document).on('click', '.deletebutton', function () {
                                Shiny.onInputChange('del_clicked',this.id);
                             });"))),
             fileInputArea(
               "upload",
               label = "Drag files here",
               buttonLabel = "max upload size 1 MB",
               icon = "upload.svg",
               multiple = TRUE,
               accept = c(".fasta", ".FASTA")
            ),
            DT::dataTableOutput("uploadedfiles", width = "50%"),
            downloadBttn("merge", "Merge", color="primary", style="simple"),
          )
fileInput
server <- function(input, output, session) {
  
    #Reactive dataframe to store values    
    r <- reactiveValues()
    #Reactive counter for del button IDs
    r$count <- 0
    #Reactive counter for total filesize
    r$filesize <- 0
    
    #Store data in reactive DF
    observeEvent(input$upload, priority = 20, {
      
      file_exts <- tolower(tools::file_ext(input$upload[["datapath"]]))
      file_names <- input$upload[["name"]]
      file_sizes <- input$upload[["size"]]

      for (i in 1:length(file_exts)) {
        #Update max file size
        r$filesize <- r$filesize + file_sizes[i]
        if (file_exts[i] %in% c('FASTA', 'fasta') && r$filesize <= max_size) {
          #Attach delete button to upload data
          r$count <- r$count + 1
          upload <- input$upload[i, ]
          id <- sprintf("delbutton-%s", r$count)
          upload$del <- del_button(id, "deletebutton")
          upload$id <- id
          #Store upload data in DF
          r$upload <- rbind(r$upload, upload)
        } else if (!file_exts[i] %in% c('FASTA', 'fasta')){
           showModal(modalDialog(
             size = "s",
             title = "Error",
             sprintf("Invalid file format: %s", file_names[i])
           ))
        } else if (r$filesize >= max_size){
          showModal(modalDialog(
            size = "s",
            title = "Error",
            sprintf("Error uploading file. Max filesize: 1 MB")
          ))
        }
      }
    })
  
  #Delete Rows when delete button is clicked
  observeEvent(input$del_clicked, priority = 20, {
    r$upload <- r$upload[!(r$upload$id == input$del_clicked),]
    r$filesize <- sum(r$upload[["size"]])
    })
    
  observeEvent(input$combine, priority = 20, {
    fasta2df(filesDF$values[["datapath"]])
  })

  output$uploadedfiles <- DT::renderDataTable({
    input$del_clicked
    req(input$upload)
    datatable(
    r$upload[,c("name","size","del")] %>% mutate(size = paste(round(size/1000, 0), "KB")),
    rownames = TRUE,
    escape = FALSE,
    selection = "none",
    options = list(
      dom = 't',
      ordering = FALSE,
      autoWidth = TRUE,
      orderable = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'display': 'none'});",
        "}"
        )
    ))
  })
  
  output$merge <- downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), "_merged_FASTA", ".fasta", sep="")
    },
    content = function(file) {
      df <- fastas2df(r$upload[["datapath"]])
      df2fasta(df, file)
    }
  )
}

# Run the application 
options(shiny.maxRequestSize = 30 * 1024^2)
shinyApp(ui = ui, server = server)
