library(shiny)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)
library(stringr)
source('R/custom_inputs.R')
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
  title = "Filter FASTA files",
  theme = my_theme,
  style = 'padding: 0;',
  lang = "en",
  includeCSS('www/advSearch.css'),
  includeCSS('www/shinyfasta-app.css'),
  tags$script(src="iframeSizer.contentWindow.min.js"),
  useShinyjs(),
  #return id when delete button is clicked
  #Set inputs to NULL
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
    fluidRow(id="input-container", style="padding: 0;",
             column(width=12, style="padding: 15px; border-radius: 20px;",
    fileInput("upload", 
              "Upload FASTA file",
              multiple = FALSE,
              accept = c(".fasta", ".FASTA", ".txt"),
              width = "100%"
              ),
    shinyjs::hidden(
    div(id="search-container", style="display: flex; align-items: flex-start; margin-bottom: -25px; margin-top: 50px;",
        div(
    textInputIcon("search_filter", label=NULL, width="100%", icon=icon("magnifying-glass")),
    div(style="margin-top: -15px;",
    helpText(actionLink("optAdvanced", "Advanced"), "|",
             actionLink("optList", "List"), "|",
             actionLink("optRange", "Range")))),
    actionButton("search", "Search", class="btn-primary", style="color:#fff"),
    )
  )
             )
  ),
  fluidRow(id="table-container", style="padding: 15px; min-height:400px;",
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
    DT::dataTableOutput("fastaTable", width = "100%")))
)
)

server <- function(input, output, session) {
  
    #Reactive dataframe to store values    
    r <- reactiveValues()
    #Reactive counter for del button IDs
    r$count <- 0
    #Reactive counter for total filesize
    r$filesize <- 0
    #Reactive DF to store reactive inputs
    r$inputsDF <- NULL
    #Reactive DF to store queryDF for advanced search
    r$queryDF <- NULL
    #Reactive DF to store dplyr query
    r$dplyr_query <- NULL

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
  
  #Filter FASTA advanced filtering option
  observeEvent(input$optAdvanced, priority = 20, {
    showModal(advancedModal())
    shinyjs::disable("advSearch")
  })

  #Eneable advanced search button
  #Enable list search button
  observe({
    if(input$`advText-1` != "" && !is.null(input$`advText-1`)){
      shinyjs::enable("advSearch")
    } else {
      shinyjs::disable("advSearch")
    }
  })
  #showModal(advancedModal())
  #Remove search filter
  observeEvent(input$adv_del_clicked, priority = 20,{
    nr <- str_remove(input$adv_del_clicked, "remove-")
    id <- sprintf("#search-container-%s", nr)
    #Set input values to null such that they can be filtered out
    runjs(
      sprintf(
        'Shiny.onInputChange("advName-%s", null);
         Shiny.onInputChange("advOperator-%s", null);
         Shiny.onInputChange("advText-%s", null);',
        nr, 
        nr, 
        nr)
      )
    removeUI(id, immediate = TRUE)
  })
  
  r$searchFieldNr <- 3
  #Add search filter
  observeEvent(input$addSearchField, priority = 20,{
    id <- sprintf("#search-container-%s", r$searchFieldNr)
    r$searchFieldNr <- r$searchFieldNr + 1 
    insertUI(selector = "#search-inputs", 
             where = "beforeEnd", 
             ui = advSearch_input(r$searchFieldNr)
             )
  })
  
  
  observeEvent(input$advSearch, priority = 20, {
    r$fasta_filtered <- r$fasta
    r$inputsDF <- inputs_to_DF(input)
    r$queryDF <- suppressMessages(input_to_queryDF(r$inputsDF))
    r$dplyr_query <- queryDF_to_dplyr(r$queryDF)
    r$fasta_filtered <- eval(parse(text=paste0('r$fasta_filtered %>%',  r$dplyr_query)))
    removeModal()
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
  
  #Search range
  observeEvent(input$optRange, priority = 20, {
    showModal(rangeModal())
    shinyjs::disable("rangeSearch")
  })
  
  #Enable range search button
  observe({
    if(!is.null(input$rangeFrom) & 
       !is.null(input$rangeTo) &
       is.numeric(input$rangeFrom) &
       is.numeric(input$rangeTo)){
         if(input$rangeTo != 0 &
            input$rangeTo > 0 &
            input$rangeFrom > 0 &
            input$rangeFrom < input$rangeTo){
        shinyjs::enable("rangeSearch")
      } else {
        shinyjs::disable("rangeSearch")
      }
  }
  })
  
  #Filter DF based on range input
  observeEvent(input$rangeSearch, priority = 20, {
    r$fasta_filtered <- r$fasta
    r$fasta_filtered <- r$fasta_filtered %>% filter(between(width, input$rangeFrom, input$rangeTo))
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
        autoWidth = FALSE,
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
options(shiny.maxRequestSize = max_size,shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
