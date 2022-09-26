#Error Modal
errorModal <- function(title = "Error", message, size = "s") {
  shiny::showModal(shiny::modalDialog(size = size,
                                      title = title,
                                      message))
}

#Modal for list search
listModal <- function() {
  modalDialog(
    h1("Retrieve/ID mapping"),
    p("Enter your IDs. Separate IDs by whitespace (space, tab, newline) or commas."),
    textAreaInput("id_list", 
                  label= NULL, 
                  width= "100%", 
                  height = "300px", 
                  resize = "none", 
                  placeholder = "P31946 P62258 ALBU_HUMAN EFTU_ECOLI"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("listSearch", "Search IDs", class="btn-primary", style="color:#fff")
    )
  )
}

#Function to insert advanced search button
#Delete button is triggered using advdeletebutton class
advSearch_input <- function(nr = 1, opr_opt = c("AND", "OR", "NOT"), del = TRUE){
  div(id=sprintf("search-container-%s", nr), 
      class="advanced-container",
      selectInput(sprintf("advOperator-%s", nr), 
                  label = "",
                  opr_opt, 
                  selected = NULL,
                  multiple = FALSE,
                  width = "90px"),
      selectInput(sprintf("advName-%s", nr), 
                  label = "",
                  multiple = FALSE,
                  choices=c("Name" = "names", "Sequence" = "seq"), 
                  selected = "",
                  width = "150px"),
      textInput(sprintf("advText-%s", nr), label = "", placeholder = "Search", width="500px"),
      if(del){
      div(style="align-self: center",
          HTML(del_button(sprintf("remove-%s", nr), class = "advdeletebutton")))
          }
  )
}

#Modal for advanced search
advancedModal <- function() {
  modalDialog(
    size = "l",
    title = "Advanced Search",
    div(id="search-inputs",
    advSearch_input(nr = 1, opr_opt = c("None", "NOT"), del = FALSE),
    advSearch_input(nr = 2),
    advSearch_input(nr = 3)
    ),
    actionLink("addSearchField", "Add Field"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("advSearch", "Search", class="btn-primary", style="color:#fff")
    )
  )
}

#Modal for range search
rangeModal <- function() {
  modalDialog(
    size = "m",
    h1("Filter by length"),
    fluidRow(
    column(6,
    numericInput("rangeFrom", "From", 1, min=1)
    ),
    column(6,
    numericInput("rangeTo", "To", 1000, min=1)
    )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("rangeSearch", "Search", class="btn-primary", style="color:#fff")
    )
  )
}
