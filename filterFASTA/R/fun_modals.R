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
