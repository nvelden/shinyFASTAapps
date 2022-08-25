#Error Modal
errorModal <- function(title = "Error", message, size = "s") {
  shiny::showModal(shiny::modalDialog(size = size,
                                      title = title,
                                      message))
}
