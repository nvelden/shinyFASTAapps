fileInputArea <- function(inputId, label, multiple = FALSE, accept = NULL, icon = NULL,
                          width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    # Don't use "display: none;" style, which causes keyboard accessibility issue; instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )
  
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  
  div(
    class = "form-group shiny-input-container w-100",
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    shiny:::shinyInputLabel(inputId, ""),
    div(
      class = "input-group mb-3",
      style = 'padding: 0;',
      # input-group-prepend is for bootstrap 4 compat
      tags$label(
        class = "input-group-btn input-group-prepend w-100",
        span(
          class = "btn btn-area w-100", inputTag,
          div(tags$image(src = icon, width = "80px;"), style = "margin-top: 2rem;"),
          div(p(label), style = "font-size: 1.2rem; font-weight: 700; padding-top: 2rem;"),
          div(p(buttonLabel), style = "font-size: 1rem; font-weight: 400; margin-bottom: 2rem;"),
          div(
             id = paste(inputId, "_progress", sep = ""),
             class = "progress active shiny-file-input-progress",
             tags$div(class = "progress-bar")
          )
        )
      )
    ),
  )
}

# Icon from <https://icons.getbootstrap.com/icons/upload/>
icon_file <- tempfile(fileext = ".svg")
writeLines('
<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="#495057" class="bi bi-upload" viewBox="0 0 16 16">
  <path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"/>
  <path d="M7.646 1.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 2.707V11.5a.5.5 0 0 1-1 0V2.707L5.354 4.854a.5.5 0 1 1-.708-.708l3-3z"/>
</svg>',
           con = icon_file
)
icon_encoded <- xfun::base64_uri(icon_file)

card <- function(title, ...) {
  htmltools::tags$div(
    class = "card",
    htmltools::tags$div(class = "card-header", title),
    htmltools::tags$div(class = "card-body", ...)
  )
}

del_button <- function(id, class, icon="trash-can"){
  sprintf('<a id=\"%s\"
              href="#" 
              class=\"action-button shiny-bound-input %s\" 
              >
           <i class=\"fa fa-%s\" role=\"presentation\" aria-label=\"download icon\"></i>
            </a>',
        id, 
        class,
        icon
  )
}

clipboard_button <- function(id, text){
  sprintf('<button id=\"%s\" 
                   type=\"button\" 
                   file=%s
                   class=\"clipboard btn btn-link btn-sm\"
                   onclick=\"tableFunction(this)\">
                   <i class=\"fa fa-copy fa-2x\"></i></button>
           <script>
                   function tableFunction(click) {
                   var file = $(click).attr("file");
                   var $temp = $("<input>");
                   $("body").append($temp);
                   $temp.val(file).select();
                   document.execCommand("copy");
                   $temp.remove(); 
                   $(".alert-box").removeClass("in").show();
	                 $(".alert-box").delay(200).addClass("in").fadeOut(1200);
                   }
           </script>', id, text)
  
}

keyBinding <- function(inputID, actionID, key = "Enter"){
 tags$head(
   HTML(
     sprintf(
          '<script type="text/javascript">
           $(document).keyup(function(event) {
          if ($("#%s").is(":focus") && (event.key == "%s")) {
          $("#%s").click();
          }
          });
          </script>', inputID, key, actionID)
        )
       )
}