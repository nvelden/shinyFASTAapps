#Split string on whitespace (comma, space, line-break)
#Remove elements that only contain special characters
split_IDs <- function(string, collapse = FALSE){
  id_list <- str_split(string, "[ ,\n]+")[[1]]
  id_list <- str_subset(id_list, "^[^a-zA-Z0-9]+$", negate = TRUE)
  id_list <- id_list[id_list != ""]
  id_list <- id_list[!duplicated(id_list)]
  if(collapse){
  id_list <- paste(id_list, collapse = "|") 
  }
  return(id_list)
}