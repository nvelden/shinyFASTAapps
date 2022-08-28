#Search across df
search_across <- function(df, keyword){
  filtered_data <- df %>% 
    filter(if_any(c(names, seq), ~str_detect(., fixed(keyword, ignore_case=TRUE))))
  return(filtered_data)
}
                                 
