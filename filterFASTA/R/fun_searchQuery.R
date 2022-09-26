#Make a DF of all reactive inputs 
inputs_to_DF <- function(inputs){
  x <- reactiveValuesToList(inputs)
  #Filter for advanced inputs
  x <- x[grepl("advText|advOperator|advName", names(x))]
  names <- names(x)
  #Replace NULL values with NA
  x[sapply(x, is.null)] <- NA
  
  inputDF <- data.frame(
    names = names(x),
    values = unlist(x, use.names = FALSE)
  )
  return(inputDF)
}

input_to_queryDF <- function(data){
  query_df <- data %>%
    dplyr::filter(!is.na(values)) %>%
    arrange(names) %>%
    mutate(
      group =
        case_when(
          str_detect(names, "advOperator") ~ "operator",
          str_detect(names, "advName") ~ "name",
          str_detect(names, "advText") ~ "text",
          TRUE ~ "other"
        )
    ) %>%
    select(-names) %>%
    filter(group != "other") %>% 
    as_tibble() %>%
    group_split(group, .keep = FALSE) %>%
    bind_cols()

    names(query_df) <- c("columns", "operators", "inputs")
    
    query_df <- query_df %>% filter(inputs != "" & inputs != " ")

  return(query_df)
}

queryDF_to_dplyr <- function(queryDF){
  
  dplyr_query <- NULL
  
  columns <- queryDF$columns
  operators <- queryDF$operators
  inputs <- queryDF$inputs
  
  for(i in 1:length(columns)){
    
    if(i == 1 & operators[i] == "None"){
      dplyr_query <- sprintf('filter(str_detect(%s, fixed("%s", ignore_case=TRUE))',
                       columns[i],
                       inputs[i])
    } else if(i == i & operators[i] == "NOT"){
      dplyr_query <- sprintf('filter(!str_detect(%s, fixed("%s", ignore_case=TRUE))',
                                    columns[i],
                                    inputs[i])
    } else if(operators[i] == "AND"){
      dplyr_query <- paste0(dplyr_query, 
                      sprintf(' & str_detect(%s, fixed("%s", ignore_case=TRUE))',
                              columns[i],
                              inputs[i])
      )
    } else if(operators[i] == "NOT"){
      dplyr_query <- paste0(dplyr_query,
                      sprintf(' & !str_detect(%s, fixed("%s", ignore_case=TRUE))',
                              columns[i],
                              inputs[i])
      )
    } else if(operators[i] == "OR"){
      dplyr_query <- paste0(dplyr_query,
                      sprintf(' | str_detect(%s, fixed("%s", ignore_case=TRUE))',
                              columns[i],
                              inputs[i])
      )
    }
  }
  
  dplyr_query <- paste(dplyr_query, ")")
  return(dplyr_query)
}

eval_dplyr_query <- function(query, data){
  filtered_df <- { eval(parse(text=paste0('data %>% ', query))) } 
  return(filtered_df)
}

# queryDF <- suppressMessages(input_to_queryDF(inputsDF))
# query <- queryDF_to_dplyr(queryDF)
# fasta <- fasta2df("FASTAS/human.fasta")
# 
# { eval(parse(text='fasta %>% filter(str_detect(names, "ECOLI"))')) }
# 
# fasta %>% { eval(parse(text=query)) }
# 
# eval_dplyr_query(query, fasta)

