#Convert FASTA to df
fasta2df <- function(filename){
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  header_idx <- grep("^>", lines)
  if(length(header_idx) == 0){
    return("No sequences detected in file.")
  }

  names <- sub("^>\\s*", "", lines[header_idx])
  seq <- character(length(header_idx))
  for(i in seq_along(header_idx)){
    start <- header_idx[i] + 1
    end <- if(i < length(header_idx)) header_idx[i + 1] - 1 else length(lines)
    seq[i] <- if(start <= end) paste0(gsub("\\s+", "", lines[start:end]), collapse = "") else ""
  }

  df_fasta <- data.frame(
    width = as.integer(nchar(seq)),
    names = as.character(names),
    seq = as.character(seq),
    stringsAsFactors = FALSE
  )
  rownames(df_fasta) <- NULL
  return(df_fasta)
}
#Load multiple FASTA files to df
fastas2df <- function(filenames, remove_duplicates = TRUE){
  bs_fasta_list <- list()
  for(nr in 1:length(filenames)){
    bs_fasta_list[[nr]] <- fasta2df(filenames[nr])
  }
  if(remove_duplicates){
    df <- dplyr::distinct(dplyr::bind_rows(bs_fasta_list))  
  } else {
    df <- dplyr::bind_rows(bs_fasta_list)
  }
  return(df)
}
