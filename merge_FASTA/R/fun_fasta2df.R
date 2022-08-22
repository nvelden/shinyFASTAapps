#Convert FASTA to df
fasta2df <- function(filename){
  bs_fasta <- Biostrings::readBStringSet(filename)
  if(length(bs_fasta) == 0){
    return("No sequences detected in file.")
  }
  df_fasta <- 
    data.frame(
      width=as.integer(bs_fasta@ranges@width),
      names=as.character(bs_fasta@ranges@NAMES),
      seq=as.character(bs_fasta)
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