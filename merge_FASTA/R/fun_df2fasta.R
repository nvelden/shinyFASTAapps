#Convert df to FASTA
df2fasta <- function(data, filepath){
  seq <- data$seq 
  names(seq) <- data$names
  Xstring <- Biostrings::BStringSet(unlist(seq))
  fasta <- Biostrings::writeXStringSet(Xstring, filepath)
  return(fasta)
}