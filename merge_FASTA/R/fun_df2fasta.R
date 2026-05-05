#Convert df to FASTA
df2fasta <- function(data, filepath){
  fasta_lines <- unlist(Map(function(name, seq) {
    wrapped <- strwrap(seq, width = 60)
    c(paste0(">", name), wrapped)
  }, data$names, data$seq), use.names = FALSE)
  writeLines(fasta_lines, filepath, useBytes = TRUE)
  invisible(filepath)
}
