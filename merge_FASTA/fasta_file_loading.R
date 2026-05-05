# Example FASTA parsing helpers used during development.

fasta2df <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  header_idx <- grep("^>", lines)
  if (length(header_idx) == 0) {
    return("No sequences detected in file.")
  }

  names <- sub("^>\\s*", "", lines[header_idx])
  seq <- character(length(header_idx))
  for (i in seq_along(header_idx)) {
    start <- header_idx[i] + 1
    end <- if (i < length(header_idx)) header_idx[i + 1] - 1 else length(lines)
    seq[i] <- if (start <= end) paste0(gsub("\\s+", "", lines[start:end]), collapse = "") else ""
  }

  data.frame(
    width = as.integer(nchar(seq)),
    names = as.character(names),
    seq = as.character(seq),
    stringsAsFactors = FALSE
  )
}

duplicate_seq <- function(data) {
  data[duplicated(data$seq), , drop = FALSE]
}

duplicate_names <- function(data) {
  data[duplicated(data$names), , drop = FALSE]
}
