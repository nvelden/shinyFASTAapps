Extinction <- function(sequence)({
  
  n_trp <- str_count(sequence, "W")
  n_tyr <- str_count(sequence, "Y")
  n_cys <- floor(str_count(sequence, "C") / 2 )
  
  Extinction <- (n_trp * 5500) + (n_tyr * 1490) + (n_cys * 125)
  
  return(Extinction)  
  
})

param_to_DF <- function(fastaDF, params){
  
if("Mw" %in% params){
  fastaDF <- fastaDF %>% mutate(`Mw` = round(Peptides::mw(seq) / 1000, 2))
} 
if("pI" %in% params){
  fastaDF <- fastaDF %>% mutate(pI = round(Peptides::pI(seq), 3))
} 
if("Extinction" %in% params){
  fastaDF <- fastaDF %>% mutate(Extinction = round(Extinction(seq), 3))
}
if("Absorbance" %in% params){
  fastaDF <- fastaDF %>% mutate(Absorbance = round((Extinction(fastaDF$seq) / mw(fastaDF$seq)),3))
}
if("Length" %in% params){
  fastaDF <- fastaDF %>% mutate(Length = Peptides::lengthpep(seq = fastaDF$seq))
}
if("Charge" %in% params){
  fastaDF <- fastaDF %>% mutate(Charge = round(Peptides::charge(seq = fastaDF$seq), 3))
}
  if("Hydrophobicity" %in% params){
    fastaDF <- fastaDF %>% mutate(Hydrophobicity = round(Peptides::hydrophobicity(seq = fastaDF$seq), 3))
  }
  return(fastaDF)
}
