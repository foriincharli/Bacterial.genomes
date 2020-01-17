# https://johnbhorne.wordpress.com/2016/06/11/quick-n50-and-n90-calculation-in-r/

# function to calculate N50 ####
N50_calc <- function(x) {
  contigs = NULL
  cond <- length(x)
  while (cond > 0) {contigs <- c(contigs, length(x[[cond]])); cond <- (cond - 1);}
  N50 <- unlist(tapply(contigs, contigs, function(x) rep(x[1], sum(x))))
  return(median(N50))
}

# read fasta file ####
library(ape)
data <- read.dna("bacterial_genome_1.fasta", format = "fasta", as.character = TRUE)

N50_calc(data)
