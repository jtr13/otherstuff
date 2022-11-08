# Remove code from .Rmd -- useful for creating homework templates
# To keep the chunk, put a space before it
# Ex. a <- remove_chunks("PSet1/PSet1-solutions.qmd")
# write(a, file = "PSet1/PSet1.qmd")
remove_chunks <- function(filename) {
  rmd_file <- readLines(filename)
  chunk <- FALSE
  index <- grepl("^\\`\\`\\`", rmd_file)
  newindex <- index
  for (i in seq_along(index)) {
    newindex[i] <- ifelse(chunk, TRUE, index[i])
    if (index[i]) {
      chunk <- ifelse(chunk, FALSE, TRUE)
    }
  }
  newfile <- rmd_file[!newindex]
  gsub("^ \\`", "\\`", newfile)
}

