library(dplyr)
countpoints <- function(filename = "Stat1201Test2Sample.Rmd", details = FALSE) {

  x <- readLines(filename)
  y <- grep("[1-9] points|10 points", x, value = TRUE)
  z <- sapply(y, function(x) substr(x, 4, 5)) %>%
    as.numeric()
  print(rbind(1:length(z), z))
  sum(z)
}

