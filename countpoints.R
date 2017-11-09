library(dplyr)
countpoints <- function(filename = "Stat1201Test2Sample.Rmd", details = FALSE) {
  x <- readLines(filename)
  y <- grep("[1-9] points|10 points", x, value = TRUE)
  bracket <- stringr::str_locate(y[1], "\\[")[1]
  z <- sapply(y, function(x) substr(x, bracket + 1, bracket + 2)) %>%
    as.numeric()
  print(rbind(1:length(z), z))
  sum(z)
}

