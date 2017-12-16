# bracket must be in the same position in each line
# Ex.
# 1.  [3 points]
# 14. [1 points]
#
library(dplyr)
countpoints <- function(filename = "Stat1201Test2Sample.Rmd") {
  x <- readLines(filename)
  y <- grep("[1-9] points|10 points", x, value = TRUE)
  bracket <- stringr::str_locate(y[1], "\\[")[1] # find position of bracket in the first line
  z <- sapply(y, function(x) substr(x, bracket + 1, bracket + 2)) %>%
    as.numeric()
  print(rbind(1:length(z), z))
  sum(z)
}

