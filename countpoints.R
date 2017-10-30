countpoints <- function(filename = "Stat1201Test2Sample.Rmd") {
  x <- readLines(filename)
  y <- grep("[1-9] points", x, value = TRUE)
  sapply(y, function(x) substr(x, 4, 4)) %>% as.numeric() %>% sum()
}

