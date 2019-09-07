# points can be anywhere following a [

newcount <- function(filename) {
  readLines(filename) %>%
    stringr::str_extract("\\[[0-9]+ points") %>%
    na.omit() %>%
    readr::parse_number() %>%
    sum() %>%
    print()
}
