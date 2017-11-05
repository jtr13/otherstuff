tex2r <- function (x) {
  x %>% stringr::str_replace_all("\\\\", "") %>%
    stringr::str_replace_all("left\\[", "(") %>%
    stringr::str_replace_all("right\\]", ")") %>%
    stringr::str_replace_all("\\{", "(") %>%
    stringr::str_replace_all("\\}", ")") %>%
    stringr::str_replace_all("_", "") %>%
    stringr::str_replace_all("frac", "") %>%
    stringr::str_replace_all("'", "prime") %>%
    stringr::str_replace_all("\\$", "") %>%
    stringr::str_replace_all("=", "<-")

}

