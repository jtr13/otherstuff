# Replaces the first space in a string after every multiple of num with a line break

add_line_breaks <- function(x, num = 40) {
  for (i in 1:(nchar(x)/num)) {
    b <- data.frame(str_locate_all(x, " ")) %>%
      filter(start > num*i)
    pos <- b[1, 1]
    if (!is.na(pos)) substr(x, pos, pos) <- "\n"
  }
  x
}
