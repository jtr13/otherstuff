# for use with drawdata.xyz
#
makemore <- function(row) dplyr::slice(tibble::tibble(x = row[1], y = row[2], z = row[3]), rep(1, each = row[2]))

dens2val <- function(data) {
  data$z <- as.numeric(factor(df$z))
  data$y <- data$y * 1000
  df <- do.call(rbind, apply(data, 1, makemore))
  df$group <- LETTERS[df$z]
  df[,c("x", "group")]
}
