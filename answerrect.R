# answerrect.R
# use fig.height = 2
smallrect <- function() {
  par(mar = c(0,0,0,0))
  plot(1, 2, xlim = c(0,8), ylim = c(0, 2),
       col = "white", ann = FALSE,
       axes = FALSE)
  rect(6, 0, 8, .5)
}

largerect <- function() {
  par(mar = c(0,0,0,0))
  plot(1, 2, xlim = c(0,8), ylim = c(0, 2),
       col = "white", ann = FALSE,
       axes = FALSE)
  rect(4, 0, 8, .5)
}
