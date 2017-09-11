mmdotplot <- function(x) {
  plot(x, rep(1, length(x)), axes = FALSE, ann = FALSE)
  abline(h = 1)
  points(mean(x), 1, col = "red", pch = 19)
  points(median(x), 1, col = "blue", pch = 19)
  legend("bottomright", c("mean", "median"), col = c("red", "blue"), pch = 19)
}
