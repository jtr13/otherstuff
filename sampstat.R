sampstat <- function(x) {
  round(data.frame(min = min(x),
    Q1 = quantile(x)[2][[1]],
    median = median(x),
    Q3 = quantile(x)[4][[1]],
    max = max(x),
    n = round(length(x),0),
    s = sd(x),
    se = sd(x)/sqrt(length(x))), 2)
}
