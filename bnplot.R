# plot of normal approximation of binomial
bnplot <- function(n, p, x = NULL, a = 0, curve = TRUE,
                   shade = FALSE, contcorr = FALSE) {
  df <- data.frame(x = 0:n, ybn = dbinom(0:n, n, p))
  g <- ggplot() +
    geom_col(data = df, aes(x, ybn),
             color = "grey50", fill = "lightblue") +
    scale_x_continuous(breaks = 0:n) +
    scale_y_continuous(expand = expand_scale(c(0, .05))) +
    ylab("density") +
    theme_grey(14) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  if (curve) {
    xn <- seq(-1, n + 1, .05)
    yn <- dnorm(xn, mean = n*p, sd = sqrt(n*p*(1-p)))
    df2 <- data.frame(xn, yn)
    g <- g + geom_line(data = df2, aes(xn, yn))
    }
  if (shade) {
      if (is.null(x)) x <- n
      df3 <- df2 %>% filter (xn <= x & xn >= a)
      g <- g + geom_area(data = df3, aes(x = xn, y = yn),
                         fill = "yellow", alpha = .5)
  }
  if (contcorr) {
    if (is.null(x)) x <- n
    df4 <- df2 %>% filter (xn >= x & xn <= x + .45)
    df5 <- df2 %>% filter (xn >= a - .45 & xn <= a)
    g <- g + geom_area(data = df4,
        aes(x = xn, y = yn), color = "black",
        fill = "yellow", alpha = .8) +
      geom_area(data = df5,
        aes(x = xn, y = yn), color = "black",
        fill = "yellow", alpha = .8)
  }
  g
}
