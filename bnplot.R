# plot of normal approximation of binomial
bnplot <- function(n, p, x = NULL, curve = TRUE,
                   shade = TRUE, contcorr = TRUE) {
  if (!curve){
    shade <- FALSE
    contcorr <- FALSE
  }
  if (!shade) contcorr <- FALSE
  df <- data.frame(x = 0:n, ybn = dbinom(0:n, n, p))
  g <- ggplot() +
    geom_col(data = df, aes(x, ybn),
             color = "grey50", fill = "lightblue") +
    scale_x_continuous(breaks = 0:n, limits = c(0, n)) +
    scale_y_continuous(expand = expand_scale(c(0, .05))) +
    ylab("density") +
    theme_grey(14) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  if (curve) {
    xn <- seq(0, n, .1)
    yn <- dnorm(xn, mean = n*p, sd = sqrt(n*p*(1-p)))
    df2 <- data.frame(xn, yn)
    g <- g + geom_line(data = df2, aes(xn, yn))
    }
  if (shade) {
      if (is.null(x)) x <- n
      df3 <- df2 %>% filter (xn <= x)
      g <- g + geom_area(data = df3, aes(x = xn, y = yn),
                         fill = "yellow", alpha = .5)
  }
  if (contcorr) {
    if (is.null(x)) x <- n
    df4 <- df2 %>% filter (xn >= x & xn <= x + .5)
    g <- g + geom_area(data = df4,
      aes(x = xn, y = yn), color = "black",
      fill = "yellow", alpha = .8)
  }
  g
}
