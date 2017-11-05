library(tidyverse)
x <- seq(.05, .95, .1)
firstnum <- 0
lastnum <- 100
digits <- data.frame()
for (i in firstnum:lastnum) {
  r <- round(i + x, 1)
  digits <- rbind(digits, 10*(r - trunc(r)))
}
colnames(digits) <- paste0("x",0:9)

heatmap(as.matrix(digits), Rowv = NA)

tidydig <- digits %>%
  add_column(int = firstnum:lastnum) %>%
  gather(key, value, -int)

g <- ggplot(tidydig, aes(key, int, fill = factor(round(value, 1)))) + geom_tile() + theme_classic()

