# https://erkal.github.io/elm-3d-playground-exploration/red-faced-cube/

library(tidyverse)

# new move
new_move <- function(x, y, m, color) {
  all_x <- c(x, x - 1, x, x + 1)
  all_y <- c(y + 1, y, y - 1, y)
  direction <- c("down", "left", "up", "right")
  new_color <- case_when(
    color == "red" ~ c("red_bottom", "red_left", "red_top", "red_right"),
    color == "yellow" ~ c("red_top", "red_right", "red_bottom", "red_left"),
    color == "red_bottom" ~ c("yellow", "red_bottom", "red", "red_bottom"),
    color == "red_left" ~ c("red_left", "yellow", "red_left", "red"),
    color == "red_top" ~ c("red", "red_top", "yellow", "red_top"),
    color == "red_right" ~ c("red_right", "red", "red_right", "yellow"))
  df <- data.frame(all_x, all_y, direction, new_color)

  # not red
  df <- df %>% filter(new_color != "red")

  # not off the board
 df <- df %>%
    filter(all_x > 0 & all_x < 9 & all_y > 0 & all_y < 9)

  # not taken
  takeaway <- vector()
  for (i in 1:nrow(df)) {
    if (!is.na(m[df$all_y[i], df$all_x[i]])) takeaway <- c(takeaway, i)
  }
  if(length(takeaway) > 0) df <- df %>% slice(-takeaway)
  # not red
  move <- sample(nrow(df), 1)
  df %>% slice(move)
}

go <- function() {
  m <- matrix(NA, nrow = 8, ncol = 8)
  x <- 1
  y <- 1
  color <- "red"
  all_moves <- data.frame(all_x = x, all_y = y, direction = NA, new_color = color)
  m[1, 1] <- 1

  while(nrow(all_moves) < 64) {
  new_pos <- new_move(x, y, m, color)
  if(nrow(new_pos) == 0) {
    return(m)
  }
  all_moves <- bind_rows(all_moves, new_pos)
  x <- new_pos$all_x
  y <- new_pos$all_y
  color <- new_pos$new_color
  m[y, x] <- nrow(all_moves)
  }
 print("Done")
}

m <- matrix(NA, nrow = 8, ncol = 8)
while(sum(is.na(m)) > 20) {
  m <- go()
}

m



