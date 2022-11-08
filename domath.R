quadrant <- function(angle) {
  floor(angle/90+1)
}
sinsol <- function(angle) {
  dplyr::case_when(
    quadrant(angle) <= 2 ~ 180 - angle,
    TRUE ~ 540 - angle
  )
}

cossol <- function(angle) 360 - angle

drawunitcircle <- function() {
  plot(x = 0, y = 0, xlim = c(-1, 1), ylim = c(-1.25, 1.25), asp = 1,
       cex = 21.5,
       xlab = "x", ylab = "y", las = 1)
  abline(h = 0)
  abline(v = 0)
}

addangle <- function(angle, color = 1) {
  colors <- c("red", "blue")
  x <- cos(angle*pi/180)
  y <- sin(angle*pi/180)
  arrows(0, 0, x, y, length = .1, col = colors[color])
  text(1.2*x, 1.2*y, angle, col = colors[color])
}

getresponse <- function(solution) {
  ans <- as.numeric(readline())
  if(ans == solution) {
      print("Correct!")
    } else {
      print(paste("Incorrect. The answer is:", solution))
    }
}


sincosunit <- function() {
    angle <- sample(72,1)*5
    func <- sample(c("sin", "cos"), 1)
    cat("Find an angle A between 0 and 360 degrees such that\n")
    cat(paste(func, "A = ", func, angle, " "))
    drawunitcircle()
    addangle(angle)
    solution <- ifelse(func == "sin", sinsol(angle), cossol(angle))
    getresponse(solution)
    addangle(solution, 2)
}

sincostan <- function() {
  #allangles <- c(seq(0, 330, 30), 45, 135, 225, 315)
  allangles <- c(0, 30, 45, 60, 90)
  angle <- sample(allangles, 1)
  drawunitcircle()
  addangle(angle)
  func <- sample(c("sin", "cos", "tan"), 1)
  cat("What is", func, angle, "?\n")
  cat("Answer as a decimal with two decimal places.\n")
  cat("Hint: sqrt(3)/2 = .87; sqrt(2)/2 = .71; sqrt(3) = 1.73; 1/sqrt(3) = .58\n")
  solution <- dplyr::case_when(
    func == "sin" ~ sin(angle*pi/180),
    func == "cos" ~ cos(angle*pi/180),
    func == "tan" ~ tan(angle*pi/180)
  )
  solution <- round(solution, 2)
  getresponse(solution)
}

domath <- function() {
  newprob <- 0
  while(newprob != -1) {
    problemtype <- 2
    if (problemtype == 1) {
      sincosunit()
    } else {
      sincostan()
    }
  newprob <- readline("Hit return for the next problem, -1 to end.")
  }
  cat("Good-bye Zupie")
}

