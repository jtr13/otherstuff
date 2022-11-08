# Bertrand's paradox
#
# Simulate the proportion of chords formed by two randomly selected points INSIDE a circle greater than the side of an equilateral triangle inscribed in the circle

# Ex.
# > sum(sapply(1:10000, g))
# [1] 7530


g <- function(j) {

z <- c(2, 2)

# randomly select two points make sure they're inside the unit circle
while (sum(z > 1) > 0) {
  x <- runif(2, min = -1, max = 1)
  y <- runif(2, min = -1, max = 1)
  z <- sqrt(x^2 + y^2)
}

# find slope and intercept of line through the two points
mod <- lm(y ~ x)
i <- coef(mod)[1] # intercept
s <- coef(mod)[2] # slope

# solve for points on the line and circumference of circle (chord endpoints)
a <- s^2 + 1
b <- 2*s*i
c <- i^2 - 1
newx <- (-1*b + c(-1, 1)*sqrt(b^2 - 4*a*c))/(2*a)
newy <- predict(mod, data.frame(x = newx))

# calculate length of chord
l <- sqrt((newx[2]-newx[1])^2 + (newy[2]-newy[1])^2)
l > sqrt(3)
}
