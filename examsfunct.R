# useful functions for exams package
# round away from zero
r <- function(x, digits = 2) {
  if (x > 0) {
    round(x+(.1^(digits+1)),digits)
    } else {
    round(x-(.1^(digits+1)),digits)
  }
}

# round to zero
r2z <- function(x, digits = 2) {
  if (x > 0) {
    round(x-(.1^(digits+1)),digits)
  } else {
    round(x+(.1^(digits+1)),digits)
  }
}



# two decimal places for currency
f <- function(x) {
  format(x, nsmall = 2)
}
# choose a significance level
getsiglev <- function() {
  siglev <- c(.01, .05, .1)
  sample(siglev, 1)
}

# choose a confidence level
getconflev <- function() {
  conflev <- c(.9, .95, .99)
  sample(conflev, 1)
}

# find max for each pair of values in two vectors
fun1 <- function(x,y) apply(cbind(x,y), 1, max)
# make decision
decide <- function(pvalue, alpha) {
  ifelse(pvalue <= alpha, "Reject $H_0$",
         "Do not reject $H_0$")
}
