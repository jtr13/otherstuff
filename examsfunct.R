# useful functions for exams package
# round 5 away from zero
r <- function(x, digits = 2) {
  if (x > 0) {
    round(x+(.1^(digits+1)),digits)
    } else {
    round(x-(.1^(digits+1)),digits)
  }
}

# round to zero
# .546 --> .54
# -.546 --> -.54
r2z <- function(x, digits = 2) {
  if (x > 0) {
    floor(x*10^digits)/10^digits
  } else {
    ceiling(x*10^digits)/10^digits
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

# determine p-value
pval <- function(xbar,
                 type = "twosided",
                 test = "z",
                 df = NULL) {
  if (test == "z") {
    if (type == "lower") {
      pnorm(xbar)
    } else if (type == "upper") {
      1 - pnorm(xbar)
    } else {
      2 * (1 - pnorm(abs(xbar)))
    }
  } else {
    if (is.null(df)) stop("Degrees of freedom must be specified.")
    if (type == "lower") {
      pt(xbar, df)
    } else if (type == "upper") {
      1 - pt(xbar, df)
    } else {
      2 * (1 - pt(abs(xbar), df))
    }
  }
}



# find max for each pair of values in two vectors
fun1 <- function(x,y) apply(cbind(x,y), 1, max)

# make decision
decide <- function(pvalue, alpha) {
  ifelse(pvalue <= alpha, "Reject $H_0$",
         "Do not reject $H_0$")
}
