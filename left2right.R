#' Evaluate expressions left to right
#'
#' It's very primitive:
#'  - can't handle negative numbers
#'  - no error checking
#'
#' @param s A string containing numbers and operations (+, -, *, /, ^)
#'
#' @examples
#' e("3/4+.25*3")
#' e("1+2*3/4")

nextcalc <- function(oldanswer, nextop, nextnum) {
  switch(nextop,
         "*" = newanswer <- oldanswer * nextnum,
         "+" = newanswer <- oldanswer + nextnum,
         "-" = newanswer <- oldanswer - nextnum,
         "^" = newanswer <- oldanswer ^ nextnum,
         "/" = newanswer <- oldanswer / nextnum)
  newanswer
}

e <- function(s) {
# remove spaces
s <- gsub(" ", "", s)

# get numbers
numbers <- as.numeric(strsplit(s, "\\+|-|\\*|/|\\^")[[1]])

# get operations
ops <- strsplit(s, "[0-9]*\\.?[0-9]+")[[1]]

# start with the first number
answer <- numbers[1]

# take next operation and number and perform the calculation. repeat
for (i in 2:length(numbers)) {
  answer <- nextcalc(answer, ops[i], numbers[i])
}

answer
}
