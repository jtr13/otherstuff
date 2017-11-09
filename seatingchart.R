#+ echo = FALSE, message = FALSE
library(tidyverse)
df <- read_csv("~/Downloads/Students.csv", col_types = cols_only(Student = "c")) %>%
  filter(Student != "Points Possible") %>%
  filter(Student != "Student, Test") %>% arrange(Student)

test_rows <- 8
exams_per_row <- ceiling(nrow(df)/test_rows)

df$row <- sample(rep(1:test_rows, exams_per_row), nrow(df))
print("Seating Chart")
knitr::kable(df)
