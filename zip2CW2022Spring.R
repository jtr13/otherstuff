# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks

library(tidyverse)

# downloaded from courseworks

cwfile <- "~/Downloads/2022-05-18T1838_Grades-STATGR5293_004_2022_1_-_TOPICS_IN_MODERN_STATISTICS.csv"

# downloaded from zip grade (Quizzes, Quiz Statistics, standard format)

zipfile <- read_csv("~/Downloads/quiz-5293Spring22Final-standard20180510.csv")



CW <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`))

Zip <- zipfile %>%
  rename(`SIS User ID` = `External Id`) |>
  select(`SIS User ID`, Final = `Percent Correct`) |>
  filter(!is.na(`SIS User ID`))

# change test column
 grades <-  CW |> select(Student, ID, `SIS User ID`, `SIS Login ID`, Section) |>
  full_join(Zip)

# change file name
write_csv(grades, "~/Downloads/grades2import.csv")
