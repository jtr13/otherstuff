# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks
#

library(tidyverse)
CW <- read_csv("~/Downloads/2018-10-23T1153_Grades-GR5702_EDAV_Fall_2018.csv") %>%
  filter(!is.na(`SIS User ID`))

Zip <- read_csv("~/Downloads/quiz-Test1Fall2018-standard20180510.csv") %>%
  rename(`SIS User ID` = `External Id`) %>%
  select(`SIS User ID`, `Percent Correct`) %>%
  filter(!is.na(`SIS User ID`))


grades <-  CW %>% select(Student, ID, `SIS User ID`, `SIS Login ID`, Section) %>%
  full_join(Zip) %>%
  rename(`Test #1 (208846)` = `Percent Correct`)

write_csv(grades, "~/Downloads/Test1grades2import.csv")
