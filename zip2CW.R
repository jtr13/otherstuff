# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks

library(tidyverse)

cwfile <- "~/Downloads/2019-10-30T2203_Grades-STATW5702_001_2019_3_-_EXPLORATORY_DATA_ANALYSIS_VISUAL.csv"

studentsA <- read_csv("quiz-MidtermAFall2019-standard20180510.csv")
studentsB <- read_csv("quiz-MidtermBFall2019-standard20180510.csv")

write_csv(rbind(studentsA, studentsB), "students.csv")

zipfile <- "students.csv"

# downloaded from courseworks

CW <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`))

# downloaded from zip grade (standard format)
Zip <- read_csv(zipfile) %>%
  rename(`SIS User ID` = `External Id`) %>%
  select(`SIS User ID`, `Num Correct`) %>% # ***
  mutate(`Midterm` = `Num Correct` + 1) %>% # ***
  filter(!is.na(`SIS User ID`)) %>%
  select(-`Num Correct`)

# change test column
 grades <-  CW %>% select(Student, ID, `SIS User ID`, `SIS Login ID`, Section) %>%
  full_join(Zip)

# change file name
write_csv(grades, "~/Downloads/Midtermgrades2import.csv")
