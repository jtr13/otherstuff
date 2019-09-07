# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks

cwfile <- "~/Downloads/2019-05-17T1440_Grades-STATGR5293_002_2019_1_-_TOPICS_IN_MODERN_STATISTICS.csv"

zipfile <- "~/Downloads/quiz-Test2-standard20180510.csv"

library(tidyverse)
# downloaded from courseworks

CW <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`))

# downloaded from zip grade (standard format)
Zip <- read_csv(zipfile) %>%
  rename(`SIS User ID` = `External Id`) %>%
  select(`SIS User ID`, `Num Correct`) %>% # ***
  mutate(`Test 2` = `Num Correct` + 10) %>% # ***
  filter(!is.na(`SIS User ID`)) %>%
  select(-`Num Correct`)

# change test column
grades <-  CW %>% select(Student, ID, `SIS User ID`, `SIS Login ID`, Section) %>%
  full_join(Zip)

# change file name
write_csv(grades, "~/Downloads/Test2grades2import.csv")
