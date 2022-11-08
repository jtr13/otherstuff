# Creates a .csv file that can be imported to ZipGrade
# Click Students tab in ZipGrade to import

# Exported file from CourseWorks
 cwfile <- "~/Downloads/2022-04-27T1551_Grades-STATGR5293_004_2022_1_-_TOPICS_IN_MODERN_STATISTICS.csv"
 class <- "5293Spring22"

library(tidyverse)

CW <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(Student, `SIS User ID`) %>%
  separate(Student, into = c("Last", "First"), sep = ", ") %>%
  mutate(Class = class)

write_csv(CW, "~/Downloads/StudentNames2import.csv")

