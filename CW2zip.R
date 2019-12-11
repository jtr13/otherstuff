# Creates a .csv file that can be imported to ZipGrade
# Click Students tab in ZipGrade to import

# Exported file from CourseWorks
 cwfile <- "~/Downloads/2019-10-20T2120_Grades-STATW5702_001_2019_3_-_EXPLORATORY_DATA_ANALYSIS_VISUAL.csv"

library(tidyverse)

CW <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(Student, `SIS User ID`) %>%
  separate(Student, into = c("Last", "First"), sep = ", ") %>%
  mutate(Class = "EDAVFall19")

write_csv(CW, "~/Downloads/StudentNames2import.csv")
