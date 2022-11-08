# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks

library(tidyverse)

cwfile <- "~/Downloads/2021-12-28T1231_Grades-STATGR5702_001_2021_3_-_EXPLORATORY_DATA_ANALYSIS_VISUAL.csv"


# downloaded from zip grade (Quizzes, Quiz Statistics, standard format)

studentsA <- read_csv("~/Downloads/quiz-FinalFall2020-standard20180510.csv")
#studentsB <- read_csv("~/Downloads/quiz-EDAVMidtermFall20-standard20180510.csv")

write_csv(studentsA, "~/Downloads/students.csv")
#write_csv(rbind(studentsA, studentsB), "students.csv")

zipfile <- "~/Downloads/students.csv"

# downloaded from courseworks

#ABCDEFGHIJKLMNOPQRSTUVWXYZ
#ccccccnnnnnn_nnnnnnnnn____

col_types = paste0("cccccc_nnnnnn_nnnnnnnnn____", paste(rep("_", 40), collapse = ""))

CW <- read_csv(cwfile, col_names = TRUE, col_types = col_types) %>%
  filter(!is.na(`SIS User ID`))

Zip <- read_csv(zipfile) %>%
  rename(`SIS User ID` = `External Id`) %>%
  select(`SIS User ID`, `Num Correct`) %>%
  mutate(`ZipFinal` = `Num Correct`) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(-`Num Correct`)

# Add final exam
CW <- CW %>% full_join(Zip) %>% mutate(`Final Exam (562467)`= 10 + ZipFinal*3, .keep = "unused")

# Reduce 4 midterm columns to one and 2 webcam columns to one

CW <- CW %>% unite(Midterm, contains("--"), na.rm = TRUE) %>%
  mutate(Midterm = as.numeric(Midterm)) %>%
  unite(Webcam, contains("Webcam"), na.rm = TRUE) %>%
  mutate(Webcam = as.numeric(Webcam))


# calculate averages

grades <- CW %>% transmute(Student,
                           PSets = rowMeans(across(contains("Set")))*2,
                        CC = rowSums(across(contains("Community")))*2,
                        Midterm = Midterm + Webcam,
                        across(starts_with("Final Exam"), ~.x, .names = "Final"),
                        across(starts_with("Final Project"), ~.x, .names = "Project"),
                        across(contains("Peer"), ~.x*5, .names = "Peer")) %>%
  mutate(Average = .25*PSets + .05*CC + .2*Midterm + .2*Final + .05*Peer + .25*Project)




write_csv(CW, "~/Downloads/Midtermgrades2import.csv")



Zip <- df %>%
  rename(`SIS User ID` = `External Id`) %>%
  select(`SIS User ID`, `Num Correct`) %>%
  mutate(`ZipFinal` = `Num Correct`) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(-`Num Correct`)


