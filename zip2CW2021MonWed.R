# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks

library(tidyverse)

cwfile <- "~/Downloads/2021-12-29T1921_Grades-STATW5702_002_2021_3_-_EXPLORATORY_DATA_ANALYSIS_VISUAL.csv"


# downloaded from zip grade (Quizzes, Quiz Statistics, standard format)

# downloaded from courseworks

#ABCDEFGHIJKLMNOPQRSTUVWXYZ
#ccccccnnnnnn_nnnnnnnnn____

col_types = paste0("cccccc_nnnnnn_nnnnnnnnn____", paste(rep("_", 40), collapse = ""))

CW <- read_csv(cwfile, col_names = TRUE) %>%
  filter(!is.na(`SIS User ID`))

inperson <- read_csv("~/Downloads/quiz-EDAVFall21MonA-standard20180510.csv") %>%
  rename(`SIS User ID` = `External Id`) %>%
  select(`SIS User ID`, `Num Correct`) %>%
  mutate(`ZipFinal` = `Num Correct`) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(-`Num Correct`)

# Add final exam
CW <- CW %>% full_join(Zip) |> mutate(`Final Exam (766248)` = ZipFinal) |> select(-ZipFinal)

# calculate averages

grades <- CW %>% transmute(Student,
                           PSets = rowMeans(across(contains("Set")))*2,
                        CC = rowSums(across(contains("Community")))*2,
                        Midterm = Midterm + Webcam,
                        across(starts_with("Final Exam"), ~.x, .names = "Final"),
                        across(starts_with("Final Project"), ~.x, .names = "Project"),
                        across(contains("Peer"), ~.x*5, .names = "Peer")) %>%
  mutate(Average = .25*PSets + .05*CC + .2*Midterm + .2*Final + .05*Peer + .25*Project)




write_csv(CW, "~/Downloads/TuesThursgrades2import.csv")




