# Merges grades from ZipGrade into a .csv that can be uploaded to CourseWorks

library(tidyverse)

# download from CourseWorks (add an exam assignment)

cwfileMW <- "~/Downloads/2022-12-31T1301_Grades-STATGR5702_001_2022_3_-_EXPLORATORY_DATA_ANALYSIS_VISUAL.csv"
cwfileTTh <- "~/Downloads/2022-12-31T1300_Grades-STATGR5702_002_2022_3_-_EXPLORATORY_DATA_ANALYSIS_VISUAL.csv"

students1206 <- read_csv("~/Downloads/quiz-EDAV20221206-standard20180510.csv")
students1207 <- read_csv("~/Downloads/quiz-EDAV20221207-standard20180510.csv")
studentsA <- read_csv("~/Downloads/quiz-EDAV20221212A-standard20180510.csv")
studentsB <- read_csv("~/Downloads/quiz-EDAV20221212B-standard20180510.csv")

write_csv(rbind(students1206, students1207, studentsA, studentsB), "~/Downloads/students.csv")

zipfile <- "~/Downloads/students.csv"

# downloaded from zip grade (standard format)
Zip <- read_csv(zipfile) |>
  rename(`SIS User ID` = `External Id`) |>
  select(`SIS User ID`, `Num Correct`, Class) |>
  mutate(`Exam` = `Num Correct`) |>
  filter(!is.na(`SIS User ID`)) |>
  select(-`Num Correct`)

Zip |> filter(Class == "EDAVFall22TTh") |>
  select(-Class) |>
  write_csv("~/Downloads/studentsTTh.csv")

Zip |> filter(Class == "EDAVFall22MW") |>
  select(-Class) |>
  write_csv("~/Downloads/studentsMW.csv")


# downloaded from courseworks

cwMW <- read_csv(cwfileMW) |>
  filter(!is.na(`SIS User ID`))

zipMW <- read_csv("~/Downloads/studentsMW.csv")

cwTTh <- read_csv(cwfileTTh) |>
  filter(!is.na(`SIS User ID`))

zipTTh <- read_csv("~/Downloads/studentsTTh.csv")

# Monday / Wednesday

# join
gradesMW <-  cwMW %>% select(Student, ID, `SIS User ID`, `SIS Login ID`, Section,
                             ends_with(" Unposted Final Score")) |>
  full_join(zipMW)


# check missing

gradesMW |> filter(is.na(Exam))

# check grade distribution

colnames(gradesMW) <- str_remove_all(colnames(gradesMW), " Unposted Final Score")

gradesMW <- gradesMW |>
  mutate(pset = as.numeric(`Problem Sets`),
         cc = as.numeric(`Community Contribution`),
         exam = as.numeric(Exam)+5,
         project = as.numeric(`Final Project`),
         peer = as.numeric(`Peer Review`)) |>
  mutate(average = round(.25*pset + .1*cc + .25*exam + .35*project + .05*peer,1)) |>
  arrange(desc(average)) |>
  mutate(letter = cut(average, breaks = c(0, 60, 70, 78.5, 81.5, 86, 89.5, 92.5, 95, 101),
                                labels = c("F", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                      right=FALSE))

summary(gradesMW$letter)

write_csv(gradesMW, "~/Downloads/CheckinExcelMW.csv")


# delete columns before import

gradesMW |>
  select(`Student`, `ID`, `SIS User ID`, `SIS Login ID`, `Section`,
         `Exam (974557)` = exam) |>
  write_csv("~/Downloads/Grades2importMW.csv")

###############################################################

# Tuesday / Thursday

# join

gradesTTh <-  cwTTh %>% select(Student, ID, `SIS User ID`, `SIS Login ID`, Section,
                               ends_with(" Unposted Final Score")) |>
  full_join(zipTTh)

# check missing

gradesTTh |> filter(is.na(Exam))


# check grade distribution

colnames(gradesTTh) <- str_remove_all(colnames(gradesTTh), " Unposted Final Score")

gradesTTh <- gradesTTh |>
  mutate(pset = as.numeric(`Problem Sets`),
         cc = as.numeric(`Community Contribution`),
         exam = as.numeric(Exam)+5,
         project = as.numeric(`Final Project`),
         peer = as.numeric(`Peer Review`)) |>
  mutate(average = round(.25*pset + .1*cc + .25*exam + .35*project + .05*peer,1)) |>
  arrange(desc(average)) |>
  mutate(letter = cut(average, breaks = c(0, 60, 70, 78, 81.5, 86, 89.5, 92.5, 95, 101),
                      labels = c("F", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                      right=FALSE))

summary(gradesTTh$letter)

write_csv(gradesTTh, "~/Downloads/CheckinExcelTTh.csv")


# delete columns before import

gradesTTh |>
  select(`Student`, `ID`, `SIS User ID`, `SIS Login ID`, `Section`,
         `Exam (974558)` = exam) |>
  write_csv("~/Downloads/Grades2importTTh.csv")
