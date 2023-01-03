# Calculate final grades

library(tidyverse)

# download from CourseWorks (add an exam assignment)

cwfile <- "~/Downloads/2023-01-02T1139_Grades-STATUN1201_001_2022_3_-_CALC-BASED_INTRO_TO_STATISTICS.csv"


# downloaded from courseworks

cwcolnames <- read_csv(cwfile, n_max = 0)

cw <- read_csv(cwfile, skip = 3, col_names = FALSE)
colnames(cw) <- colnames(cwcolnames)

# select relevant columns

grades <-  cw %>% select(Student, ends_with("Unposted Final Score")) |>
  rename(Average = "Unposted Final Score") |>
  arrange(desc(Average))

colnames(grades) <- str_remove_all(colnames(grades), " Unposted Final Score")

grades <- grades |> select(-Ungraded)

# remove students who didn't take the final

grades <- grades |>
  filter(Final != 0)

grades <- grades |>
  mutate(Letter = cut(Average, breaks = c(0, 58, 60, 70, 73, 80, 84, 89, 92, 96, 100),
                      labels = c("F", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                      right=FALSE))

summary(grades$Letter)

