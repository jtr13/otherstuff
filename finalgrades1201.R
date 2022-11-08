# Calculates final grades
# Can't do it in CourseWorks until grades are released
# Also serves as a check on CW
#
#
# Fall 2021

library(tidyverse)
CW <- read_csv("~/Downloads/CW1201.csv") |>
  select(Student, ID, `SIS User ID`, `SIS Login ID`, Section)

Drive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1XpcPvhDz1HgeHZQ_PP2unE9fyJXsR0snN61_uERnd-k/edit#gid=0") |>
  select(Student, ID, `SIS User ID`, `Test 1 (727702)`, `Test 2`, `Final`)

grades <- full_join(CW, Drive)

grades |>
  write_csv("~/Downloads/gradestoimport1201.csv")


missing <- function(test, final, test_value = .2, final_value = .35) {
  new_test_value <- (test_value/(test_value + final_value))*(2*test_value + final_value)
  new_final_value <- (2*test_value + final_value) - new_test_value
  ((new_test_value * test + new_final_value * final) - (test_value * test) - (final_value * final))/test_value
}


# Spring 2021

library(tidyverse)

reweight <- function(test, final, test_perc = .2, final_perc = .35) {
  ratio <- final_perc/test_perc
  final_avg <- (ratio*final + test)/(ratio + 1)
  names(final_avg) <- c("Grade to Enter for Missing Test")
  final_avg
}

# Exported file from CourseWorks
#
readdir <- "~/Downloads"
savedir <- "~/Dropbox/Columbia Stats/UN1201/Stat1201_2021Spring"
choices <- c("1201_002", "1201_003", "EDAV")

section <- menu(choices = choices)

file_matches <- dir(readdir, pattern = choices[section])
info <- file.info(file.path(readdir, file_matches))
cwfile <- file.path(readdir, file_matches[which.max(info$mtime)])

df <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(Student, `SIS User ID`, matches("[0-9]")) %>%
  select(-matches("(Late|Zoom| R )")) %>%
  select(-starts_with("Problem Set #6"))

# Check for missing values
skim_results <- skimr::skim(df)
sum(skim_results$n_missing)
skim_results %>% filter(n_missing > 0) %>% select(skim_variable, n_missing)

# Remove assignment numbers

colnames(df) <- str_remove_all(colnames(df), " \\([0-9]{6}\\)")


# Manually fix missing grades
# df$`Class Participation 3 -- Group 1`[is.na(df$`Class Participation 3 -- Group 1`)] <- 10
# df$`Test 2`[is.na(df$`Test 2`)] <- 60

# Get other scores for students with missing tests
#
# df %>% filter(is.na(`Test 1`)) %>% select(c("Student", "Test 2", "Final Exam"))
# df %>% filter(is.na(`Test 2`)) %>% select(c("Student", "Test 1", "Final Exam"))


# example: reweight(74, 84, .2, .35)


# Section grades

tidy_df <- df %>%
  pivot_longer(cols = starts_with("Problem"),
               names_to = "pset_num",
               names_prefix = "Problem Set #",
               values_to = "pset_score") %>%
  filter(pset_score != "EX" | is.na(pset_score)) %>%
  pivot_longer(cols = starts_with("Final"),
               names_to = "final_type",
               values_to = "final") %>%
  filter(final != "N/A" | is.na(final)) %>%
  select(-final_type) %>%
  pivot_longer(cols = starts_with("Class Participation"),
               names_to = "class_part_name",
               names_prefix = "Class Participation ",
               values_to = "class_score") %>%
  filter(class_score != "N/A" | is.na(class_score)) %>%
  filter(class_score != "EX" | is.na(class_score)) %>%
  pivot_longer(cols = starts_with("Test 2"),
               names_to = "test_2_type",
               values_to = "test2") %>%
  filter(test2 != "N/A" | is.na(test2)) %>%
  select(-test_2_type) %>%
  rename(test1 = "Test 1")


grades <- tidy_df %>%
  mutate(across(.cols = c(test1, test2, final, pset_score, class_score),
                .fns = as.numeric)) %>%
  group_by(Student, `SIS User ID`, test1, test2, final) %>%
  summarize(pset = mean(as.numeric(pset_score)),
            class = mean(as.numeric(class_score))) %>%
  mutate(average = round((.2*test1 + .2*test2 + .05*(class*10) + .35*final + .2*(pset*5)), 2)) %>%
  arrange(desc(average)) %>%
  mutate(section = choices[section])

filename <- paste0(choices[section], ".csv")

write_csv(grades, file.path(savedir, filename))

f1 <- file.path(savedir, paste0(choices[1], ".csv"))
f2 <- file.path(savedir, paste0(choices[2], ".csv"))
all <- bind_rows(read_csv(f1), read_csv(f2))
write_csv(all, file.path(savedir, "all.csv"))


