# Download grades to assign letter grades manually


library(tidyverse)

# Exported file from CourseWorks
#
readdir <- "~/Downloads"
savedir <- "~/Dropbox/Columbia Stats/GR5702/EDAV_2021Spring5702"

file_matches <- dir(readdir, pattern = "5702")
info <- file.info(file.path(readdir, file_matches))
cwfile <- file.path(readdir, file_matches[which.max(info$mtime)])

df <- read_csv(cwfile) %>%
  filter(!is.na(`SIS User ID`)) %>%
  select(Student, `SIS User ID`, matches("[0-9]{6}"), `Final Score`) %>%
  select(-starts_with("Boxplot")) %>%
  arrange(desc(`Final Score`))

# Check for missing values
skim_results <- skimr::skim(df)
sum(skim_results$n_missing)
skim_results %>% filter(n_missing > 0) %>% select(skim_variable, n_missing)

# Remove assignment numbers

colnames(df) <- str_remove_all(colnames(df), " \\([0-9]{6}\\)")

write_csv(df, file.path(savedir, "grades.csv"))
