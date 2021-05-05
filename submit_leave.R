
library(tidyverse)
library(lubridate)


# Get CourseWorks submit time
# Takes a long time but manually downloading gradebook
# doesn't have submit times

# Spring 2021 Stat 1201 Mon/Wed 124310 Tues/Thurs 124312
# This is slow: uncomment and read once, in the meantime, get Zoom info (see below)

# set_canvas_token(...) once -- see help -- leave commented

# library(rcanvas)
# set_canvas_domain("https://courseworks2.columbia.edu/")
# gradebook <- get_course_gradebook(124310)
# saveRDS(gradebook, file="~/Downloads/gradebookMW.rds")
gradebook <- readRDS("~/Downloads/gradebookMW.rds")
test <- gradebook %>%
  filter(assignment_name == "Test 2") %>%
  select(name = user.name, attachments) %>%
  mutate(submit_time = map(attachments, ~.x[["created_at"]]),
         .keep = "unused") %>%
  mutate(submit_time = ymd_hms(submit_time, tz = "America/New_York"))

# Get Zoom info: Reports, Usage, Leave "unique users" unchecked
# Adjust tz if necessary (EST EDT)

zoom <- read_csv("~/Downloads/participantsWed.csv") %>%
  mutate(name = `Name (Original Name)`,
         leave_time = ymd_hms(`Leave Time`, tz = "America/New_York"))

leave <- zoom %>% group_by(name) %>%
  summarize(leave_time = max(leave_time))

test <- left_join(test, leave) %>%
  mutate(diff = leave_time - submit_time)

# NOTE: CourseWorks is for latest submission only

test %>% filter(diff < 0) %>%
  mutate(minutes = round(as.numeric(diff)/60, 1)) %>%
  select(name, minutes, everything(), -diff) %>%
  arrange(minutes) %>%
  knitr::kable()




