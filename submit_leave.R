library(rcanvas)
library(tidyverse)
library(lubridate)
# set_canvas_token(...) once -- see help

# Get CourseWorks submit time
# Takes a long time but manually downloading gradebook
# doesn't have submit times
set_canvas_domain("https://courseworks2.columbia.edu/")
# Spring 2021 Stat 1201 Mon/Wed 124310 Tues/Thurs 124312
# gradebook <- get_course_gradebook(124312)
# saveRDS(gradebook, file="~/Downloads/gradebookTTh.rds")
gradebook <- readRDS("~/Downloads/gradebookTTh.rds")
test1 <- gradebook %>%
  filter(assignment_name == "Test 1") %>%
  select(name = user.name, attachments) %>%
  mutate(submit_time = map(attachments, ~.x[["created_at"]]),
         .keep = "unused") %>%
  mutate(submit_time = with_tz(ymd_hms(submit_time), "EST"))

# Get Zoom info: Reports, Usage, Leave "unique users" unchecked

zoom <- read_csv("~/Downloads/participants.csv") %>%
  mutate(name = `Name (Original Name)`,
         leave_time = ymd_hms(`Leave Time`, tz = "EST"))

leave <- zoom %>% group_by(name) %>%
  summarize(leave_time = max(leave_time))

test1 <- left_join(test1, leave) %>%
  mutate(diff = leave_time - submit_time)

# NOTE: CourseWorks is for latest submission only

test1 %>% filter(diff < 0) %>%
  mutate(minutes = round(as.numeric(diff)/60, 1)) %>%
  select(name, minutes, everything()) %>%
  arrange(minutes)




