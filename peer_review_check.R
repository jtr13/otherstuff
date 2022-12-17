# check that students aren't assigned a member of their own group
# for final project peer review

# Go to https://courseworks2.columbia.edu/courses/159579/assignments/938951/peer_reviews
# Copy and paste into Excel, add header column, save as .csv

library(tidyverse)
df <- read.csv("~/Downloads/peer.csv")
df <- df |>
  filter(substr(Name, 1, 4) != "Give") |>
  filter(substr(Name, 1, 6) != "Remind") |>
  mutate(column = ifelse(str_detect(Name, "Assessment|None"), "Assignee", "Name"))
df <- df |> mutate(id = rep(1:(nrow(df)/2), each = 2)) |>
  pivot_wider(names_from = column, values_from = Name) |>
  mutate(Assignee = str_remove(Assignee, "Assessment not yet Completed")) |>
  mutate(Assignee = trimws(Assignee)) |>
  mutate(Assignee = str_replace(Assignee, "[^[:alnum:] ]", ""))

# Get project groups: People, Final Project, Import, Download

g <- read.csv("~/Downloads/Final Project1.csv") |>
  select(name, group_name)

g2 <- g |>
  group_by(group_name) |>
  summarize(allnames = paste(name, collapse = " / "))

find_partners <- function(name, g2) {
  g2$allnames[str_detect(g2$allnames, name)]
}

df$Group <- sapply(df$Name, find_partners, g2 = g2)

df$Problem <- ifelse(str_detect(df$Group, df$Assignee), TRUE, FALSE)

write.csv(df, "~/Downloads/peerreviews.csv", row.names = FALSE)

