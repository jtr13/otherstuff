# read a messy Excel file

library(tidyverse)
df <- readxl::read_excel("~/Downloads/exh1.xlsx", n_max = 10)

# skip 1 cell rows
df <- readxl::read_excel("~/Downloads/exh1.xlsx", skip = 3)

View(df)

# "fill right"

df[1:2,] <- df[1:2,] %>% 
  rownames_to_column("id") %>%
  pivot_longer(-id) %>%
  fill(value)%>%
  pivot_wider() %>% 
  select(-id)

# create unique column names 

column_names <- paste(as.character(df[1,]), as.character(df[2,]))

# now read in the data for real

df2 <- readxl::read_excel("~/Downloads/exh1.xlsx",
                   skip = 5,
                   col_names = FALSE) %>%
  set_names(column_names)


##################################################################


column_names <- c("Period Imports", "Balance Total", "Balance Goods (1)", "Balance Services", 
                  "Exports Total", "Exports Goods (1)", "Exports Services", "Imports Total", 
                  "Imports Goods (1)", "Imports Services")

df2 <- readxl::read_excel("~/Downloads/exh1.xlsx",
                          skip = 5,
                          col_names = FALSE) %>%
  set_names(column_names)

