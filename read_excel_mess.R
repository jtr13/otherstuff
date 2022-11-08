#' Reads an Excel file with multiple, merged headers and returns a tibble with unique column names
#' 
#' 
#' 
#' 
library(tidyverse)
df <- readxl::read_excel("~/Downloads/exh1.xlsx", n_max = 20)

# find first complete row
first_data_row <- min(which(complete.cases(df)))

h <- df[1:(first_data_row-1),]

# eliminate all rows with zero or one values
h <- h %>%
  slice(-which(rowSums(is.na(h)) >= ncol(h) - 1))

# fill right

h <- h %>%
  rownames_to_column("id") %>%
  pivot_longer(-id) %>%
  fill(value)%>%
  pivot_wider() %>% 
  select(-id)

# create unique column names working back from last header row
n <- nrow(h)
column_names <- as.character(h[n,])

while(anyDuplicated(column_names)) {
  column_names <- paste(as.character(h[n-1,]), column_names)
  n <- n - 1
}

column_names

# now read in the data for real

readxl::read_excel("~/Downloads/exh1.xlsx",
                         skip = first_data_row - 1,
                         col_names = FALSE) %>%
  set_names(column_names)
