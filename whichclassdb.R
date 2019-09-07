# create which class database
#
library(tidyverse)

path <- "~/Documents/Students"

files <- dir(path = path, pattern = "Fall_2018")
students <- map_df(files, ~read_csv(file.path(path, .x)))

