library(tidyverse)


files <- read_rds("data_clean/names_of_files.rds")

states <- files[1:24]
details <- files[25:26]

data_states <- map(states, function(x) paste("data_clean/",x,".csv", sep = "")) %>%
  map(., read_csv) 

data_details <- map(details, function(x) paste("data_clean/",x,".csv", sep = "")) %>%
  map(., read_csv) 

names(data_states)  <- states
names(data_details) <- details


