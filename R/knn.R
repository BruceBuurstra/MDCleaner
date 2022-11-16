library(readr)
library(tidyverse)
ratings_wide <- read_csv("~/MDCleaner/ratings_wide.csv")

#Function to drop the columns having 95% or more mising values.
drop_columns <- function() {
  # dropping the columns having 95% or more 'NA' values using discard 
  ratings_wide <- ratings_wide %>% purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=97.5)
  #ratings <- ratings %>% purrr::discard(sum(is.na(.x))/length(.x)* 100 <=95)
  assign('ratings_wide', ratings_wide, envir = .GlobalEnv)
}

drop_columns()

#view(drop_columns())
print(ncol(drop_columns()))
