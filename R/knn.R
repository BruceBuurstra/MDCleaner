library(readr)
library(tidyverse)

ratings <- read_csv("~/MDCleaner/ratings_wide.csv"
# ratings <- read_csv("/Users/madankc/MDCleaner/ratings_wide.csv")
ratings <- data.frame(ratings)

#Function to drop the columns having 95% or more mising values.
drop_columns <- function() {
  # dropping the columns having 95% 'NA' values using discard 
  ratings <- ratings %>% purrr::discard(~sum(is.na(.x)) / length(.x) * 100 >= 95)
  return(ratings)
}

drop_columns()
#print(ncol(drop_columns()))
