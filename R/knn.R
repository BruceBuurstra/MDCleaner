library(readr)
library(tidyverse)
ratings_wide <- read_csv("ratings_wide.csv")

#Function to drop the columns having 95% or more mising values.
drop_columns <- function() {
  # dropping the columns having 95% or more 'NA' values using discard 
  ratings_wide <- ratings_wide %>% purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=97.5)
  #ratings <- ratings %>% purrr::discard(sum(is.na(.x))/length(.x)* 100 <=95)
  assign('ratings_wide', ratings_wide, envir = .GlobalEnv)
}

row_remover <- function(y) {
rows <- unlist(rowSums(!is.na(ratings_wide))/9725)
bad_rows <- c()
for (x in 1:length(rows)) {
  if (rows[x]<y){
    print(x)
    vector <- c(x)
    bad_rows <- rbind(bad_rows, vector)
    print("row added")
  }
  assign('bad_rows', bad_rows, envir = .GlobalEnv)
}
ratings_wide <- ratings_wide[-bad_rows,]
assign('ratings_wide', ratings_wide, envir = .GlobalEnv)
}

drop_columns()
row_remover(0.00226)

#view(drop_columns())
print(nrow(ratings_wide))
print(ncol(ratings_wide))
