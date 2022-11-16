# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

library(readr)
ratings_wide <- read_csv("ratings_wide.csv")
View(ratings_wide)


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

row_remover(0.00226)
print(nrow(ratings_wide))
print(ncol(ratings_wide))
