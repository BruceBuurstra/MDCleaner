library(readr)
library(tidyverse)
library(Matrix)
library(factoextra)
ratings_wide <- read_csv("ratings_wide.csv")
movie_titles <- read.csv("movies.csv")

ratings_wide <- ratings_wide %>% select(-`userId`)
#Function to drop the columns having 95% or more mising values.
drop_columns <- function() {
  # dropping the columns having 95% or more 'NA' values using discard
  ratings_wide <- ratings_wide %>% purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=90)
  #ratings <- ratings %>% purrr::discard(sum(is.na(.x))/length(.x)* 100 <=95)
  assign('ratings_wide', ratings_wide, envir = .GlobalEnv)
}

row_remover <- function(y) {
  rows <- unlist(rowSums(!is.na(ratings_wide))/ncol(ratings_wide))
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
  if (length(bad_rows)>0)
    ratings_wide <- ratings_wide[-bad_rows,]
  assign('ratings_wide', ratings_wide, envir = .GlobalEnv)
}

drop_columns()
row_remover(0.25)

#view(drop_columns())
print(nrow(ratings_wide))
print(ncol(ratings_wide))

ratings_wide_m <- ratings_wide
ratings_wide_m[is.na(ratings_wide_m)] <- 0
ratings_wide_m <- data.matrix(ratings_wide_m)

nmf <- RcppML::nmf(ratings_wide_m, 3, mask="zeros")

nmf_mult <- as.matrix(nmf@w %*% Diagonal(x = nmf@d) %*% nmf@h)
nmf_mult_round<-round(nmf_mult,0)

movies <- t(nmf@h)
fviz_nbclust(movies, kmeans, method = "silhouette", k.max=20)
fviz_nbclust(movies, kmeans, nstart=100, method = "wss") +
  geom_vline(xintercept = 2, linetype = 1)

kmean <- kmeans(movies, 3, nstart=25)
fviz_cluster(kmean, data = movies,
             palette = c("#2E9FDF", "#000000", "#FF0000"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

kmeans_2 <- kmeans(movies, 3, nstart = 100)

fviz_cluster(kmeans_2, data = movies, geom = c("point"),ellipse.type = "euclid")

kmeansClusters <- factor(kmean$cluster)

movies_final <- as.data.frame(movies) %>%
  mutate(Cluster = kmeansClusters)%>%
  mutate(movieId = row.names(movies))

movies_final2 <- merge(movies_final, movie_titles, by = "movieId")
movies_final2$movieId <- as.numeric(movies_final2$movieId)
movies_final2 <- movies_final2 %>%
  arrange(movieId)%>%
  select(movieId, Cluster, title, genres)

euclidean_matrix <- matrix(, nrow=ncol(ratings_wide_m), ncol=ncol(ratings_wide_m))
for (i in 1:ncol(ratings_wide_m)){
  for (j in 1:ncol(ratings_wide_m)){
    i1 = which(ratings_wide_m[,i]!=0)
    i2 = which(ratings_wide_m[,j]!=0)
    indx = intersect(i1, i2)
    int1 = ratings_wide_m[indx,i]
    int2 = ratings_wide_m[indx,j]
    euc = sqrt(sum(int1 - int2)^2)
    euclidean_matrix[i,j] <- euc
    print(i)
    print(j)
  }
  assign('euclidean_matrix', euclidean_matrix, envir = .GlobalEnv)
}
