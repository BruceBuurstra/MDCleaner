library(readr)
library(tidyverse)
library(Matrix)
library(factoextra)
library(RcppML)
library(pkgdown)
library(usethis)
library(magrittr)
ratings_wide <- read_csv("ratings_wide.csv")
ratings_wide <- ratings_wide %>% select(-`userId`)

movie_titles <- read.csv("movies.csv")

# Drop the columns having x or more percentage of missing values in dataset.
drop_columns <- function(x) {
  ratings_wide <- ratings_wide %>%
    purrr::discard(~sum(is.na(.x)) / length(.x) >= x)
  assign("ratings_wide", ratings_wide, envir = .GlobalEnv)
}

# Drop  the rows having 75% or more missing values in dataset
drop_rows <- function(y) {
  rows <- unlist(rowSums(!is.na(ratings_wide)) / ncol(ratings_wide))
  bad_rows <- c()
  y <- 1 - y
  for (x in 1:length(rows)) {
    if (rows[x] < y) {
      vector <- c(x)
      bad_rows <- rbind(bad_rows, vector)
    }
    assign("bad_rows", bad_rows, envir = .GlobalEnv)
  }
  if (length(bad_rows) > 0)
    ratings_wide <- ratings_wide[-bad_rows, ]
  assign("ratings_wide", ratings_wide, envir = .GlobalEnv)
}


drop_columns(.9)
drop_rows(0.75)

# Rounding the value to movie ratings
round_to_movie_rating <- function(x) {
  if (x < 0.25) {
    x <- 0
  } else if (x < 0.75) {
    x <- 0.5
  } else if (x < 1.25) {
    x <- 1
  } else if (x < 1.75) {
    x <- 1.5
  } else if (x < 2.25) {
    x <- 2
  } else if (x < 2.75) {
    x <- 2.5
  } else if (x < 3.25) {
    x <- 3
  } else if (x < 3.75) {
    x <- 3.5
  } else if (x < 4.25) {
    x <- 4
  } else if (x < 4.75) {
    x <- 4.5
  } else {
    x <- 5
  }
}

# Reducing the dimension of dataset using nmf
run_nmf_imputation <- function(z) {
  ratings_wide_m <- ratings_wide
  ratings_wide_m[is.na(ratings_wide_m)] <- 0
  ratings_wide_m <- data.matrix(ratings_wide_m)

  nmf <- nmf(ratings_wide_m, z, mask = "zeros")

  nmf_mult <- as.matrix(nmf@w %*% Diagonal(x = nmf@d) %*% nmf@h)
  nmf_mult_round <- sapply(nmf_mult, round_to_movie_rating)
  ratings_nmf_imputed <- matrix(unlist(nmf_mult_round),
                                ncol = ncol(ratings_wide_m), byrow = FALSE)
  assign("ratings_nmf_imputed", ratings_nmf_imputed, envir = .GlobalEnv)
  assign("nmf", nmf, envir = .GlobalEnv)
}

run_nmf_imputation(3)

# Finding the optimal number of kmeans cluster
optimal_nmf_kmeans_clusters <- function() {
  movies <- t(nmf@h)
  return(fviz_nbclust(movies, kmeans, method = "silhouette", k.max = 20))
}

optimal_nmf_kmeans_clusters()

# Finding the elbow plot for kmeans clustering
get_nmf_kmeans_elbow_plot <- function() {
  movies <- t(nmf@h)
  return(fviz_nbclust(movies, kmeans, nstart = 100, method = "wss"))
}

get_nmf_kmeans_elbow_plot()

# Clustering movies using nmf and kmeans
run_nmf_movies_clustering <- function(w) {
  movies <- t(nmf@h)

  kmean <- kmeans(movies, w, nstart = 100)

  plot1 <- fviz_cluster(kmean, data = movies,
                        geom = c("point"), ellipse.type = "euclid")

  assign("kmean", kmean, envir = .GlobalEnv)
  return(plot1)
}

run_nmf_movies_clustering(3)

get_nmf_clustered_data <- function() {
  movies <- t(nmf@h)
  kmeansClusters <- factor(kmean$cluster)

  movies_final <- as.data.frame(movies) %>%
    mutate(Cluster = kmeansClusters) %>%
    mutate(movieId = row.names(movies))

  movies_final2 <- merge(movies_final, movie_titles, by = "movieId")
  movies_final2$movieId <- as.numeric(movies_final2$movieId)
  nmf_clustered_data <- movies_final2 %>%
    arrange(movieId) %>%
    select(movieId, Cluster, title, genres)
  return(nmf_clustered_data)
}

nmf_clusterd_data <- get_nmf_clustered_data()

run_knn_imputation <- function(n) {
  ratings_wide_m <- ratings_wide
  ratings_wide_m[is.na(ratings_wide_m)] <- 0
  ratings_wide_m <- data.matrix(ratings_wide_m)
  euclidean_matrix <- matrix(nrow = ncol(ratings_wide_m),
                             ncol = ncol(ratings_wide_m))

  for (i in 1:ncol(ratings_wide_m)) {
    for (j in 1:ncol(ratings_wide_m)) {
      i1 <- which(ratings_wide_m[, i] != 0)
      i2 <- which(ratings_wide_m[, j] != 0)
      indx <- intersect(i1, i2)
      int1 <- ratings_wide_m[indx, i]
      int2 <- ratings_wide_m[indx, j]
      euc <- sqrt(sum(int1 - int2)^2)
      euclidean_matrix[i, j] <- euc
      print(i)
      print(j)
    }
    assign("euclidean_matrix", euclidean_matrix, envir = .GlobalEnv)
  }

  ratings_knn_imputed <- ratings_wide_m
  for (i in 1:ncol(ratings_knn_imputed)){
    dist <- euclidean_matrix[-i, i]
    distances <- sort(dist)[1:5]
    neighbor_ind <- which(dist %in% sort(dist)[1:n])
    ret <-  list(neighbor_ind, distances)
    list <- c(unlist(ret[1]))
    print(i)
    for (j in 1:nrow(ratings_knn_imputed)) {
      if (ratings_knn_imputed[j, i] == 0) {
        knn_values <- ratings_wide_m[j, list]
        knn_non_zero <- which(knn_values != 0)
        knn_non_zero_values <- knn_values[knn_non_zero]
        if (length(knn_non_zero_values) == 0) {
          ratings_knn_imputed[j, i] <- 0
        } else {
          avg_knn_values <- sum(knn_non_zero_values) /
            length(knn_non_zero_values)
          ratings_knn_imputed[j, i] <- avg_knn_values
        }
      } else {
        print(j)
      }
    }
  }
  knn_round <- sapply(ratings_knn_imputed, round_to_movie_rating)
  ratings_knn_imputed <- matrix(unlist(knn_round),
                                ncol = ncol(ratings_wide_m), byrow = FALSE)
  assign("ratings_knn_imputed", ratings_knn_imputed, envir = .GlobalEnv)
  return(ratings_knn_imputed)
}

run_knn_imputation(5)
optimal_knn_kmeans_clusters <- function() {
  knn_movies <- t(ratings_knn_imputed)
  return(fviz_nbclust(knn_movies, kmeans, method = "silhouette", k.max = 20))
}

optimal_knn_kmeans_clusters()

get_knn_kmeans_elbow_plot <- function() {
  knn_movies <- t(ratings_knn_imputed)
  return(fviz_nbclust(knn_movies, kmeans, nstart = 100, method = "wss"))
}

get_knn_kmeans_elbow_plot()

run_knn_movies_clustering <- function(v) {
  knn_movies <- t(ratings_knn_imputed)

  knn_kmeans <- kmeans(knn_movies, v, nstart = 100)

  plot1 <- fviz_cluster(knn_kmeans, data = knn_movies,
                        geom = c("point"), ellipse.type = "euclid")

  assign("knn_kmeans", knn_kmeans, envir = .GlobalEnv)
  return(plot1)
}

run_knn_movies_clustering(2)

get_knn_clustered_data <- function() {
  movies <- t(nmf@h)
  knn_movies <- t(ratings_knn_imputed)
  knn_kmeansClusters <- factor(knn_kmeans$cluster)

  knn_movies_final <- as.data.frame(knn_movies) %>%
    mutate(Cluster = knn_kmeansClusters) %>%
    mutate(movieId = row.names(movies))

  knn_movies_final2 <- merge(knn_movies_final, movie_titles, by = "movieId")
  knn_movies_final2$movieId <- as.numeric(knn_movies_final2$movieId)
  knn_clustered_data <- knn_movies_final2 %>%
    arrange(movieId) %>%
    select(movieId, Cluster, title, genres)
  return(knn_clustered_data)
}

knn_clustered_data <- get_knn_clustered_data()
