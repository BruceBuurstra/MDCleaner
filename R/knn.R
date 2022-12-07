#library(readr)
#library(tidyverse)
#library(Matrix)
#library(factoextra)
#library(RcppML)
#library(pkgdown)
#library(usethis)
#library(magrittr)
#ratings_wide <- read_csv("MDCleaner/ratings_wide.csv")
#ratings_wide <- ratings_wide %>% select(-`userId`)

#movie_titles <- read.csv("MDCleaner/movies.csv")

# Drop the columns having x or more percentage of missing values in dataset.
drop_columns <- function(x, data) {
  data <- data %>%
    purrr::discard(~sum(is.na(.x)) / length(.x) >= x)
  return(data)
}

# Drop  the rows having 75% or more missing values in dataset
drop_rows <- function(y, data) {
  rows <- unlist(rowSums(!is.na(data)) / ncol(data))
  bad_rows <- c()
  y <- 1 - y
  for (x in 1:length(rows)) {
    if (rows[x] < y) {
      vector <- c(x)
      bad_rows <- rbind(bad_rows, vector)
    }
  }
  if (length(bad_rows) > 0)
    data <- data[-bad_rows, ]
  return(data)
}

#columns_dropped <- drop_columns(0.9, ratings_wide)
#rows_dropped <- drop_rows(0.75, ratings_wide)

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
run_nmf_imputation <- function(z, data) {
  data_matrix <- data
  data_matrix[is.na(data_matrix)] <- 0
  data_matrix <- data.matrix(data_matrix)

  nmf <- nmf(data_matrix, z, mask = "zeros")

  data_nmf_imputed <- as.matrix(nmf@w %*% Diagonal(x = nmf@d) %*% nmf@h)
  assign("nmf", nmf, envir = .GlobalEnv)
  return(data_nmf_imputed)
}

#nmf_imputed <- run_nmf_imputation(3, rows_dropped)
#nmf_mult_round <- sapply(nmf_imputed, round_to_movie_rating)
#nmf_mult_round <- matrix(unlist(nmf_mult_round),
#                           ncol = ncol(ratings_wide_m), byrow = FALSE)

# Finding the optimal number of kmeans cluster
optimal_nmf_kmeans_clusters <- function(nmf) {
  movies <- t(nmf@h)
  return(fviz_nbclust(movies, kmeans, method = "silhouette", k.max = 20))
}

#optimal_nmf_kmeans_clusters(nmf)

# Finding the elbow plot for kmeans clustering
get_nmf_kmeans_elbow_plot <- function(nmf) {
  movies <- t(nmf@h)
  return(fviz_nbclust(movies, kmeans, nstart = 100, method = "wss"))
}

#get_nmf_kmeans_elbow_plot(nmf)

# Clustering movies using nmf and kmeans
run_nmf_movies_clustering <- function(w, nmf) {
  movies <- t(nmf@h)

  kmean <- kmeans(movies, w, nstart = 100)

  plot1 <- fviz_cluster(kmean, data = movies,
                        geom = c("point"), ellipse.type = "euclid")

  assign("kmean", kmean, envir = .GlobalEnv)
  return(plot1)
}

#run_nmf_movies_clustering(3, nmf)

get_nmf_clustered_data <- function(nmf, kmean) {
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

#nmf_clusterd_data <- get_nmf_clustered_data(nmf, kmean)

run_knn_imputation <- function(n, data) {
  data_matrix <- data
  data_matrix[is.na(data_matrix)] <- 0
  data_matrix <- data.matrix(data_matrix)
  euclidean_matrix <- matrix(nrow = ncol(data_matrix),
                             ncol = ncol(data_matrix))

  for (i in 1:ncol(data_matrix)) {
    for (j in 1:ncol(data_matrix)) {
      i1 <- which(data_matrix[, i] != 0)
      i2 <- which(data_matrix[, j] != 0)
      indx <- intersect(i1, i2)
      int1 <- data_matrix[indx, i]
      int2 <- data_matrix[indx, j]
      euc <- sqrt(sum(int1 - int2)^2)
      euclidean_matrix[i, j] <- euc
      print(i)
      print(j)
    }
    assign("euclidean_matrix", euclidean_matrix, envir = .GlobalEnv)
  }

  knn_imputed <- data_matrix
  for (i in 1:ncol(ratings_knn_imputed)){
    dist <- euclidean_matrix[-i,i]
    distances <- sort(dist)[1:n]
    neighbor_ind = which(dist %in% sort(dist)[1:n])
    ret <-  list(neighbor_ind, distances)
    list <- c(unlist(ret[1]))
    print(i)
    for (j in 1:nrow(knn_imputed)) {
      if (knn_imputed[j, i] == 0) {
        knn_values <- data_matrix[j, list]
        knn_non_zero <- which(knn_values != 0)
        knn_non_zero_values <- knn_values[knn_non_zero]
        if (length(knn_non_zero_values) == 0) {
          knn_imputed[j, i] <- 0
        } else {
          avg_knn_values <- sum(knn_non_zero_values) /
            length(knn_non_zero_values)
          knn_imputed[j, i] <- avg_knn_values
        }
      } else {
        print(j)
      }
    }
  }
  assign("knn_imputed", ratings_knn_imputed, envir = .GlobalEnv)
  return(knn_imputed)
}

#run_knn_imputation(5, rows_dropped)
#knn_round <- sapply(knn_imputed, round_to_movie_rating)
#ratings_knn_imputed <- matrix(unlist(knn_round),
 #                             ncol = ncol(ratings_wide_m), byrow = FALSE)

optimal_knn_kmeans_clusters <- function(data) {
  knn_movies <- t(data)
  return(fviz_nbclust(knn_movies, kmeans, method = "silhouette", k.max = 20))
}

#optimal_knn_kmeans_clusters(ratings_knn_imputed)

get_knn_kmeans_elbow_plot <- function(data) {
  knn_movies <- t(ratings_knn_imputed)
  return(fviz_nbclust(knn_movies, kmeans, nstart = 100, method = "wss"))
}

#get_knn_kmeans_elbow_plot(ratings_knn_imputed)

run_knn_movies_clustering <- function(v, data) {
  knn_movies <- t(data)

  knn_kmeans <- kmeans(knn_movies, v, nstart = 100)

  plot1 <- fviz_cluster(knn_kmeans, data = knn_movies,
                        geom = c("point"), ellipse.type = "euclid")

  assign("knn_kmeans", knn_kmeans, envir = .GlobalEnv)
  return(plot1)
}

#run_knn_movies_clustering(2, ratings_knn_imputed)

get_knn_clustered_data <- function(original, data){
  movies <- t(original)
  knn_movies <- t(data)
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

#knn_clustered_data <- get_knn_clustered_data(rows_dropped, ratings_knn_imputed)

