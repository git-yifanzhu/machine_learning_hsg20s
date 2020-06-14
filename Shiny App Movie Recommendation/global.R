# Machine Learning
# University of St. Gallen
# May 2020
# Movie Recommendation Engine
# Yifan, Daniel, Nils, Jan

# 
# This file deals with the recommendation system.

# ==============================
# RECOMMENDATION ENGINE !!!
# ==============================

load("./data/movies.RData")
load("./data/tfidf.RData")
load("./data/row.RData")
load("./data/col.RData")



# Define public function to be used in this part

get_movie_data <- function(movie_title, invert=F){
  if (invert==F) {return(subset(movies_data, title == movie_title))
  } else {return(movies_data[-which(movies_data$title==movie_title), ])}
}

# ------------------------------
# 1 Best-Rated Movies
# ------------------------------

# Retrieve the best-rated movies
get_best_rated_movies <- function(num=10){
  setorder(movies_data, -weighted_rating)
  return(movies_data[1:num, 
                     c("title", "weighted_rating")])
}

# ------------------------------
# 2 Popular Movies
# ------------------------------

# Retrieve the most popular movies
get_popular_movies <- function(num=10){
  setorder(movies_data, -popularity)
  return(movies_data[1:num, 
                     c("title", "popularity")])
}

# ------------------------------
# 4 k-NN for Genres, Keywords, Director, Cast
# ------------------------------

# Calculate the similarity score of a feature between object and target
get_similarity <- function(obj_feature, target_feature){
  obj_feature <- unlist(obj_feature)
  target_feature <- unlist(target_feature)
  
  similarity_unit <- 1 / (length(obj_feature) * length(target_feature))
  
  similarity <- 0
  
  if (!is.null(obj_feature) && !is.null(target_feature)) {
  for (i in 1:length(obj_feature)) {
    for (j in 1:length(target_feature)) {
      if (obj_feature[i] == target_feature[j]) {
        similarity <- similarity + similarity_unit
      }
    }
  }
  }
  return(similarity)
}

# Retrieve the most similar movies based on genres, keywords, director, and cast
get_recommend_kNN <- function(obj_title, num, weights){
  
  obj <- get_movie_data(obj_title)
  target <- get_movie_data(obj_title, T)
  
  similarity_scores <- vector()
  
  if (!is.null(nrow(target))){
    for (i in 1:nrow(target)) {
      
      similarity = sum(
        c(get_similarity(obj$genres, target[i, ]$genres), 
          get_similarity(obj$keywords, target[i, ]$keywords), 
          get_similarity(obj$director, target[i, ]$director),
          get_similarity(obj$cast, target[i, ]$cast)) * 
          weights)
      similarity_scores <- c(similarity_scores, similarity)
      
    }
  }

  if (nrow(target)!=0){
    target = target %>% mutate(similarity_score = similarity_scores) %>% arrange(desc(similarity_score))
    
    # target[, similarity_score := similarity_scores]
    # setorder(target, -similarity_score)
    
    return(target[1:num,
                  c("title", "genres", "keywords", "director", "cast","id")])
    
  }
    
  # }else{
  #   target = data.frame(matrix(ncol = 6, nrow = 0))
  #   colnames(target) <- c("title", "genres", "keywords", "director", "cast","id")
  # }

  

}

# ------------------------------
# 5 Plot Based Recommender
# ------------------------------
# Retrieve the most similar movies based on plots
get_recommend_plot <- function(obj_title, num=10){
  
  obj_id <- get_movie_data(obj_title)$id
  obj_idx <- which(row_idx==obj_id)

  obj_cos_sim <- cos_sim_mat[, obj_idx]

  cos_sim <- cbind(row_idx, obj_cos_sim)
  cos_sim <- cos_sim[sort(obj_cos_sim, decreasing=T, index.return=T)$ix, ]

  if(length(cos_sim)!=0){
    target_ids <- cos_sim[2:(num+1), "row_idx"]

    targets <- data.frame()
    for (target_id in target_ids){
      target <- subset(movies_data, id == target_id) %>% select("title", "genres", "director", "cast","id")
      targets <- rbind(targets, target)
    }
  } else{
    targets = data.frame()
  }
  return(targets)
}



# ------------------------------
# Logistic Regression
# ------------------------------
get_recommend_logistic <- function(user_id, num=10){
  user_rating <- subset(ratings_data, userId == user_id)
  user_rating$likes <- user_rating$rating==5
  
  user_rating <- merge(user_rating, movies_data, by.x="movieId", by.y="id")
  
  logistic_mod <- glm(likes ~ popularity + 
                        revenue + runtime + vote_average + 
                        year, 
                      data=user_rating)
  
  target_ids <- dplyr::setdiff(movies_data$id, user_rating$movieId)
  target <- movies_data[c(id) %in% target_ids, ]
  

  target$likes <- stats::predict(logistic_mod, newdata=target)
  
  target$likes <- round(target$likes, digits = 4)
  # setorder(target, -likes)

  # target[with(target,order(-likes)), ]
  target = target %>% arrange(desc(likes))
  
  
  
  return(target[1:num,
                c("title", "likes", "genres", "year", "director", "cast")])
}


# ------------------------------
# 6 Collaborative Filtering Based on User Ratings
# ------------------------------

get_recommend_user <- function(user_id, num=10){
  # Constract user-movie matrix where entries are users' ratings
  row_idx <- sort(unique(ratings_data$userId))
  col_idx <- sort(unique(ratings_data$movieId))

  rating_mat_raw <- matrix(nrow=length(row_idx), ncol=length(col_idx))

  for (i in 1:nrow(ratings_data)) {
        row <- which(row_idx==ratings_data[i, ]$userId)
        col <- which(col_idx==ratings_data[i, ]$movieId)
        rating_mat_raw[row, col] <- ratings_data[i, ]$rating
      }

  # Replace NA with column mean (movies average)
  rating_mat <- rating_mat_raw

  for (i in 1:ncol(rating_mat)) {
        rating_mat[, i][is.na(rating_mat[, i])] <- colMeans(rating_mat, na.rm=T)[i]
      }

  # SVD decompose
  rating_mat_svd <- svd(rating_mat)

  # The first 3 dimensions are used
  # supposedly incorporate more than 99% percent of the inforamtion
  rating_pred <- rating_mat_svd$u[, 1:3] %*%
        diag(rating_mat_svd$d[1:3]) %*%
        t(rating_mat_svd$v)[1:3, ]

  rating_pred[!is.na(rating_mat_raw)] <- 0

      # Forecast the movies that the user is likely to have highest ratings of

  row <- which(row_idx==user_id)
  target_ids <- col_idx[sort(rating_pred[row, ], decreasing=T, index.return=T)$ix[1:num]]

  targets <- data.frame()
  for (target_id in target_ids) {
    target <- subset(movies_data, id == target_id) %>% select("title", "genres", "director", "cast")
    # target <- movies_data[id==target_id,c("title", "genres", "year", "director", "cast")]
    targets <- rbind(targets, target)}

  return(targets)
}




