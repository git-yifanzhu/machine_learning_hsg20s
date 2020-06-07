# Machine Learning
# University of St. Gallen
# May 2020
# Movie Recommendation Engine
# Yifan, Daniel, Nils, Jan

# 
# This file deals with the recommendation system.
# 

# ==============================
# DATA PREP
# ==============================

# ==============================
# DATA VISUALIZATION
# ==============================

# ==============================
# RECOMMENDATION ENGINE !!!
# ==============================


# Load packages
library(data.table)
library(dplyr)
library(tidytext)
library(formattable)

# Load data
load("./data/movies.RData")

# Define public function to be used in this part
get_movie_data <- function(movie_title, invert=F){
  if (invert==F) {
    return(movies_data[title == movie_title, ])
  } else {
    return(movies_data[-which(movies_data$title==movie_title), ])
  }
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
movies_data$weighted_rating <- round(movies_data$weighted_rating,digits = 4)
get_best_rated_movies(10)
formattable(get_best_rated_movies(10), align = 'l')

# ------------------------------
# 2 Popular Movies
# ------------------------------

# Retrieve the most popular movies
get_popular_movies <- function(num=10){
  setorder(movies_data, -popularity)
  return(movies_data[1:num, 
                     c("title", "popularity")])
}

movies_data$weighted_rating <- round(movies_data$weighted_rating,digits = 4)
get_popular_movies(10)
formattable(get_popular_movies(10), align = 'l')

# ------------------------------
# 3 Logistic Regression
# ------------------------------

# Predict the movies a user is most likely to have a rating of 5 towards
get_recommend_logistic <- function(user_id, num=10){
  user_rating <- ratings_data[userId==user_id, ]
  user_rating$likes <- user_rating$rating==5
  
  user_rating <- merge(user_rating, movies_data, by.x="movieId", by.y="id")
  
  logistic_mod <- glm(likes ~ popularity + 
                        revenue + runtime + vote_average + 
                        year, 
                      data=user_rating)
  summary(logistic_mod)
  
  target_ids <- setdiff(movies_data$id, user_rating$movieId)
  target <- movies_data[id %in% target_ids, ]
  
  target$likes <- predict(logistic_mod, newdata=target)
  target$likes <- round(target$likes, digits = 4)
  setorder(target, -likes)
  
  return(target[1:num,
                c("title", "likes", "genres", "year", "director", "cast")])
}

# User with the most ratings
user_id <- names(sort(table(ratings_data$userId), decreasing=T))[1]
formattable(get_recommend_logistic(user_id, 10))

# test recommendation system
user_rating <- ratings_data[userId==user_id, ]
user_rating$likes <- user_rating$rating==5
user_rating <- merge(user_rating, movies_data, by.x="movieId", by.y="id")

set.seed(1)
resh <- sample(1:nrow(user_rating))
user_rating <- user_rating[resh,]

Data.train <- user_rating[1:ceiling(nrow(user_rating)*7/10),]
Data.test <- user_rating[(ceiling(nrow(user_rating)*7/10)+1):nrow(user_rating),]

logistic_mod_test <- glm(likes ~ popularity + 
                      revenue + runtime + vote_average + 
                      year, 
                    data=Data.train)
summary(logistic_mod_test)

prediction_error <- matrix(0, nrow = 50, ncol = 2)
for (i in 1:50){
  decCrit = 0.165 + (i-1) * 0.005
  logistic.prob <- predict(logistic_mod_test, newdata=Data.test, type='response')
  logistic.pred <- rep(0, length(logistic.prob))
  logistic.pred[logistic.prob >= decCrit] = 1
  err.rate.logistic <- 1 - sum(logistic.pred == Data.test[, "likes"]) / nrow(Data.test)
  prediction_error[i,1] = decCrit
  prediction_error[i,2] = err.rate.logistic
  } 
plot(prediction_error, xlab = "decCrit", ylab = "error rate", main = "Relationship between error rate and decCrit")
prediction_error


# ------------------------------
# 4 k-NN for Genres, Keywords, Director, Cast
# ------------------------------

# Calculate the similarity score of a feature between object and target
get_similarity <- function(obj_feature, target_feature){
  obj_feature <- unlist(obj_feature)
  target_feature <- unlist(target_feature)
  
  similarity_unit <- 1 / (length(obj_feature) * length(target_feature))
  
  similarity <- 0
  
  for (i in 1:length(obj_feature)) {
    for (j in 1:length(target_feature)) {
      if (obj_feature[i] == target_feature[j]) {
        similarity <- similarity + similarity_unit
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
  
  for (i in 1:nrow(target)) {
    similarity = sum(
      c(get_similarity(obj$genres, target[i, ]$genres), 
        get_similarity(obj$keywords, target[i, ]$keywords), 
        get_similarity(obj$director, target[i, ]$director),
        get_similarity(obj$cast, target[i, ]$cast)) * 
        weights)
    similarity_scores <- c(similarity_scores, similarity)
  }
  
  target[, similarity_score := similarity_scores]
  
  setorder(target, -similarity_score)
  
  return(target[1:num,
                c("title", "genres", "keywords", "director", "cast","id")])
}

recommendation <- get_recommend_kNN("Terminator 3: Rise of the Machines", 10, c(1, 1, 1, 1))
formattable(recommendation, align = "l")

# test recommendation by finding the rating that users who watched both movies gave to recommended movies
relevant_users <- as.list(ratings_data[which(ratings_data$movieId == 296 & ratings_data$rating >=3.5),"userId"])
relevant_movies <- as.list(recommendation$id)
movie_ratings <- ratings_data[which(is.element(ratings_data$movieId,relevant_movies) == TRUE),]
relevant_ratings <- movie_ratings[which(is.element(movie_ratings$userId,relevant_users$userId) == TRUE),]
relevant_movie_ratings <- round(aggregate(relevant_ratings[,"rating"], list(relevant_ratings$movieId), mean),digits = 2)
setorder(relevant_movie_ratings, -rating)
relevant_movie_ratings$title <- movies_data[which(is.element(movies_data$id,relevant_movie_ratings$Group.1) == TRUE),"title"]
formattable(relevant_movie_ratings[,c("title","rating")], align = "l")

# ------------------------------
# 5 Plot Based Recommender
# ------------------------------

# Calculate the TF-IDF Based Similarity of all the movies in our dataset
plots <- movies_data %>%
  select(id, title, text=overview)

plots_words <- plots %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(id, title, word, sort=TRUE)

total_words <- plots_words %>%
  group_by(id, title) %>% 
  summarize(total = sum(n))

plots_words <- left_join(plots_words, total_words, by=c("id", "title"))
plots_words <- plots_words %>% bind_tf_idf(word, id, n)

row_idx = sort(unique(plots_words$id))
col_idx = unique(plots_words$word)

tfidf_mat <- matrix(0, nrow=length(row_idx), ncol=length(col_idx))
for (i in 1:nrow(plots_words)) {
  row <- which(plots_words[i, ]$id==row_idx)
  col <- which(plots_words[i, ]$word==col_idx)
  tfidf_mat[row, col] = plots_words[i, ]$tf_idf
}

cos_sim_mat <- tfidf_mat %*% t(tfidf_mat)

# Retrieve the most similar movies based on plots
get_recommend_plot <- function(obj_title, num=10){
  obj_id <- get_movie_data(obj_title)$id
  obj_idx <- which(row_idx==obj_id)
  
  obj_cos_sim <- cos_sim_mat[, obj_idx]
  
  cos_sim <- cbind(row_idx, obj_cos_sim)
  cos_sim <- cos_sim[sort(obj_cos_sim, decreasing=T, index.return=T)$ix, ]
  
  target_ids <- cos_sim[2:(num+1), "row_idx"]
  
  targets <- data.frame()
  for (target_id in target_ids) {
    target <- movies_data[id==target_id, 
                          c("title", "genres", "director", "cast","id")]
    targets <- rbind(targets, target)
  }
  
  return(targets)
}

# get recommendation for Terminator 3
recommendation <- get_recommend_plot("Terminator 3: Rise of the Machines", 10)
formattable(recommendation, align = "l")

# glance at overview of terminator and recommended movies
terminator <- movies_data[which(movies_data$title == "Terminator 3: Rise of the Machines"),c("title","overview")]
recommendation_overview <- movies_data[which(is.element(movies_data$title,recommendation$title)== TRUE),c("title","overview")]
formattable(rbind(terminator,recommendation_overview[1:3,]), align = "l")

# test recommendation system
relevant_users <- as.list(ratings_data[which(ratings_data$movieId == 296 & ratings_data$rating >=3.5),"userId"])
relevant_movies <- as.list(recommendation$id)
movie_ratings <- ratings_data[which(is.element(ratings_data$movieId,relevant_movies) == TRUE),]
relevant_ratings <- movie_ratings[which(is.element(movie_ratings$userId,relevant_users$userId) == TRUE),]
relevant_movie_ratings <- round(aggregate(relevant_ratings[,"rating"], list(relevant_ratings$movieId), mean),digits = 2)
setorder(relevant_movie_ratings, -rating)
relevant_movie_ratings$title <- movies_data[which(is.element(movies_data$id,relevant_movie_ratings$Group.1) == TRUE),"title"]
formattable(relevant_movie_ratings[,c("title","rating")], align = "l")

# ------------------------------
# 6 Collaborative Filtering Based on User Ratings
# ------------------------------

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
get_recommend_user <- function(user_id, num=10){
  row <- which(row_idx==user_id)
  target_ids <- col_idx[sort(rating_pred[row, ], decreasing=T, index.return=T)$ix[1:num]]
  
  targets <- data.frame()
  for (target_id in target_ids) {
    target <- movies_data[id==target_id, 
                          c("title", "genres", "year", "director", "cast")]
    targets <- rbind(targets, target)
  }
  
  return(targets)
}

formattable(get_recommend_user(564), align = "l")
