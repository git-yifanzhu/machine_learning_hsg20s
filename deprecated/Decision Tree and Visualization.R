###################################
# Machine Learning Group Project
###################################

# load libraries
library(ggplot2)
library(ggthemes)
library(tree)
library(rpart)

# load the dataset
load("./data/movies.RData")

##################################
# Visualization
##################################

# join datasets
colnames(ratings_data) <- c("userId","id","rating")
ratings_data
all_data <- merge(ratings_data, movies_data, by ="id")
all_data

# visualize ratings
mean(ratings_data$rating)
median(ratings_data$rating)
sd(ratings_data$rating)
ratings_table <- as.data.frame(table(ratings_data$rating))
ratings_table$fraction <- round(((ratings_table$Freq/nrow(ratings_data))*100), digits = 4)
colnames(ratings_table) <- c('rate', 'freq', 'fraction')
ratings_table

# plot a line chart
graph_ratings <- ggplot(ratings_table, aes(x = rate, y = fraction, group = 1)) + geom_line(size= 1, color = 'dodgerblue2') + geom_point(shape = 23, color = 'dodgerblue2', fill = 'white', size = 3) + 
  theme_clean() + xlab("Rate") + ylab('Percentage') + ggtitle('Rating prevalence', subtitle = 'Mid/high ratings seem to be prevalent in the sample')
graph_ratings

###################################
# Decision Tree Algorithm
###################################

# 3 possible ways to use decision tree algorithm
alt <- 1

# Alternative 1: compute the tree for the most active user based on movies he/she has rated
# The goal is to predict the rating this user will give to movies he hasn't watched thus far

if (alt == 1) {

# identify most active user
user_activity <- as.data.frame(table(all_data$userId))
most_active_user <- subset(user_activity,user_activity$Freq==max(user_activity$Freq),select=Var1)
userId <-  most_active_user[1,1]
rated_movies <- which(all_data$userId == userId)

# only include numeric features and data for the most active user
Data.train <- all_data[rated_movies,c("budget","popularity","revenue","runtime","vote_average",
                                      "vote_count","year","weighted_rating","rating")]

# identify movies the user hasn't watched. For those movies we want to make predictions
# get movieIds of watched movies
rated_movieids <- as.list(all_data[rated_movies,"id"])
movies_data$watched <- ifelse(is.element(movies_data$id,rated_movieids$id)==TRUE,1,0)

# make unwatched movies prediction dataset and only keep numeric features
Data.pred <- movies_data[which(movies_data$watched==0),c("budget","popularity","revenue","runtime",
                                                         "vote_average","vote_count", "year","weighted_rating","title")]
# compute the tree
movie_tree <- tree(rating~.,Data.train)

} else if (alt == 2) {
  
  # Alternative 2: like alternative 1 but instead of making predictions for unwatched movies by
  # the most active user test the accuracy of the decision tree algorithm
  
  # identify most active user
  user_activity <- as.data.frame(table(all_data$userId))
  most_active_user <- subset(user_activity,user_activity$Freq==max(user_activity$Freq),select=Var1)
  userId <-  most_active_user[1,1]
  rated_movies <- which(all_data$userId == userId)
  
  # only include numeric features and data for the most active user
  User_data <- all_data[rated_movies,c("budget","popularity","revenue","runtime","vote_average",
                                        "vote_count","year","weighted_rating","rating")]
  set.seed(1)
  resh <- sample(1:nrow(User_data))
  Data <- User_data[resh,]
  
  Data.train <- Data[1:ceiling(nrow(Data)*7/10),]
  Data.test <- Data[(ceiling(nrow(Data)*7/10)+1):nrow(Data),]

  # compute the tree
  movie_tree <- tree(rating~.,Data.train)
  
}  else if (alt == 3) {

# Alternative 3: use 2000 observations i.e. ratings (note that the decision tree would only have 1-2 nodes for >2000 observations)
# by different users to compute the decision tree. Goal: identify binary movie features allowing to predict user ratings

# use all available numeric features to build the tree
tree_data <- all_data[,c("budget","popularity","revenue",
                         "runtime","vote_average","vote_count",
                         "year","weighted_rating","rating")]

# split data into training and testing data after shuffling
set.seed(1)
resh <- sample(1:nrow(tree_data))
Data <- tree_data[resh,]
Data.train <- Data[1:2000,]
Data.test <- Data[2001:nrow(Data),]

# compute movie tree
movie_tree <- tree(rating~.,Data.train)
}
summary(movie_tree)

# plot the tree
plot(movie_tree)
text(movie_tree)
title(main = "Decision Tree for user 564")

#for alternative 1: predict ratings of unwatched movies
if (alt == 1){
  Data.pred$predicted_rating <- predict(movie_tree, newdata = Data.pred)
  setorder(Data.pred,-predicted_rating)
  highest_rating <- max(Data.pred$predicted_rating)
  print(Data.pred[which(Data.pred$predicted_rating == highest_rating),c("title")])

# for alternative 2: test model by applying model to the testing data
} else if (alt == 2) {
  # compute the testing error
  yhat <- predict(movie_tree, newdata = Data.test)
  error <- Data.test$rating-yhat
  RSS <- error%*%error
  print(RSS)
  MSE <- RSS/(nrow(Data.test))
  print(MSE)
  
  # prune tree and plot
  mse <- rep(0,17)
  rmse <- rep(0,17)
  stop <- length(unique(movie_tree$where))
  stop
  for (num in 2:stop){
    pruned_tree <- prune.tree(movie_tree, best = num)
    yhat <- predict(pruned_tree, newdata = Data.test)
    error <- Data.test$rating-yhat
    RSS <- error%*%error
    MSE <- RSS/(nrow(Data.test))
    RMSE <- sqrt(MSE)
    k <- num - 1
    mse[k] <- MSE
    rmse[k] <- RMSE
    plot(pruned_tree)
    text(pruned_tree)
    title(main = "New Decision Tree with k=18")
  }
  plot(mse)
  plot(rmse)
  title("RMSE for different numbers of terminal nodes")

} else if (alt == 3) {
  yhat <- predict(movie_tree, newdata = Data.test)
  errors <- Data.test$rating-yhat
  RSS <- errors%*%errors
  MSE <- RSS/(nrow(Data.test))
  print(errors)
  print(RSS)
  print(MSE)
}



########################################
# Logistic Regression
########################################

