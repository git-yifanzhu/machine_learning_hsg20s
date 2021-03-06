# Machine Learning Project
# University of St. Gallen
# May 2020
# Movie Recommendation Engine
# Yifan, Daniel, Nils, Jan

# 
# This file is part 1 and deals with data processing and visualisations.
# 

# ==============================
# DATA PREPARATION
# ==============================
# Load packages
library(data.table)
library(stringr)
library(bit64)
library(plm)


# Define function to parse JSON formatted columns
parse_json <- function(string_in, target, len){
	# Parse JSON in the TMDB data
	# target is the field to look for
	# len is the maximum length of output
	string_out = str_extract_all(string_in, 
		paste("\'", target, "\':\\s{1}\'.*?\'", sep=""))
	string_out = lapply(string_out, 
		str_extract, ":.*")
	string_out = lapply(string_out, 
		str_extract, "[a-zA-Z0-9]+[a-zA-Z0-9 ]+")
	string_out = unlist(string_out)

	if (length(string_out) > len) {
		return(string_out[1:len])
	} else{
		return(string_out)
	}
}

# Define function to parse JSON formatted CREW for director
parse_director_json <- function(string_in){
	# Parse JSON in the TMDB data to get name of the director
	string_out = str_extract(string_in, 
		"\'Director\'.*?\\}")
	string_out = str_extract(string_out,
		":.*")
	string_out = str_extract(string_out,
		"[a-zA-Z]+[a-zA-Z ]+")
}


# Import csv files
movies_data <- fread("./data/movies_metadata.csv", encoding="UTF-8", fill=T)
keywords_data <- fread("./data/keywords.csv", encoding="UTF-8", fill=T)
credits_data <- fread("./data/credits.csv", encoding="UTF-8", fill=T)
ratings_data <- fread("./data/ratings_small.csv", encoding="UTF-8", fill=T)

# Remove unsed columns
movies_data[, belongs_to_collection := NULL]
movies_data[, homepage := NULL]
movies_data[, imdb_id := NULL]
movies_data[, original_title := NULL]
movies_data[, poster_path := NULL]
movies_data[, spoken_languages := NULL]
movies_data[, status := NULL]
movies_data[, video := NULL]

ratings_data[, timestamp := NULL]

# Merge data
movies_data <- movies_data[grepl("^[0-9]*$", movies_data$id), ]
movies_data[, id := as.numeric(id)]
movies_data <- merge(movies_data, keywords_data, by="id")
movies_data <- merge(movies_data, credits_data, by="id")

# Remove intermediary datasets
rm(keywords_data, credits_data)

# Convert types
movies_data[, adult := as.logical(adult)]
movies_data[, budget := as.numeric(budget)]
movies_data[, popularity := as.numeric(popularity)]
movies_data[, revenue := as.numeric(revenue)]

# Generate new variables
movies_data[, year := as.numeric(substr(release_date, 1, 4))]
movies_data[, release_date := NULL]

# Drop nonsense obs
movies_data <- movies_data[budget > 10 ^ 3, ]
movies_data <- movies_data[genres != "[]", ]
movies_data <- movies_data[overview != "", ]
movies_data <- movies_data[revenue > 10 ^ 3, ]
movies_data <- movies_data[runtime > 0, ]
movies_data <- movies_data[keywords != "[]", ]
movies_data <- movies_data[cast != "[]", ]
movies_data <- movies_data[crew != "[]", ]
movies_data <- movies_data[year > 0, ]

# Parse JSON Formatted Columns
movies_data$genres <- lapply(movies_data$genres, parse_json, "name", 3)
movies_data$keywords <- lapply(movies_data$keywords, parse_json, "name", 3)
movies_data$production_companies <- lapply(movies_data$production_companies, parse_json, "name", 3)
movies_data$production_countries <- lapply(movies_data$production_countries, parse_json, "name", 3)
movies_data$cast <- lapply(movies_data$cast, parse_json, "name", 3)
movies_data$crew <- lapply(movies_data$crew, parse_director_json)

# Rename Columns
colnames(movies_data)[which(colnames(movies_data) == "crew")] <- "director"
colnames(movies_data)[which(colnames(movies_data) == "original_language")] <- "language"

# Remove duplicates
movies_data <- movies_data[!duplicated(movies_data, by="id"), ]

# Remove obs with NA nested in column Manually
movies_data <- movies_data[id != 807, ]
movies_data <- movies_data[id != 9042, ]
movies_data <- movies_data[id != 9431, ]
movies_data <- movies_data[id != 9600, ]
movies_data <- movies_data[id != 16911, ]
movies_data <- movies_data[id != 37430, ]
movies_data <- movies_data[id != 70527, ]
movies_data <- movies_data[id != 146304, ]
movies_data <- movies_data[id != 158011, ]
movies_data <- movies_data[id != 244506, ]
movies_data <- movies_data[id != 362045, ]

#check if balanced data -> TRUE
is.pbalanced(movies_data)

# Adjust ratings data accordingly
movies_id <- unique(movies_data$id)
ratings_data <- ratings_data[movieId %in% movies_id]

# Calculate weighted rating with IMDB Formula
rating_average <- mean(movies_data$vote_average)
minimum_votes <- quantile(movies_data$vote_count, 0.75)

movies_data[, weighted_rating := 
	vote_count * vote_average / (vote_count + minimum_votes) + 
	minimum_votes * rating_average / (vote_count + minimum_votes)]

save(movies_data, ratings_data, file="./data/movies.RData")

# ==============================
# 2 DATA VISUALIZATION
# ==============================
library(GGally)

#---------- Correlation heatmap:
#Remove highly correlated variables (Better: Identify highly correlated variables to avoid them in analysis)
ggcorr(movies_data, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

# Introduce profitability column out of revenue and budget to get rid of correlation
# movies_data <- movies_data %>% 
#   mutate(profit = revenue - budget,
#          return_on_investment = (profit/budget)*100)

# Now, budget does not have a good distribution but otherwise only take revenue or profit. 


#--------- Check descriptive Statistics and distributions:
summary(movies_data)


## distributions
ggplot(movies_data, aes(movies_data$vote_average)) + geom_density(color="darkblue", fill="cornflowerblue")
ggplot(movies_data, aes(movies_data$budget)) + geom_density(color="darkblue", fill="cornflowerblue")#skewed
ggplot(movies_data, aes(movies_data$revenue)) + geom_density(color="darkblue", fill="cornflowerblue")#skewed
ggplot(movies_data, aes(movies_data$return_on_investment)) + geom_density(color="darkblue", fill="cornflowerblue")# not using
ggplot(movies_data, aes(movies_data$runtime)) + geom_density(color="darkblue", fill="cornflowerblue")
ggplot(movies_data, aes(movies_data$vote_average)) + geom_density(color="darkblue", fill="cornflowerblue")
ggplot(movies_data, aes(movies_data$vote_count)) + geom_density(color="darkblue", fill="cornflowerblue") #skewed

#target variable
hist(movies_data$weighted_rating)
hist(ratings_data$rating)#ok

##################################
# Visualization
##################################

# join datasets
colnames(ratings_data) <- c("userId","id","rating")
all_data <- merge(ratings_data, movies_data, by ="id")

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







