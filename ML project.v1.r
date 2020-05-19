########################################################################
############ ML Project - Nils, Yifan, Jan and Daniel  #################
########################################################################

########### Install and load packages (Probably not all needed, to be reduced at the end)
install.packages('ggcorrplot')
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('ggthems')
install.packages('GGally')
install.packages('VIM')
install.packages('formattable')
install.packages('rlang')
install.packages("curl")
install.packages('tm')

library(class)
library(stringr)
library(lubridate)
library(rlang)
library(ggrepel)
library(ggthems)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(GGally)
library(formattable)
library(VIM)
library(lubridate)
library(here)
library(Rtools)
library(scales)
library(devtools)
library(curl)
library(tm)
library(tidyverse)

########################################################################################
########################################################################################
#####  Load the Data
mainDir = "C:\\Users\\Daniel\\HSG\\2. Semester\\3. Machine Learning\\Projekt\\Data"
setwd(mainDir)
movie <- read.csv('tmdb_5000_movies.csv')
movie2 <- read.csv('tmdb_5000_credits.csv')

########################################################################################
################### Data cleaning and exploration ######################################
#######################################################################################

################ movie ###############################################################

###################  Exploration of Data  #############################################
class(movie)
str(movie)
# Movie is a data.frame and the entries are of class numeric, character and integer.

dim(movie)
# The data base contains 4803 different film entries and 20 properties which are compared.. 

# Exploration of individual categories
movie$spoken_languages #Einfach Englisch, Deutsch, etc.
movie$keywords
movie$title
movie$tagline
movie$vote_average
movie$revenue
movie$popularity
movie$production_countries
movie$original_title
movie$original_language
movie$genres

# Store category names
names<-names(movie)
names

## Eventually to be deleted
# Double entries: 3+3 have he same movie title but are just remakes.
summary(movie) # Shows that three titles are doubled. 
movie$id['2166']
#############################

#####  Check for duplicated entries
sum(duplicated(movie))

# Zero duplicates can be found.

#####  How many complete cases exist
completecases<-sum(complete.cases(movie))
completecases

# 4801 complete cases could be found.

###################  Data cleaning ##############################################
############# Finding and substituting missing values
## Check for NA entries. 
summary(is.na(movie)) 
sum(is.na(movie))

ind <- which(is.na(movie$runtime))
print(movie[ind,])
print(movie$title[ind])
# Two NAs can be identified in runtime. 

## Check for entry = "".
summary(movie=="")

# Homepage, Overview, tagline and release date have empty entries.
# Identify empty entries, for instace regarding category tagline.
ind2<-which(movie$tagline=="")
ind2

## Check for entry = 0.
summary(movie==0)

# Entries of 0 occur in budget, popularity, revenue, runtime, vote_average and vote_count.
ind3<-which(movie$budget==0)
ind3

## Check for enty = []
summary(movie=="[]")

# Entries of [] occur in genres, keywords, production_companies, production_countries and spoken_languages.

### Replace missing values with <NA>
movie[movie==""] <-NA
movie[movie=="[]"] <-NA
movie[movie==0] <-NA

### Check wether empty entries further exist.
summary(movie=="")
summary(movie==0)
summary(movie=="[]")
        
# All empty entries have been replaced by NA.

### Analyis after having replaced empty entries.
## Count NAs.
summary(is.na(movie)) 
sum(is.na(movie))
# 7617 NA entries exist now.

## How many complete cases exist
completecases<-sum(complete.cases(movie))
completecases
incompletecases<-nrow(movie)-completecases
incompletecases

# Now, 1225 entries are complete and thus 3578 complete .

## Check wether all substutions have been successful
summary(movie=="")
summary(movie==0)
summary(movie=="[]")

# Substitions have been successful.

## Check for duplicated entries.
sum(duplicated(movie))

# Further on, duplicated entries cannot be identified.


### Correlation heatmap: Remove highly correlated variables (Better: Identify highly correlated variables to avoid them in analysis)
## Heat map
ggcorr(movie, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

# A correlation of >0.70 is regarded as high. Thus, revenue and vote_count, popularity and vote_count as well as budget and revenue have high correlation.
# Revenue and budget are supposed to correlated because a high budget also leads to a higher expected return compared to a film with a low budger. 

# Introduce profitability column out of revenue and budget to get rid of correlation
movie <- movie %>% 
  mutate(profit = revenue - budget,
         return_on_investment = (profit/budget)*100)
movie

## Heat map after introduction of profitability
ggcorr(movie, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

# Now, revenue and profit are very highly correlated. Also, vote_count and profit show a high correlation.
# However, budget and profit have now a low vorrelation and can be used for data anayzed.
# Return on investment has very low correlation with other categories and can be used for data analysis.
# We do not delete categories with high correlation. Instead we choose uncorrelated variables specifically for individual analysis.

### Analysing of missing data after extension of categories
### Visualize missing data
missing.values <- aggr(movie, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .5, cex.numbers = 5, combined = F, gap = -.2)

# Maybe just delet homepage, proft, revenue and budet, tagline, return:no investment because to few values. 
colSums(sapply(movie, is.na))

# It can be seen that homepage, profit, return on investment, revenue, budget, tagline and #keywords have a high number of missing datas. 
# Therefore, it is suggested to omit homepage, profit and return_on investment of analyis.
########## tbd...

################################## Cleaning of categories with multiple entrie
## Check classes of all categories
head(movie)
# As seen in data exploration, production countries, original languages, spoken languages, keywords as well as genre have multiple entries for one film. 

## Check classes of categories with multiple entries
str(movie$keywords)
str(movie$production_countries)
str(movie$original_language)
str(movie$spoken_languages)
str(movie$genres)

# The classes with multiple entries are of type character. We change the entries in order to have more easily handalbe entries.

##### Genres ################
# Store genres in variable, check class
Genre<-movie$genres
class(Genre)

# Clean entries and set format to dataframe
Genre <- removeNumbers(Genre)
Genre<-gsub('name', '', Genre)
Genre<-gsub('id', '', Genre)
Genre[Genre==""]<-NA
Genre<-as.data.frame(Genre)

# Separate Columns
Genre<-separate(Genre, col = Genre, into=c(as.character(1:25)))

# Sort columuns: NAs after genre names.
sort.Genre<-do.call(rbind,lapply(1:nrow(Genre),function(x) { z=sort(Genre[x,],na.last=TRUE);colnames(z)<-c(as.character(1:25));return(z) } ))
sort.Genre[is.na(Genre)]<-""

# Comprise columns in one column
Genre2<-apply(sort.Genre,1,paste,collapse="")
Genre3<-gsub("(?!^)(?=[[:upper:]])", ", ", Genre2, perl=T)
Genre3<-gsub('Fiction, Science', 'Science Fiction', Genre3)
genres<-Genre3

# Bind genre to movie
movie = subset(movie, select = -c(genres) )
movie<-cbind(movie,genres) ## Noch löschen das erste Genre.
movie

########### Hint: How the Genre string can be filtered to a specific word:
#install.packages('stringr')
#library(string)

which(str_detect(genre, "Adventure"))
movie[84,]

#########################################################################
##### Keywords ################
# Store keywords in variable, check class
keywords<-movie$keywords
class(keywords)

# Clean entries and set format to dataframe
keywords <- removeNumbers(keywords)
keywords<-gsub('name', '', keywords)
keywords<-gsub('id', '', keywords)
keywords<-gsub('": , "": ', '', keywords)
keywords<-gsub('\"', '',keywords)
keywords<-gsub("\\{|\\}", "", keywords)
keywords<-gsub("\\[|\\]", "", keywords)
keywords<-as.data.frame(keywords)

# Separate Columns
keywords<-separate(keywords, col=keywords, into=c(as.character(1:25)), sep="[(\\,\\)]")
keywords[keywords==""]<-NA

# Sort columuns: NAs after genre names. 
sort.keywords<-do.call(rbind,lapply(1:nrow(keywords),function(x) { z=sort(keywords[x,],na.last=TRUE);colnames(z)<-c(as.character(1:25));return(z) } ))

# Comprise columns in one column
keywords2<-apply(sort.keywords,1,paste,collapse=", ")
keywords2<-gsub(", NA", "", keywords2)
keywords<-keywords2
keywords

# Bind keywords to movie
movie = subset(movie, select = -c(keywords) )
movie<-cbind(movie,keywords)
movie

## [tbd]: remove single characters. 

##### Production Countries ################ 
# Store keywords in variable, check class
countries<-movie$production_countries
class(countries)

# Clean entries and set format to dataframe
countries <- removeNumbers(countries)
countries<-gsub('name', '', countries)
countries<-gsub('id', '', countries)
countries[countries==""]<-NA
countries<-gsub('iso', '', countries)
countries<-gsub('\"__": ', "", countries)
countries<-gsub(', \"\": ', "", countries)
countries<-gsub("\\{|\\}", "", countries)
countries<-gsub("\\[|\\]", "", countries)
countries<-gsub('\"', "", countries, fixed = TRUE)

countries<-as.data.frame(countries)
class(countries)

# Separate Columns
#Genre<-sepGenre[Genre=='name']<-""
# Genre<-separate(Genre, col = Genre, into=c("1","2","3","4","5","6", "7", "8", "9", "10","11","12","13","14","15", "16","17", "18","19", "20", "21", "22", "23", "24", "25"))
# countries<-separate(countries, col = countries, into=c(as.character(1:25)))
countries<-separate(countries, col=countries, into=c(as.character(1:25)), sep="[(\\,\\)]")
countries

# Sort columuns: NAs after genre names. 
sort.countries<-do.call(rbind,lapply(1:nrow(countries),function(x) { z=sort(countries[x,],na.last=TRUE);colnames(z)<-c(as.character(1:25));return(z) } ))
#sort.countries[is.na(countries)]<-""

# Comprise columns in one column
countries2<-apply(sort.countries,1,paste,collapse=", ")
countries2<-gsub(", NA", "", countries2)
countries2<-gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", countries2)

production_countries<-countries2

# Bind production_country to movie
movie = subset(movie, select = -c(production_countries) )
movie<-cbind(movie,production_countries) ## Noch löschen das erste Genre.

##### Original Languages ################ 
## Daniel: I suggest to omit this category and to use only spoken_languages
movie = subset(movie, select = -c(original_language) )

##### Production Companies ################ 
## Daniel: I suggest to omit this category.
movie = subset(movie, select = -c(production_companies) )

##### Spoken Languages  ################  
# Store keywords in variable, check class
spoken_languages<-movie$spoken_languages
class(spoken_languages)

# Clean entries and set format to dataframe
spoken_languages <- removeNumbers(spoken_languages)
spoken_languages<-gsub('name', '', spoken_languages)
spoken_languages<-gsub('id', '', spoken_languages)
spoken_languages[spoken_languages==""]<-NA
spoken_languages<-gsub('iso', '', spoken_languages)
spoken_languages<-gsub('\"__": ', "", spoken_languages)
spoken_languages<-gsub(', \"\": ', "", spoken_languages)
spoken_languages<-gsub("\\{|\\}", "", spoken_languages)
spoken_languages<-gsub("\\[|\\]", "", spoken_languages)
#spoken_languages<- str_extract_all(spoken_languages, "\"[a-z]{1,4}\"")

spoken_languages<-as.data.frame(spoken_languages)

# Separate Columns
# Comprise columns in one column
spoken_languages<-apply(spoken_languages,1,paste,collapse=", ")
spoken_languages

# Bind spoken_languages to movie
movie = subset(movie, select = -c(spoken_languages) )
movie<-cbind(movie,spoken_languages)
movie

################################# Check dataset after datacleaning ######################
head(movie)
names(movie)

######################### Stuff from Daniel
#spoken_languages<-gsub("[^[a-z][a-z]]", "", spoken_languages, fixed=TRUE)
#spoken_languages<- str_extract_all(spoken_languages, "[a-z]{1,4}")
#spoken_languages<-gsub('"\"', "", spoken_languages, fixed = TRUE)
#spoken_languages<-gsub("", "", "", spoken_languages)

# Separate Columns
# spoken_languages <- data.frame(matrix(unlist(spoken_languages), nrow=4803, ncol=5),stringsAsFactors=FALSE)
#spoken_languages<-separate(spoken_languages, col = spoken_languages, into=c(as.character(1:25)))

# Sort columuns: NAs after genre names. 
#sort.spoken_languages<-do.call(rbind,lapply(1:nrow(spoken_languages),function(x) { z=sort(spoken_languages[x,],na.last=TRUE);colnames(z)<-c(as.character(4));return(z) } ))
#sort.countries[is.na(spoken_languages)]<-""

#spoken_languages<-unite(spoken_languages, col='spoken languages', sep = ", ", remove = TRUE, na.rm = FALSE)
#spoken_languages<-paste(unique(spoken_languages), collapse = ' ')
#spoken_languages3<-gsub("(?!^)(?=[[:upper:]])", ", ", spoken_languages2, perl=T)
#########################

####################################################################################
################## Data visualisastion #############################################
####################################################################################

### tbd by Daniel: I will extend this part for the description of the data. 
ggplot(movie, aes(title)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))

## Maybe make a category with years...

ggplot(movie, aes(movie$vote_average)) + geom_density(color="darkblue", fill="cornflowerblue")
ggplot(movie, aes(movie$budget)) + geom_density(color="darkblue", fill="cornflowerblue")
ggplot(movie, aes(movie$revenue)) + geom_density(color="darkblue", fill="cornflowerblue")
ggplot(movie, aes(movie$runtime)) + geom_density(color="darkblue", fill="cornflowerblue")

# Calculate average vote
averagevoting<-sum(movie$vote_average)/length(movie$vote_average)
averagevoting

# eventually make table



######################### movie2 ###########################

# Daniel: Codes processing credit database have a very long running time. Suggestion: Either find an other way to 
# clean it, which may be faster, or just drop this set.
#names(movie2)

# Remove crew column 
#movie2 = subset(movie2, select = -c(crew) )

##### Keywords ################
# Store keywords in variable, check class
#cast<-movie2$cast
#class(cast)


# Clean entries and set format to dataframe
#cast <- removeNumbers(cast)
#cast<-gsub('name', '', cast)
#cast<-gsub('id', '', cast)
#cast<-gsub('cast_', '', cast)
#cast<-gsub('character', '', cast)
#cast<-gsub('credit', '', cast)
#cast<-gsub('gender', '', cast)

#cast<-gsub('": , "": ', '', cast)
#cast<-gsub('\"', '',cast)
#cast<-gsub("\\{|\\}", "", cast)
#cast<-gsub("\\[|\\]", "", cast)
#cast<-gsub('\"', "", cast, fixed = TRUE)
#cast<-gsub('\\', "", cast, fixed = TRUE)
#cast<-gsub('\\\\', "", cast, fixed = TRUE)
#cast<-gsub(':', "", cast, fixed = TRUE)
#cast<-gsub('\"\"', "", cast, fixed = TRUE)

#cast<-as.data.frame(cast)
#head(cast)

# Separate Columns
#cast<-separate(cast, col=cast, into=c(as.character(1:40)), sep="[(\\,\\)]")

# Sort columuns: NAs after genre names. 
#sort.cast<-do.call(rbind,lapply(1:nrow(cast),function(x) { z=sort(cast[x,],na.last=TRUE);colnames(z)<-c(as.character(1:25));return(z) } ))
#head(cast)
#cast[cast==""]<-NA
#cast <- cast[,c(6,13,20,27,34)]

# Comprise columns in one column
#cast2<-apply(cast,1,paste,collapse=", ")
#cast2<-gsub(", NA", "", cast2)
#cast<-cast2
#tail(cast2)

# Bind keywords to movie
#movie = subset(movie, select = -c(cast) )
#movie<-cbind(movie,cast)
#movie


######################################################################################
################## Data preprocessing ################################################
#####################################################################################



#######################################################################################
################## Implement the algorithm ############################################
######################################################################################

