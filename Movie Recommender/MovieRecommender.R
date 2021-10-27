#Essential libraries
library(ggplot2)
library(recommenderlab)
library(data.table)
library(reshape2)

#Reading in the data
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
View(rating_data)
#Creating Genre data frame, to create a matrix to compare all the genres and find matches
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)

movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE)

colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

#Constructing new matrix with all the genres
genre_mat1 <- matrix(0,10330,18)

#Fill first row with our 18 genres
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

#Sets value to each column
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
}
str(genre_mat1)

#Converts the columns from chr into int because we will put numbers into this.
str(genre_mat1)
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)

#Search Matrix adding each movie title in so we can start assigning genre values.
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
View(SearchMatrix)    

#Most movies have multipe genre classifications, so we have to create a sparse matrix 
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
View(ratingMatrix)

#The different types of recommender models through recommenderlab
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
recommendation_model$IBCF_realRatingMatrix$parameters

#Constructing a samples matrices that look at similarity between users and movies
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

movie_similarity <- similarity(ratingMatrix[, 1:4], 
                               method ="cosine", 
                               which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")

#Creating a table of unique rating values. (1-5 w/ increments of .5)
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

#Creating a count of movie ratings
Table_of_Ratings <- table(rating_values) 
Table_of_Ratings

########################################################## 
#The dataset is too large to plot for every user. We only want to use ratings from active users, so we are setting the minimum voting threshold to 50.
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings
View(movie_ratings)
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
#Heatmap between the most frequent raters and their ratings. 
#Poor visualization because the gradient goes from a light grey to black, making it hard to differentiate between the values. 
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

#Bar plot of the average ratings per user and the volume of those ratings.
#We can see most of the ratings lie in between 3.5 and 4.5.
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")

############################################# 
#Data Normalization to standardize the rating scale
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

#Heatmap of the normalized ratings of the top users
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

#####################
#Data Binarization
#Even better than normalization because it's more definitive. 
#The options are either good fit (rating of 3 or more) or bad fit (>3)
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_films <- binarize(movie_ratings, minRating = 3)
#Heatmap of top users and top movies
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")
############################################# 
#Item-based Collaborative Filtering
#Sample data to test. 80% Training; 20% Test
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))

training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]
training_data
testing_data

#Building the system
#Setting the recommenderlab recommendation system
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)

#only takes the top 20 to make a small heatmap
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

#Sum of the rows with similarities above 0
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

############################################# 
#How to build Recommender System
#Top_recommendations refers to the number of items to recommend to each user
top_recommendations <- 10 
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations


user1 <- predicted_recommendations@items[[1]] 
#Applies the recommendation system from above 
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
#Converts the movie index into the movie title 
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}

#Top 10 Recommendations for user 2 based on user 1
movies_user2

########################
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) 
#dim(recommendation_matrix)
recommendation_matrix[,1:4]
#Shows the similarity values for the top 10 movies
recommendation_matrix

#Looks at the number of matched items in the dataset
number_of_items <- factor(table(recommendation_matrix))
number_of_items
chart_title <- "Distribution of the Number of Items for IBCF"

qplot(number_of_items, fill=I("steelblue"), col=I("red")) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
#Converts indexes into titles
for(i in 1:4) {
  table_top[i,1] <- as.character(subset(movie_data,
                                        movie_data$movieId == table_top[i,1])$title)
}

#top movies with the most matches. 
colnames(table_top) <- c("Movie Title", "No. of Items")
head(table_top)
View(table_top)
