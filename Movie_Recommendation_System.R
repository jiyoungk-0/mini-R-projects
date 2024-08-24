# Install and load required packages
# Uncomment and run if the packages are not already installed
# install.packages(c("recommenderlab", "ggplot2", "Matrix", "data.table"))

library(recommenderlab)
library(ggplot2)

# Read in the data
ratings <- read.table("~/Documents/Portfolio/Data/ml-100k/u.data", sep="\t", header=FALSE, 
                      col.names = c("user_id", "item_id", "rating", "timestamp"))

movies <- read.table("~/Documents/Portfolio/Data/ml-100k/u.item", 
                     sep="\t", header=FALSE, stringsAsFactors=FALSE, 
                     col.names = c("movieId", "title", "release_date", "video_release_date", "IMDb_URL", 
                                   "unknown", "Action", "Adventure", "Animation", "Children", "Comedy", 
                                   "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", 
                                   "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western"),
                     fill=TRUE, quote="")

# Fix movie data issues
movies_split <- do.call(rbind, strsplit(movies$movieId, "\\|"))
movies_df <- as.data.frame(movies_split, stringsAsFactors = FALSE)
colnames(movies_df) <- c("movieId", "title", "release_date", "video_release_date", "IMDb_URL", "unknown",
                         "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary",
                         "Drama", "Fantasy", "Film.Noir", "Horror", "Musical", "Mystery", "Romance", "Sci.Fi",
                         "Thriller", "War", "Western")
movies_df$movieId <- as.numeric(movies_df$movieId)

# Data cleaning
ratings <- ratings[, c("user_id", "item_id", "rating")]

# Convert data to realRatingMatrix format
ratingMatrix <- as(ratings, "realRatingMatrix")

# Set seed for reproducibility
set.seed(123)

# Create evaluation scheme
evaluationScheme <- evaluationScheme(ratingMatrix, method="split", train=0.8, given=10, goodRating=4)

# Train User-based and Item-based Collaborative Filtering models
recommender_ubcf <- Recommender(getData(evaluationScheme, "train"), method = "UBCF")
recommender_ibcf <- Recommender(getData(evaluationScheme, "train"), method = "IBCF")

# Predict ratings
predicted_ubcf <- predict(recommender_ubcf, getData(evaluationScheme, "known"), type="ratings")
predicted_ibcf <- predict(recommender_ibcf, getData(evaluationScheme, "known"), type="ratings")

# Calculate prediction accuracy
ubcf_results <- calcPredictionAccuracy(predicted_ubcf, getData(evaluationScheme, "unknown"))
ibcf_results <- calcPredictionAccuracy(predicted_ibcf, getData(evaluationScheme, "unknown"))

# Print accuracy results
print("User-based Collaborative Filtering (UBCF) Results:")
print(ubcf_results)

print("Item-based Collaborative Filtering (IBCF) Results:")
print(ibcf_results)

# Interpretation of accuracy results:

# User-based Collaborative Filtering (UBCF):
# RMSE (1.2196): Indicates the square root of the average squared differences between predicted and actual ratings. Lower RMSE values suggest better prediction accuracy.
# MSE (1.4875): Represents the average squared differences between predicted and actual ratings. Lower MSE is better.
# MAE (0.9540): Measures the average absolute differences between predicted and actual ratings. Lower MAE suggests better accuracy.

# Item-based Collaborative Filtering (IBCF):
# RMSE (1.4466): Higher than UBCF, indicating larger deviations between predicted and actual ratings.
# MSE (2.0927): Higher than UBCF, suggesting less accurate predictions.
# MAE (1.0607): Also higher than UBCF, indicating larger average absolute error.
# Summary: UBCF generally performs better than IBCF, suggesting that user-based filtering is more accurate in predicting ratings for this dataset.

# Generate top recommendations for user 1
top_recommendations <- predict(recommender_ubcf, ratingMatrix[1], n=10)
top_recommendations_list <- as(top_recommendations, "list")

# Extract details for recommended movies
recommended_movie_ids <- c("477", "1048", "899", "1025", "275", "984", "898", "1127", "347", "902")
recommended_movies_list <- movies_df[movies_df$movieId %in% recommended_movie_ids, ]
print("Recommended Movies for User 1")
print(recommended_movies_list)

# Interpretation of recommendations:

# Top Recommendations for User 1: Diverse set of recommendations suggesting movies based on user preferences.
# Movie Details: Covers a range of genres and release dates, beneficial for users who appreciate variety.
# Recommendation Strategy: Suggestions are based on similar users' preferences, helping User 1 discover new movies.

# Plot rating distribution
ggplot(ratings, aes(x=rating)) +
  geom_histogram(binwidth=0.5, fill="blue", color="white") +
  labs(title="Rating Distribution", x="Rating", y="Count")

# Interpretation of rating distribution:

# Most Frequent Rating: 4
# Second Most Frequent Rating: 3
# Less Frequent Ratings: 5, 2, 1
# Summary: The histogram shows users generally rate items positively, with a tendency for high ratings (4 and 5). Lower ratings (1 and 2) are less common, suggesting that extreme dissatisfaction is rare.
