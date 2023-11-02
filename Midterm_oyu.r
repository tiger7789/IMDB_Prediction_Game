##################################################################################
################################ Figure 1. HISTOGRAM (from Sheida) ###############
##################################################################################
# Load dataset
imdb_data <- read_csv("C:/Users/oyund/OneDrive/Desktop/IMDB_data_Fall_2023.csv")

# Load libraries
library(tidyverse)
library(ggplot2)

# Histogram for 'movie_budget'
# Transforming movie_budget to 'Million' scale
imdb_data$movie_budget_M=imdb_data$movie_budget / 1e6

# Histogram plot for movie_budget in 'Million' scale
ggplot(imdb_data, aes(x=movie_budget_M)) + 
  geom_histogram(binwidth=10, fill="skyblue", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Budget (in Million scale)", x="Movie Budget (in thousands)", y="Count") +
  theme_minimal()

# Histogram for 'duration'
# Transforming duration to 'Hour' scale
imdb_data$duration_hour=imdb_data$duration / 60

# Histogram plot for duration in 'Hour' scale
ggplot(imdb_data, aes(x=duration_hour)) + 
  geom_histogram(binwidth=0.5, fill="lightgreen", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Duration (in Hour scale)", x="Movie Duration (in hours)", y="Count") +
  theme_minimal()


# Histogram for 'nb_news_articles'
ggplot(imdb_data, aes(x=nb_news_articles)) + 
  geom_histogram(binwidth=10, fill="coral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of News Articles", x="Number of Articles", y="Count") +
  theme_minimal()

# Histogram plot for nb_faces
ggplot(imdb_data, aes(x=nb_faces)) + 
  geom_histogram(binwidth=1, fill="lightcoral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of Faces in Movie Posters", x="Number of Faces", y="Count") +
  theme_minimal()

# Histogram plot for release_year
ggplot(imdb_data, aes(x=release_year)) + 
  geom_histogram(binwidth=1, fill="red", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Release Years", x="Release Year", y="Count") +
  theme_minimal()

library(ggplot2)
library(gridExtra)

# Preparing the data
imdb_data$movie_budget_M=imdb_data$movie_budget / 1e6
imdb_data$duration_hour=imdb_data$duration / 60

# Creating individual plots
p1=ggplot(imdb_data, aes(x=imdb_score)) + 
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) +
  labs(title="Distribution of IMDb Scores", x="IMDb Score", y="Count")

p2=ggplot(imdb_data, aes(x=movie_budget_M)) + 
  geom_histogram(binwidth=10, fill="skyblue", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Budget (in Million scale)", x="Movie Budget (in millions)", y="Count")

p3=ggplot(imdb_data, aes(x=duration_hour)) + 
  geom_histogram(binwidth=0.5, fill="lightgreen", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Duration (in Hour scale)", x="Movie Duration (in hours)", y="Count")

p4=ggplot(imdb_data, aes(x=nb_news_articles)) + 
  geom_histogram(binwidth=10, fill="coral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of News Articles", x="Number of Articles", y="Count")

p5=ggplot(imdb_data, aes(x=nb_faces)) + 
  geom_histogram(binwidth=1, fill="lightcoral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of Faces in Movie Posters", x="Number of Faces", y="Count")

p6=ggplot(imdb_data, aes(x=release_year)) + 
  geom_histogram(binwidth=1, fill="red", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Release Years", x="Release Year", y="Count")

# Arranging the plots in a 2x3 grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2)


##################################################################################
################################Figure 2. CORRELATION MATRIX #####################
##################################################################################
# Load dataset
imdb_df <- read.csv("/Users/macbookpro/Desktop/lxc/git/IMDB_Prediction_Game/IMDB_data.csv")

# Load libraries
library(ggplot2)
library(ggcorrplot)

# List of numeric variables
numeric_vars <- c('movie_id', 'imdb_score', 'movie_budget', 'release_day', 'release_year',
                  'duration', 'aspect_ratio', 'nb_news_articles', 'actor1_star_meter',
                  'actor2_star_meter', 'actor3_star_meter', 'nb_faces', 'action',
                  'adventure', 'scifi', 'thriller', 'musical', 'romance', 'western',
                  'sport', 'horror', 'drama', 'war', 'animation', 'crime',
                  'movie_meter_IMDBpro')

# Calculate correlation matrix
corr_matrix <- cor(df[, numeric_vars], use = "complete.obs")

# Plot heatmap of the correlation matrix
ggcorrplot(corr_matrix, hc.order = TRUE, 
           type = "full", lab = FALSE, 
           outline.col = "white", 
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) + 
  ggtitle('Correlation Matrix of Numeric Variables')


##################################################################################
################################ Figure 3. SCATTERPLOTS ##########################
##################################################################################
# Load dataset
imdb_df <- read.csv("/Users/macbookpro/Desktop/lxc/git/IMDB_Prediction_Game/IMDB_data.csv")

# Load libraries
library(tidyverse)
library(ggplot2)

# Transform actor1_star_meter and movie_meter_IMDBpro like the following:
imdb_df$inverse_actor_ranking = 1/(imdb_df$actor1_star_meter)
imdb_df$inverse_movie_ranking = 1/(imdb_df$movie_meter_IMDBpro)

# Scatter plot for imdb_score vs release_year
p1 <- ggplot(imdb_df, aes(x=release_year, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Release Year", x="Release Year", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs duration
p2 <- ggplot(imdb_df, aes(x=duration, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Duration", x="Duration", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs movie_budget
p3 <- ggplot(imdb_df, aes(x=movie_budget, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Movie Budget", x="Movie Budget", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs movie_meter
p4 <- ggplot(imdb_df, aes(x=imdb_df$inverse_movie_ranking, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Inverse of Movie Meter", x="Movie Meter", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs nb_faces
p5 <- ggplot(imdb_df, aes(x=nb_faces, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Number of Faces", x="Number of Faces", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs inverse of actor 1 meter
p6 <- ggplot(imdb_df, aes(x = imdb_df$inverse_actor_ranking, y = imdb_score)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "IMDb Score vs Inverse of Actor 1 Meter", x = "Actor 1 Meter") + 
  theme_minimal() + 
  theme(legend.position = "none")

# Scatter plot for imdb_score vs number of news articles
p7 <- ggplot(imdb_df, aes(x = imdb_df$nb_news_articles, y = imdb_score)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "IMDb Score vs Number of News Articles", x = "Number of News Articles") + 
  theme_minimal() + 
  theme(legend.position = "none")

# Arrange the plots in a grid
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=4)

#############################################################################
########################## STARGAZER TABLE FOR FINAL REGRESSION #############
#############################################################################

# Load dataset
IMDB <- read_csv("C:/Users/oyund/OneDrive/Desktop/IMDB_data_Fall_2023.csv")

# Remove outliers
IMDB2=IMDB[-c(32, 316,1806,989,1581,192,261),]

# Regression
reg15= glm(formula = imdb_score ~ bs(release_year, knots = c(quantiles),degree = 1) + 
             duration + bs(inv_movie_meter, knots = c(quantiles1), degree = 1) 
           + nb_news_articles + nb_faces+
             action + adventure + thriller + musical + romance + sport + horror + drama + war + animation + crime, 
           data = IMDB2)

# Load stargazer
library(stargazer)

# Stargazer
stargazer(reg15, type = "html")
summary(reg15)






