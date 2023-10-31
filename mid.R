
# imdb_data=read.csv("/Users/sheidamajidi/Desktop/Fall2023Courses/MGSC661/Midterm/Data/IMDB_data_Fall_2023.csv")
imdb_data=read.csv("/Users/sheidamajidi/Desktop/Fall2023Courses/MGSC661/Midterm/Data/IMDB_data_Fall_2023.csv")
attach(imdb_data) 
View(imdb_data)
###################################################################
### Test ### agian
# Lec 2

# Step 1 : Descriptive Analysis of Data:

# Visualizing variables individually:

# Summary statistics
summary(imdb_score)
summary(movie_budget)
summary(release_year)
summary(duration)
summary(aspect_ratio)
summary(nb_news_articles)
summary(actor1_star_meter)
summary(actor2_star_meter)
summary(actor3_star_meter)
summary(nb_faces)
summary(movie_meter_IMDBpro)

# Boxplots
boxplot(imdb_score)
boxplot(movie_budget)
boxplot(release_year)
boxplot(release_month)
boxplot(release_day)
boxplot(duration)
boxplot(aspect_ratio)
boxplot(nb_news_articles)
boxplot(nb_faces)
boxplot(movie_meter_IMDBpro)

# Histograms
hist(imdb_score)
hist(movie_budget)
hist(release_year)
hist(release_month)
hist(release_day)
hist(duration)
hist(aspect_ratio)
hist(nb_news_articles)
hist(nb_faces)
hist(movie_meter_IMDBpro)

## Visualizing relationships between variables
plot(imdb_score,movie_budget)
plot(imdb_score,release_year)
plot(imdb_score,release_day) # show no good
plot(imdb_score,duration)
plot(imdb_score,aspect_ratio)
plot(imdb_score,nb_news_articles)
plot(imdb_score,nb_faces)
plot(imdb_score,movie_meter_IMDBpro)

# Correlation matrix
library(corrplot)
numeric_vars = imdb_data[, sapply(imdb_data, is.numeric)]
correlations = cor(numeric_vars, use="pairwise.complete.obs")
corrplot(correlations, method="color")

# Scatterplot matrix
install.packages("GGally")
library(GGally)
selected_vars = c("imdb_score", "movie_budget", "release_year", "duration", "aspect_ratio", "nb_news_articles")
ggpairs(imdb_data[selected_vars])

# Density plot
library(ggplot2)
ggplot(imdb_data, aes(x=imdb_score)) + geom_density(fill="blue")

# Bar plot for categorical variables
ggplot(imdb_data, aes(x=release_month)) + geom_bar(fill="blue")
ggplot(imdb_data, aes(x=release_year)) + geom_bar(fill="green")
ggplot(imdb_data, aes(x=release_day)) + geom_bar(fill="salmon")
ggplot(imdb_data, aes(x=language)) + geom_bar(fill="yellow")

###################################################################

# Lec3

# Step 2: Dummy variables for categorical data

#Adding a dummy variable in the regression (language and Color):

imdb_data$language=as.factor(imdb_data$language)
imdb_data$colour_film=as.factor(imdb_data$colour_film)
attach(imdb_data)


# Two-category dummy variable (colour_film) 
# Explore the levels and counts for 'colour_film'
cat("\nLevels for colour_film:\n")
print(levels(colour_film))
cat("\nTable for colour_film:\n")
print(table(colour_film))
plot(imdb_score, colour_film, col=ifelse(colour_film=="Color", "red", "blue"))

lm_color=lm(imdb_score ~colour_film)
summary(lm_color)
plot(imdb_score, colour_film, col=ifelse(colour_film=="Color", "red", "blue"))

######################## not sure if they make sense!#########################
#b0=coef(lm_color)[1]
#b1=coef(lm_color)[2]
#b2=coef(lm_color)[3]
#abline(b0+b2, b1, col="red") ##Regression line for a house (intercept=b0+b2, slope=b1)
#abline(b0, b1, col="blue") 


# Multiple-category dummy variable (language) 
# Explore the levels and counts for 'language'
cat("Levels for language:\n")
print(levels(language))
cat("\nTable for language:\n")
print(table(language))

lm_language=lm(imdb_score ~ language)
summary(lm_language)

# Combination of two different sets of dummy variables
lm_model=lm(imdb_score ~ language + colour_film)
summary(lm_model)

###################################################################

# Lec 4

# Step 3: Exploring the four key issues of a linear regression model

install.packages("car")

## Significance in Multiple Linear Regression

reg1=lm(imdb_score~movie_budget+release_year+duration+aspect_ratio+nb_news_articles+nb_faces+movie_meter_IMDBpro)
summary(reg1)

# RESULT:        Multiple R-squared:  0.2482,	Adjusted R-squared:  0.2455 
# RESULT:        F-statistic: 90.66 on 7 and 1922 DF,  p-value: < 2.2e-16

library("car")
residualPlots(reg1)

# Remove variables with p-value < 0.1 :  movie_budget, duration, nb_news_articles, movie_meter_IMDBpro

reg2=lm(imdb_score~release_year+aspect_ratio+nb_faces)
residualPlots(reg2)

# Remove release_year (p-value < 0.1)

reg3=lm(imdb_score~aspect_ratio+nb_faces)
residualPlots(reg3)

# Remove aspect_ratio (p-value < 0.1)

reg4=lm(imdb_score~nb_faces)
residualPlots(reg4)

# still p-value <0.1 : no linear relationship!

###################################################################

# Lec 5

# Step 4: Exploring polynomial regression model

# Step 4-1 : Polynomial regression of degree 2 to 6 - between each two variables

# Remove non-numeric columns for simplicity.
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# Remove 'imdb_score' from the columns to be regressed upon, as it's the response variable.
numeric_data$imdb_score = NULL

# Loop through each column and fit polynomial regression models
for (col_name in names(numeric_data)) {
  cat(paste("\n\nRegression for:", col_name, "\n"))
  
  for (degree in 2:6) {
    formula_str = paste("imdb_score ~ poly(", col_name, ",", degree, ", raw=TRUE)")
    model = lm(as.formula(formula_str), data=imdb_data)
    cat(paste("\nDegree:", degree, "\n"))
    print(summary(model))
  }
}


#################################################### R-square matrix:

# Remove non-numeric columns for simplicity. If you want to include factors, you'd need to handle them separately.
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols = c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime")

# Drop the specified columns
numeric_data = numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Remove 'imdb_score' from the columns to be regressed upon, as it's the response variable.
numeric_data$imdb_score = NULL

# Initialize a matrix to store R-squared values
r_squared_matrix = matrix(0, nrow=length(names(numeric_data)), ncol=5,
                           dimnames=list(names(numeric_data), 2:6))

# Loop through each column and fit polynomial regression models
for (col_name in names(numeric_data)) {
  
  for (degree in 2:6) {
    formula_str = paste("imdb_score ~ poly(", col_name, ",", degree, ", raw=TRUE)")
    model = lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the matrix
    r_squared_matrix[col_name, as.character(degree)] = summary(model)$r.squared
  }
}

# Convert matrix to a long-format data frame for easy sorting and grouping
library(tidyr)
r_squared_df = as.data.frame(as.table(r_squared_matrix))
colnames(r_squared_df) = c("Variable", "Degree", "R_Squared")

# Group by variable and select the highest R-squared for each variable
library(dplyr)
best_r_squared_per_variable = r_squared_df %>%
  group_by(Variable) %>%
  top_n(1, wt=R_Squared)

# Print the sorted dataframe
print(best_r_squared_per_variable)

# Step 4-2 : Polynomial regression of degree 2 to 6 - between each three variables

# Remove non-numeric columns and the specified columns
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols = c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data = numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 3 variables
combinations = combn(names(numeric_data), 3)

# Initialize a data frame to store R-squared values
r_squared_df = data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo = combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms = sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, ")")
    })
    poly_terms = paste(terms, collapse = " + ")
    formula_str = paste("imdb_score ~", poly_terms)
    
    model = lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df = rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Sort by R-squared in descending order
library(dplyr)
sorted_r_squared_df = r_squared_df %>%
  arrange(desc(R_Squared))
#test
# Print the sorted dataframe
print(sorted_r_squared_df)

######################################### Group each set together

# Remove non-numeric columns and the specified columns
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols = c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 3 variables
combinations=combn(names(numeric_data), 3)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df <- rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
library(dplyr)
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

# Step 4-3: Polynomial regression of degree 2 to 6 - between each four variables

# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 4 variables
combinations=combn(names(numeric_data), 4)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, ")*I(", combo[4], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Degree=degree, R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
library(dplyr)
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

# Step 4-4: five variables


# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 5 variables
combinations=combn(names(numeric_data), 5)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Variable5=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, 
             ")*I(", combo[4], "^", d, ")*I(", combo[5], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Variable5=combo[5], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
library(dplyr)
best_r_squared_per_combo <- r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4, Variable5) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

# Step 4-5: six variables


# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 6 variables
combinations=combn(names(numeric_data), 6)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Variable5=character(0), Variable6=character(0), 
                           Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, 
             ")*I(", combo[4], "^", d, ")*I(", combo[5], "^", d, ")*I(", combo[6], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Variable5=combo[5], Variable6=combo[6], 
                                                   Degree=degree, R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
library(dplyr)
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4, Variable5, Variable6) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

# Step 4-6: Seven variables

# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 7 variables
combinations=combn(names(numeric_data), 7)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Variable5=character(0), Variable6=character(0),
                           Variable7=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, 
             ")*I(", combo[4], "^", d, ")*I(", combo[5], "^", d, ")*I(", combo[6], "^", d, 
             ")*I(", combo[7], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Variable5=combo[5], Variable6=combo[6], 
                                                   Variable7=combo[7], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
library(dplyr)
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4, Variable5, Variable6, Variable7) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)
