# Load required libraries
library(tidyverse)         # For data manipulation (includes dplyr, ggplot2, etc.)
library(stringr)           # For string handling like trimming, pattern matching
library(tidytext)          # For text mining and tokenization
library(syuzhet)           # For sentiment analysis using NRC lexicon
library(ggplot2)           # For creating visualizations (e.g., bar plots)
library(tidyr)             # For data reshaping (e.g., pivot_longer)
library(wordcloud)         # For creating word cloud visualizations

# Read in the survey data
data <- read.csv("data/qualitative_analysis_data.csv", stringsAsFactors = FALSE)

# Rename columns
colnames(data)[1:11] <- c("Q3", "Q6", "Q8", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17")

# Clean the text
data_clean <- data %>%
  mutate(across(everything(), ~ str_trim(tolower(as.character(.)))))

# Initialize an empty data frame to collect sentiment results
sentiment_results <- data.frame(
  Question = character(),
  Positive = numeric(),
  Negative = numeric(),
  Neutral = numeric(),
  stringsAsFactors = FALSE
)

# Define function to calculate sentiment per question
analyze_sentiment <- function(question_col, question_name) {
  sentiments <- get_nrc_sentiment(data_clean[[question_col]])
  
  # Calculate positive and negative
  pos <- sum(sentiments$positive)
  neg <- sum(sentiments$negative)
  
  # Define neutral as total rows - (pos > 0 or neg > 0)
  neutral <- nrow(sentiments) - sum(rowSums(sentiments[, c("positive", "negative")]) > 0)
  
  # Append to results
  sentiment_results <<- rbind(sentiment_results, data.frame(
    Question = question_name,
    Positive = pos,
    Negative = neg,
    Neutral = neutral
  ))
}

# Define the desired order of questions
ordered_questions <- c("Q3", "Q6", "Q8", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17")

# Convert the 'Question' column to a factor with specified levels
sentiment_long$Question <- factor(sentiment_long$Question, levels = ordered_questions)

# Plot with corrected order
ggplot(sentiment_long, aes(x = Question, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Positive" = "darkgreen", "Negative" = "red", "Neutral" = "gray")) +
  theme_minimal() +
  labs(title = "Sentiment Distribution by Survey Question",
       x = "Survey Question",
       y = "Sentiment Count")

