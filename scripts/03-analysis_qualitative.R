# Load required libraries
library(tidyverse)
library(stringr)
library(tidytext)
library(syuzhet)
library(ggplot2)
library(tidyr)
library(wordcloud)

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

# Function to calculate sentiment
analyze_sentiment <- function(question_col, question_name) {
  sentiments <- get_nrc_sentiment(data_clean[[question_col]])
  
  pos <- sum(sentiments$positive)
  neg <- sum(sentiments$negative)
  neutral <- nrow(sentiments) - sum(rowSums(sentiments[, c("positive", "negative")]) > 0)
  
  sentiment_results <<- rbind(sentiment_results, data.frame(
    Question = question_name,
    Positive = pos,
    Negative = neg,
    Neutral = neutral
  ))
}

# Apply the function to all relevant questions
question_names <- c("Q3", "Q6", "Q8", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17")

for (qn in question_names) {
  analyze_sentiment(qn, qn)
}

# Reshape the data to long format for plotting
sentiment_long <- sentiment_results %>%
  pivot_longer(cols = c("Positive", "Negative", "Neutral"), 
               names_to = "Sentiment", 
               values_to = "Count")

# Set correct order for x-axis
sentiment_long$Question <- factor(sentiment_long$Question, levels = question_names)

# Plot
ggplot(sentiment_long, aes(x = Question, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Positive" = "darkgreen", "Negative" = "red", "Neutral" = "gray")) +
  theme_minimal() +
  labs(title = "Sentiment Distribution by Survey Question",
       x = "Survey Question",
       y = "Sentiment Count")