#### New Code ####
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(tidytext)

# 1. Read the CSV file (assumes that columns have already been cleaned and renamed)
qual_data <- read_csv("data/cleaned_qualitative_data.csv")

# 2. (If needed) Rename columns – here we assume your file has columns Q3, Q6, ... already.
#    Otherwise, you would use rename() and select(starts_with("Q")) as before.

# 3. Pivot the data into long format
question_names <- c("Q3", "Q6", "Q8", "Q10", "Q13", "Q15", "Q16", "Q17")
qual_long <- qual_data %>%
  pivot_longer(cols = all_of(question_names),
               names_to = "question",
               values_to = "response") %>%
  filter(!is.na(response) & response != "")

# 4. Apply an extended coding scheme
qual_coded <- qual_long %>%
  mutate(theme = case_when(
    # --- Generic uninformative responses (across all questions) ---
    str_detect(response, regex("^\\s*(good|okay|fine|neutral)\\s*$", ignore_case = TRUE)) 
    ~ "Generic Response",
    
    #############################
    ## Q3: Statistical Communication Skills
    #############################
    # Enhanced Communication (positive)
    question == "Q3" &
      str_detect(response, regex("communicat|explain|express|clarif", ignore_case = TRUE)) &
      !str_detect(response, regex("confus|unclear|struggl|diffic", ignore_case = TRUE)) 
    ~ "Q3 - Enhanced Communication",
    
    # Communication Challenges (negative)
    question == "Q3" &
      str_detect(response, regex("confus|unclear|struggl|diffic", ignore_case = TRUE)) 
    ~ "Q3 - Communication Challenges",
    
    #############################
    ## Q6: R Statistical Skills
    #############################
    # R Proficiency (positive)
    question == "Q6" &
      str_detect(response, regex("\\bR\\b|code|function|package|script|improv|succeed", ignore_case = TRUE)) &
      !str_detect(response, regex("error|bug|issu|struggl|crash|overwhelm|lack practice", ignore_case = TRUE)) 
    ~ "Q6 - R Proficiency",
    
    # R Technical Challenges (negative)
    question == "Q6" &
      str_detect(response, regex("error|bug|issu|struggl|crash|overwhelm", ignore_case = TRUE)) 
    ~ "Q6 - R Technical Challenges",
    
    # Need More Practice for R (specific mention)
    question == "Q6" &
      str_detect(response, regex("lack practice|need more (practice|example)|not enough practice", ignore_case = TRUE)) 
    ~ "Q6 - Need More Practice",
    
    #############################
    ## Q8: Understanding of Concepts
    #############################
    # Deepened Understanding (positive)
    question == "Q8" &
      str_detect(response, regex("understand|master|clear|makes sense|clarif", ignore_case = TRUE)) &
      !str_detect(response, regex("confus|unclear|struggl|lost|not sure", ignore_case = TRUE)) 
    ~ "Q8 - Deepened Understanding",
    
    # Conceptual Confusion (negative)
    question == "Q8" &
      str_detect(response, regex("confus|unclear|struggl|lost|hard to grasp|not sure", ignore_case = TRUE)) 
    ~ "Q8 - Conceptual Confusion",
    
    # Insufficient Learning / Practice (new condition)
    question == "Q8" &
      str_detect(response, regex("didn'?t learn|not enough practice|didn'?t do enough", ignore_case = TRUE))
    ~ "Q8 - Insufficient Learning",
    
    # Mixed Understanding (if partly clear and partly confused)
    question == "Q8" &
      str_detect(response, regex("(some parts|sometimes) .*?(clear|confus)", ignore_case = TRUE)) 
    ~ "Q8 - Mixed Understanding",
    
    #############################
    ## Q10: Weekly Tutorials
    #############################
    # Effective Tutorials (positive)
    question == "Q10" & 
      str_detect(response, regex("helpful|reinforc|engaging|beneficial|useful|well[- ]?structured", ignore_case = TRUE)) &
      !str_detect(response, regex("boring|too fast|disorganize|inefficient|not help|confus|no time|short session|overcrowd|zoom problem|technical issu", ignore_case = TRUE)) 
    ~ "Q10 - Effective Tutorials",
    
    # Tutorial Challenges (negative)
    question == "Q10" & 
      str_detect(response, regex("boring|too fast|disorganize|inefficient|not help|confus|no time|short session|overcrowd|zoom problem|technical issu", ignore_case = TRUE)) 
    ~ "Q10 - Tutorial Challenges",
    
    #############################
    ## Q13: Real-World Relevance
    #############################
    # Effective Real-World Application (positive)
    question == "Q13" &
      str_detect(response, regex("real[- ]?world|real[- ]?life|relevan|practical|applic", ignore_case = TRUE)) &
      !str_detect(response, regex("forced|irrelevan|useless|inadequate", ignore_case = TRUE))
    ~ "Q13 - Effective Real-World Application",
    
    # Critique of Examples (negative)
    question == "Q13" &
      str_detect(response, regex("forced|irrelevan|useless|inadequate", ignore_case = TRUE)) 
    ~ "Q13 - Critique of Examples",
    
    #############################
    ## Q15: Collaboration on Infographic Assignment
    #############################
    # Positive Collaboration (teamwork)
    question == "Q15" &
      str_detect(response, regex("teamwork|collab|group|peer|supportive|good synergy|help each other", ignore_case = TRUE)) &
      !str_detect(response, regex("uneven|lack of participat|conflict|disagree|friction", ignore_case = TRUE))
    ~ "Q15 - Positive Collaboration",
    
    # Collaboration Challenges (issues in group dynamics)
    question == "Q15" &
      str_detect(response, regex("uneven|lack of participat|conflict|disagree|friction", ignore_case = TRUE)) 
    ~ "Q15 - Collaboration Challenges",
    
    # Preference for Individual Work (new condition)
    question == "Q15" &
      str_detect(response, regex("alone|individual", ignore_case = TRUE))
    ~ "Q15 - Preference for Individual Work",
    
    #############################
    ## Q16: TA Support
    #############################
    # Positive TA Support
    question == "Q16" &
      str_detect(response, regex("helpful|available|support|responsive|encourag|timely feedback", ignore_case = TRUE)) &
      !str_detect(response, regex("unhelpful|absent|slow|didn.?t respond|never respond|rude|dismiss", ignore_case = TRUE)) 
    ~ "Q16 - Positive TA Support",
    
    # TA Support Issues (negative)
    question == "Q16" &
      str_detect(response, regex("unhelpful|absent|slow|didn.?t respond|never respond|rude|dismiss", ignore_case = TRUE)) 
    ~ "Q16 - TA Support Issues",
    
    #############################
    ## Q17: Overall Course Experience
    #############################
    # Course Satisfaction (positive)
    question == "Q17" &
      str_detect(response, regex("satisf|enjoy|great|like|loved|good experience|recommend", ignore_case = TRUE)) &
      !str_detect(response, regex("improv|lack|disappoint|not good|could be better", ignore_case = TRUE)) 
    ~ "Q17 - Course Satisfaction",
    
    # Course Improvement Suggestions (explicit critiques/suggestions)
    question == "Q17" &
      str_detect(response, regex("improv|lack|disappoint|not good|could be better|wish|would prefer|need more|should be", ignore_case = TRUE))
    ~ "Q17 - Course Improvement Suggestions",
    
    # Mixed Satisfaction & Suggestions (both positive and suggestions)
    question == "Q17" &
      str_detect(response, regex("enjoy|like|satisf", ignore_case = TRUE)) &
      str_detect(response, regex("improv|lack|disappoint|not good|could be better|wish|need more", ignore_case = TRUE))
    ~ "Q17 - Mixed Satisfaction & Suggestions",
    
    # New condition for course pacing & scheduling (if mentioned)
    question == "Q17" &
      str_detect(response, regex("fast pace|slow pace|rushed|long lecture|timing|schedule", ignore_case = TRUE))
    ~ "Q17 - Course Pacing Issues",
    
    # New condition for assessment/grading feedback (if mentioned)
    question == "Q17" &
      str_detect(response, regex("grade|grading|score|exam|test|evaluation", ignore_case = TRUE))
    ~ "Q17 - Assessment/Grading Feedback",
    
    #############################
    ## Default (if nothing else matches)
    #############################
    TRUE ~ "Other"
  ))

# 5. Summarize the results
theme_counts <- qual_coded %>%
  count(theme, sort = TRUE)
print(theme_counts)

# 6. Plot the frequency distribution
ggplot(theme_counts, aes(x = reorder(theme, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frequencies of Identified Themes", x = "Theme", y = "Number of Responses")

