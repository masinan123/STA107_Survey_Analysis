#### Preamble ####
# Purpose: Filtering qualitative data from STA107 post-course survey
# Author: Sinan Ma and Jaiditya Dev
# Email: sinan.ma@mail.utoronto.ca
# Date: 9 April 2025

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Load cleaned data ####
survey_clean <- read_csv("data/clean_data.csv")

#### Filter qualitative data ####
qual_data <- survey_clean |> 
  select(-starts_with("x4911959"),
         -starts_with("x4911960"),
         -starts_with("x4911962"),
         -starts_with("x4911963"),
         -starts_with("x4911996"),
         -starts_with("x4911965")) |> 
  filter(if_any(everything(), ~ !is.na(.)))

#### Save filtered data ####
write_csv(
  x = qual_data,
  file = "data/qualitative_analysis_data.csv"
)
