#### Preamble ####
# Purpose: Cleaning STA107 post-course survey data
# Author: Sinan Ma and Jaiditya Dev
# Email: sinan.ma@mail.utoronto.ca
# Date: 9 April 2025

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Load raw data ####
survey_raw <- read_csv("data/raw_data.csv")

#### Clean data ####
survey_clean <- survey_raw |> 
  clean_names() |> 
  select(
    -section, -section_id, -section_sis_id, -submitted, -attempt,
    -n_correct, -n_incorrect, -score,
    -starts_with("x1_")
  )

#### Save cleaned data ####
write_csv(
  x = survey_clean,
  file = "data/clean_data.csv"
)
