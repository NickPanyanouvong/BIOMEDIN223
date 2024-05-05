library(readxl)
library(Hmisc)
library(tidyverse)
library(rstudioapi)

# Adjust wd
file_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_path))

# FACTORIZE
# Helper function to factorize all categorical variables in a df, converting any "" into "Missing".
# Arguments:
#   [df] df: The dataframe to process. Assumes that missing values are representing as empty strings.
# Outputs:
#   [df] result: The processed dataframe, with all empty strings replaced with "Missing".

factorize <- function(df){
  
  # Initialize an empty vector to store categorical column names
  categorical_columns <- vector("character")
  
  # Record the categorical variables to be factorized
  for(col in names(df)) {
    if (is.character(df[[col]])) {
      categorical_columns <- c(categorical_columns, col)
    }
  }
  
  result <- df
  
  # Convert each identified column into a factor
  for(col in categorical_columns){
    unique_levels <- unique(result[[col]])
    
    if(any(unique_levels == "")){
      unique_levels <- unique_levels[unique_levels != ""] # Remove blank
      result[[col]] <- factor(result[[col]], levels = c(unique_levels, ""), labels = c(unique_levels, "Missing"))
    }else{
      result[[col]] <- factor(result[[col]], levels = unique_levels, labels = unique_levels)
    }
  }
  
  return(result)
  
}