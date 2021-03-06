

## Function that inputs 1 column from the WHO data frame and outputs a 3 column data frame
fix_who_column <- function(z){
  library(tidyverse)
  library(stringr)
  library(dplyr)


  z<-str_replace_all(z," ", "")
  
  ## Function to create lower bound column
  get_lower_bound <- function(x){
   x <- str_extract(x, "\\[[0-9.]+\\-") %>% str_replace_all("\\[", "") %>% str_replace_all("-", "")
    x
  }

  ## Function to create upper bound column
  get_upper_bound <- function(x){
    x <- str_extract(x, "-[0-9.]+\\]") %>% str_replace_all("-", "") %>% str_replace_all("\\]", "")
    x
  }

  ## Function to tidy maternal mortality rate column
  get_mr <- function(x){
    x <- str_extract(x, "[0-9.]+\\[") %>% str_replace_all("\\[", "")
    x
  }

  mortality_rate <- 0
  lower_bound <- 0
  upper_bound <- 0
  clean_mr_df <- data.frame(mortality_rate, lower_bound, upper_bound)
  
  z <- data.frame(z)
  
  i<-1
  while(i<=nrow(z)){
    mortality_rate <- as.numeric(get_mr(z[i,]))
    lower_bound <- as.numeric(get_lower_bound(z[i,]))
    upper_bound <- as.numeric(get_upper_bound(z[i,]))
    tempdf <- data.frame(mortality_rate, lower_bound, upper_bound)
    clean_mr_df <- rbind(clean_mr_df, tempdf)
    i <- i+1
  }
  clean_mr_df <- clean_mr_df[-1,]  
  return(clean_mr_df)
}

