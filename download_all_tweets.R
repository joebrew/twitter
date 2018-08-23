library(tidyverse)
library(readr)

download_all_tweets <- function(person = 'Albert_Rivera'){
  
  file_name <- paste0('data/',
                      person,
                      '_tweets.csv')
  
  # Get all of that persons tweets
  system(paste0('python3 ../twint/Twint.py -u ',
                person,
                ' -o ',
                file_name,
                ' --csv'))
  
  message('Saved data to ', file_name)
  
  # # Read in those tweets
  # df <- read_csv(paste0('data/', person, '_tweets.csv'))
}
