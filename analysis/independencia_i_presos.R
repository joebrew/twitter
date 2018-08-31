# Attach libraries
library(tidyverse)
library(readr)
library(databrew)
library(tidyverse)
library(ggplot2)
# Also must have the twint python library at the
# same level as this repo (joebrew/twitter):
# https://github.com/twintproject/twint
# and installed

# Go up one level to the "data" directory
setwd('../data')

# Define people
people <- c('carlesral', 'cupnacional', 'elsa_artadi',
            'epaluzie', 'Esquerra_ERC', 'josepcosta',
            'KRLS', 'miriamnoguerasM', 'Pdemocratacat',
            'perearagones', 'QuimTorraiPla', 'yeyaboya')

# Loop through each person and collect their tweets
for(i in 1:length(people)){
  this_person <- people[i]
  message(i)
  if(!paste0(this_person, '_tweets.csv') %in% dir()){
    system(paste0("python3 ../../twint/Twint.py -u ",
                  this_person,
                  " -o data/",
                  this_person,
                  "_tweets.csv --csv"))
  }
}

# Data has now been downloaded into the data dir (current wd)
# Return to the analysis dir (dir of this script)
setwd('../analysis/')

# Read in the tweets from csvs
out_list <- list()
for(i in 1:length(people)){
  person <- people[i]
  out_list[[i]] <- read_csv(paste0('../data/', person, '_tweets.csv')) %>% 
    mutate(person = person)
}

# Combine all tweets together into one dataframe
tweets <- bind_rows(out_list) %>%
  dplyr::distinct(id, .keep_all = TRUE)

# Define function for getting the month of a date (in date format)
make_month <- function(x){
  as.Date(paste0(format(as.Date(x), '%Y-%m'), '-01'))
}

# Define functions for searching for certain terms
search_x <- function(x){
  grepl('independ', tolower(x))
}
search_y <- function(x){
  grepl('presos|preses', tolower(x))
}

# Perform the search, and get number of search-string-matched
# tweets for each month, as well as the total number of 
# tweets for each person
x <- tweets %>%
  mutate(keep_x = search_x(tweet),
         keep_y = search_y(tweet)) %>%
  group_by(date = make_month(date) ,person) %>%
  summarise(numerator_x = length(which(keep_x)),
            numerator_y = length(which(keep_y)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p_x = numerator_x / denominator * 100,
         p_y = numerator_y / denominator * 100) %>%
  arrange(date)

# Gather the data into a more plot friendly format
x_long <- tidyr::gather(x %>% dplyr::select(date, person, p_x, p_y), key, value, p_x:p_y) %>% 
  # Filter for only on or after Oct 2017
  filter(date >= '2016-10-01') %>%
  # Recode x and y to be independencia or presos
  mutate(key = ifelse(key == 'p_x', 'Independència',
                      ifelse(key == 'p_y', 'Presos', NA))) 
# Define breaks for plotting labels
date_breaks1 <- seq(as.Date('2010-01-01'),
                    as.Date('2018-01-01'),
                    by = 'year')
date_breaks2 <- seq(as.Date('2010-07-01'),
                    as.Date('2018-07-01'),
                    by = 'year')
date_breaks <- sort(unique(c(date_breaks1, date_breaks2)))[sort(unique(c(date_breaks1, date_breaks2))) >= '2016-07-01']

ggplot(data = x_long %>%
         mutate(person = paste0('@', person)),
       aes(x = date,
           y = value,
           color = key,
           group = key)) +
  geom_line(stat = 'smooth', method = 'auto', size = 1.3, alpha = 0.9) +
  geom_point(alpha = 0.8,
             size = 0.4)+
  geom_line(alpha = 0.3) +
  facet_wrap(~person, scales = 'free_y') +
  theme_databrew() +
  scale_color_manual(name = '',
                     values = c('#3492C7',
                                # '#E5c388',
                                '#FF5733')) +
  scale_x_date(#labels = scales::date_format("%b\n%Y"),
    breaks = date_breaks,
    labels = c('Jul\n2016', 'Gen\n2017', 'Jul\n2017', 'Gen\n2018', 'Jul\n2018')) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 24,
                                   color = 'darkgrey'),
        plot.title = element_text(size = 18,
                                  face = 'bold'),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        # axis.text.x = element_text(size = 22),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        strip.text = element_text(size = 15),
        plot.caption = element_text(size = 11, hjust = 0)) +
  labs(x = 'Mes',
       y = 'Percentge dels seus tuits',
       title = 'Title',
       caption = 'Incloent "independent", variacions en castellà, etc. Línies suavitzades per regressió local (loess). Oct 2016-Jul 2018.') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = date_breaks1,
             alpha = 0.3)  

# Aggregated of all the above people

x_agg <- x_long %>%
  group_by(date, key) %>%
  summarise(value = mean(value, na.rm = TRUE))
ggplot(data = x_agg,
       aes(x = date,
           y = value,
           color = key,
           group = key)) +
  geom_line(stat = 'smooth', method = 'auto', size = 1.3, alpha = 0.9) + 
  geom_point(alpha = 0.8,
             size = 0.4)+
  geom_line(size = 1.3, alpha = 0.9) +
  theme_databrew() +
  scale_color_manual(name = '',
                     values = c('#3492C7',
                                # '#E5c388',
                                '#FF5733')) +
  scale_x_date(labels = scales::date_format("%b\n%Y"),
               breaks = date_breaks) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 24,
                                   color = 'darkgrey'),
        plot.title = element_text(size = 25,
                                  face = 'bold'),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text = element_text(size = 17)) +
  labs(x = 'Mes',
       y = 'Percentge de tots els seus tuits',
       title = 'Tweets amb les paraules "independència" o "presos"',
       caption = 'Incloent variacions en castellano, "independent", "preses", etc. Línies suavitzades per regressió local (loess). 24 Agost 2018') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = date_breaks,
             alpha = 0.3)  

