

library(tidyverse)
library(readr)
# Function for unfollowing
source('unfollow.R')
source('follow.R')
source('follow_and_unfollow.R')
source('download_all_tweets.R')

person <- 'boye_g'

# Get a list of people whom Rivera follows
# system(paste0('python3 get_following_details.py ', person))
# system(paste0('python3 get_following_simple.py ', person))
system(paste0('python3 get_followers_simple.py ', person))

# person_follows <- read_csv(paste0('data/', person, '_is_following_simple.csv'))
person_is_followed_by <- read_csv(paste0('data/', person, '_is_followed_by_simple.csv'))


# Get list of people I'm following
i_follow <- read_csv('data/joethebrew_is_following.csv') %>%
  mutate(p = followers / following) %>%
  arrange(desc(followers))

# Get the usernames of those who I don't already follow
to_follow <- person_is_followed_by %>%
  filter(!username %in% i_follow$username) %>%
  .$username

# Follow and then unfollow each
follow_and_unfollow(users = to_follow)



# # Get list of people I'm following
# i_follow <- read_csv('data/joethebrew_is_following.csv') %>%
#   mutate(p = followers / following) %>%
#   arrange(desc(followers))
# 
# # Get list of people who are following me
# following_me <- read_csv('data/joethebrew_is_followed_by.csv') %>%
#   mutate(p = followers / following) %>%
#   arrange(desc(followers))
# 
# # Don't remove these
# dont_remove <- c('jmfarrep', 'WillMcAvoy9', 'dylanfried', '202cycle', 'TrullolsAdv', 'OTRO_KNHI18', 'beekeeper_creek',
#                  'marathon254', 'xingcbrew', 'edmondps8', 'AndreuLlop', 'ClicksxREPCAT', 'rhdearosa', 'thundercore84', 
#                  'jordimfb', 'ptlong11', 'TanfurLeGuy', '_jpastor', 'mariapastor25', 'shreyakbhatt', 'jaumepoto', 
#                  'data_brew', 'benmbrew', 'Laiaborrasmarti', 'CarlesValbuena', 'willmyfriend', 'casc_SCQ', 'opis76',
#                  'LucyFerMon', 'cryptocatalunya', 'TeJodesyBailas7', 'rosamaria211972', 'acolverson1','Manhica_CISM',
#                  'socanallanes', 'JordiAldoma', 'DrMennoSmit', 'St_E_du_Chesnoy', 'Bernatxelmundo', 'matthewsmp',
#                  'BabelRepCat_DE', 'watsonwilliamb', 'Baywatch_esteve','fundacionjbosca', 'lvcandler',
#                  'Xeviplay','XavierGEltroll', 'bcn_drone', 'tribuddha',  'OzzieMontse', 'Painterman1964',
#                  'jnalavedra','2iTwits','miquelcasals4','Jabbastation','EuroPH','Jonsilman1','FreeRovira','xuletti',
#                  'anyelinehg','cbaguiles','dmalmusi','Dylaannn1','thomasbrew','cfageda','vidira4444','ConsellConca',
#                  'JordiBalaguer','JaumePalafugell', 'FreeForcadell', 'free_catalan','PilotPyrx', 'SergiAlcazar',
#                  'QuantumAspect','ANC_Conca','UESantaColoma','Ciudadanos','PECE_Oficial','HolaTabarnia',
#                  'PDeCATCornella','DavidBCoe','adrisancast','FundacioCEO','nuriacoe','RobertHMerriman', 'polpastor',
#                  'BabelRepCat_EN', 'FrancoTabarnia')
# 
# # Remove those with less than n followers and not in above
# n <- 3314
# 
# remove_these <- i_follow %>%
#   filter(followers <= 3314) %>%
#   .$username
# remove_these <- remove_these[!remove_these %in% dont_remove]
# 
# unfollow(users = remove_these,
#          sleep = c(1,5))
