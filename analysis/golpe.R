library(tidyverse)
library(readr)

# Define people
people <- c('Albert_Rivera',
            'InesArrimadas',
            'miqueliceta',
            'pablocasado_',
            'Santi_ABASCAL',
            'Societatcc',
            'vox_es',
            'Albiol_XG',
            'GirautaOficial',
            'jordi_canyas',
            'anarosaq',
            'sanchezcastejon',
            'Eva_Granados',
            'carrizosacarlos'
            )

# for(i in 1:length(people)){
#   person <- people[i]
#   message('Working on ', person)
#   log <- capture.output({system(paste0('python3 ../../twint/Twint.py -u ', person,
#          ' -o ../data/',
#          person,
#          '_tweets.csv --csv'))})
#   # Sys.sleep(30)
# }

# Get tweets
out_list <- list()
for(i in 1:length(people)){
  person <- people[i]
  data <- read_csv(paste0('../data/', person, '_tweets.csv'))
  out_list[[i]] <- data %>% mutate(person = person)
}
tweets <- bind_rows(out_list)
tweets <- tweets %>%
  dplyr::distinct(id, .keep_all = TRUE)

# Group by person / month and get data
search <- function(x){
  grepl('violen', tolower(x)) &
    !grepl(' eta', tolower(x)) &
    !grepl('eta.', tolower(x), fixed = TRUE) &
    !grepl('eta,', tolower(x), fixed = TRUE) &
    
    !grepl('e.t.a.', tolower(x),
           fixed = TRUE) &
    !grepl('eta ', tolower(x)) &
    !(grepl('terrori', tolower(x))) &
    !grepl('mujer', tolower(x)) &
    !grepl('dona|dones', tolower(x)) &
    !grepl('género', tolower(x)) & 
    !grepl('machis', tolower(x)) &
    !grepl('gèner', tolower(x))
}

# search <- function(x){
#   grepl('violen', tolower(x))
# }

z <- tweets %>%
  mutate(coup = search(tweet)) 
View(z %>% filter(coup))

df <- z %>%
  group_by(person,
           date = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
  summarise(numerator = length(which(coup)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)

library(databrew)
x <- df %>%
  filter(person %in% c('Albert_Rivera',
                       # 'anarosaq'
                       
                       'InesArrimadas',
                       'miqueliceta',
                       # 'pablocasado_',
                       'Santi_ABASCAL',
                       # 'Eva_Granados',
                       'carrizosacarlos'
                       # 'Societatcc',
                       # 'vox_es'
                       # 'Albiol_XG',
                       # 'GirautaOficial',
                       # 'GirautaOficial'#,
                       # 'jordi_canyas'#,
                       # 'anarosaq',
                       # 'sanchezcastejon'
  )
         ) %>%
  filter(date >= '2017-01-01') #%>%
  # mutate(person = ifelse(grepl('ivera', person), 'Albert Rivera',
                         # ifelse(grepl('iceta', person), 'Miquel Iceta', NA)))

dict <- data.frame(person = people,
                   new_person = 
                     c('Albert Rivera',
                       'Ines Arrimadas', 
                       'Miquel Iceta', 
                       'Pablo Casado',
                       'Santi Abascal',
                       'Societat Civil Catalan',
                       'VOX',
                       'Xavier Garcia Albiol',
                       'Juan Carlos Girauta',
                       'Jordi Cañas',
                       'Ana Rosa Quintana',
                       'Pedro Sánchez',
                       'Eva Granados',
                       'Carlos Carrizosa'))
x <- x %>% left_join(dict) %>%
  dplyr::select(-person) %>%
  dplyr::rename(person = new_person) #%>%
  # filter(!person %in% c('VOX', 'Santi Abascal'))
library(scales)
ggplot(data = x,
       aes(x = date,
           y = numerator)) +
  geom_line(aes(group = person,
                color = person),
            alpha = 0.7) +
  geom_point(aes(group = person,
                 color = person),
             alpha = 0.7) +
  theme_databrew() +
  scale_color_manual(name = '',
                     values = make_colors(n = length(unique(x$person)),
                                          categorical = TRUE)) +
  labs(x = '',
       y = 'Tweets',
       title = 'Tuits conteniendo las palabras "violencia/violento"*',
       subtitle = 'Y variaciones como "violentos", "violents", etc.; Tuits públicos',
       caption = '*Excluye a los tuits que también contienen las palabras:\n"ETA", "mujer", "dona", "género", "machismo", "terrorism" (y similar/variaciones)') +
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 15)) +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = sort(unique(x$date))) +
  scale_y_continuous(breaks = seq(0, 1000, by = 2)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 19),
        plot.subtitle = element_text(size = 15),
        axis.title.y  = element_text(size = 20),
        plot.caption = element_text(size = 12,
                                    color = 'darkgrey',
                                    hjust = 0))

#¿Por qué no comenzaron a hablar de la "violencia" del procés hasta marzo de 2018, si los eventos violentos tuvieron lugar en septiembre-octubre de 2017?

#¿No sería que el relato de la "violencia" corresponde más al calendario judicial alemán que a la realidad? 

# EL PROCES HA MUERTO
# Spanish date
spanish_date <- function(x){
  part2 <- format(x, '%d')
  part3 <- format(x, '%Y')
  part1 <- data.frame(x = as.numeric(format(x, '%m')))
  right <- data_frame(x = 1:12,
                      y = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun',
                            'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'))
  joined <- left_join(part1, right) %>%
    .$y
  out <- paste0(part2, ' ', joined, ' ', part3)
  out
}


search <- function(x){
  grepl('muerto|mort|acabat|acabado|acabó|terminado|agon|terminó|terminando', tolower(x)) &
    grepl('procès|prucès|procés|prucés|proceso', tolower(x))
}

df <- tweets %>%
  mutate(coup = search(tweet)) %>%
  filter(coup) %>%
  arrange(date) %>%
  mutate(date = spanish_date(date)) %>%
  mutate(x = paste0('-', date, ': ', person, ', ', tweet))
x <- df %>%
  filter(date %in% c('05 Ago 2014',
                     '17 Jun 2018',
                     '04 Jul 2018',
                     '16 Abr 2015',
                     '09 Ene 2016',
                     '09 Jun 2016',
                     '23 Mar 2018',
                     '22 Mar 2018',
                     '25 Sep 2017',
                     '31 Oct 2017',
                     '23 Mar 2018')) %>%
  dplyr::select(date, person, tweet, link)# %>% .$link 
cat(paste0(gsub('https://', '', x$link), collapse = '\n'))
cat(paste0(df$x, collapse = '\n\n'))

group_by(person,
           date = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
  summarise(numerator = length(which(coup)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)


# LLACOS GROCS
df <- tweets %>%
  mutate(puig = grepl('puigd', tolower(tweet))) %>%
  mutate(lazo = grepl('lazo|llaç', tolower(tweet))) %>%
  group_by(person,
           date = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
  summarise(numerator_puig = length(which(puig)),
            numerator_lazo = length(which(lazo)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p_puig = numerator_puig / denominator * 100,
         p_lazo = numerator_lazo / denominator * 100)

ggplot(data =df %>%
         filter(person %in% c('Albert_Rivera', 
                              'pablocasado_',
                              'miqueliceta',
                              'InesArrimadas',
                              'Santi_ABASCAL'
         )) %>%
         filter(date >= '2017-06-01'),
       aes(x = date,
           y = p_lazo)) +
  geom_line(aes(group = person,
                color = person)) +
  geom_point(aes(group = person,
                 color = person)) 






search <- function(x){
  grepl('suprema|racis', tolower(x))
}

z <- tweets %>%
  mutate(coup = search(tweet)) 
View(z %>% filter(coup))

df <- z %>%
  group_by(person,
           date = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
  summarise(numerator = length(which(coup)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)

library(databrew)
x <- df %>%
  filter(person %in% c('Albert_Rivera',
                       # 'anarosaq'
                       
                       'InesArrimadas',
                       'miqueliceta',
                       'pablocasado_',
                       # 'Santi_ABASCAL',
                       # 'Eva_Granados',
                       # 'carrizosacarlos'
                       # 'Societatcc',
                       # 'vox_es',
                       # 'Albiol_XG',
                       'GirautaOficial',
                       # 'GirautaOficial'#,
                       # 'jordi_canyas'#,
                       # 'anarosaq',
                       'sanchezcastejon'
  )
  ) %>%
  filter(date >= '2016-01-01') #%>%
# mutate(person = ifelse(grepl('ivera', person), 'Albert Rivera',
# ifelse(grepl('iceta', person), 'Miquel Iceta', NA)))

dict <- data.frame(person = people,
                   new_person = 
                     c('Albert Rivera',
                       'Ines Arrimadas', 
                       'Miquel Iceta', 
                       'Pablo Casado',
                       'Santi Abascal',
                       'Societat Civil Catalan',
                       'VOX',
                       'Xavier Garcia Albiol',
                       'Juan Carlos Girauta',
                       'Jordi Cañas',
                       'Ana Rosa Quintana',
                       'Pedro Sánchez',
                       'Eva Granados',
                       'Carlos Carrizosa'))
x <- x %>% left_join(dict) %>%
  dplyr::select(-person) %>%
  dplyr::rename(person = new_person) #%>%
# filter(!person %in% c('VOX', 'Santi Abascal'))
library(scales)
library(databrew)
ggplot(data = x,
       aes(x = date,
           y = numerator)) +
  geom_line(aes(group = person),
            alpha = 0.7,
            color = 'black') +
  geom_area(aes(fill = person)) +
  # geom_point(aes(group = person,
  #                color = person),
  #            alpha = 0.7) +
  facet_wrap(~person) +
  theme_databrew() +
  scale_color_manual(name = '',
                     values = make_colors(n = length(unique(x$person)),
                                          categorical = TRUE)) +
  scale_fill_manual(name = '',
                     values = make_colors(n = length(unique(x$person)),
                                          categorical = TRUE)) +
  labs(x = '',
       y = 'Tuits',
       title = 'Tuits con palabras "supremacista/racista"',
       caption = 'Y variaciones "supremacismo/racismo". Tuits públicos, capturados el 23 de Agosto de 2018.') +
  # scale_x_date(labels = date_format("%b %Y"),breaks = sort(unique(x$date[format(x$date, '%m') %in%
  #                                             c('01','07')]))) +
  # scale_y_continuous(breaks = seq(0, 1000, by = 2)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.text.x = element_text(#angle = 90, 
                                   size = 12),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 21),
        plot.subtitle = element_text(size = 15),
        axis.title.y  = element_text(size = 20),
        plot.caption = element_text(size = 9,
                                    color = 'darkgrey',
                                    hjust = 0),
        strip.text = element_text(size = 15,
                                  family = 'Sawasdee'),
        strip.background = element_rect(color = NA, fill = NA)) +
  theme(legend.position = 'none') +
  geom_vline(xintercept = seq(as.Date('2016-01-01'),
                              as.Date('2018-01-01'),
                              by = 'year'),
             alpha = 0.4)

