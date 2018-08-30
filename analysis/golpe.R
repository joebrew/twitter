library(tidyverse)
library(readr)
library(databrew)

# Define people
people <- dir('../data')
people <- people[grepl('_tweets.csv', people, fixed = TRUE)]
people <- gsub('_tweets.csv', '', people)
# people <- people[people!= 'BeatrizTalegon']
# Get tweets
out_list <- list()
for(i in 1:length(people)){
  person <- people[i]
  out_list[[i]] <- read_csv(paste0('../data/', person, '_tweets.csv')) %>% 
    mutate(person = person)
}
tweets <- bind_rows(out_list) %>%
  dplyr::distinct(id, .keep_all = TRUE)

# # Get one person
# the_person <- c('jordi_canyas')
# person_data <- tweets %>%
#   filter(person %in% the_person)
# person_data %>%
#   filter(grepl('extrema', tolower(tweet))) %>% 
#   View
# Define parties
parties <- c('CiudadanosCs', 'CiutadansCs', 'cupnacional', 'Esquerra_ERC', 'PSOE',
             'JuntsXCat', 'PPopular', 'vox_es', 'Pdemocratacat',
             'CatEnComu_Podem', 'PPCatalunya', 'socialistes_cat')
people <- people[!people %in% parties]
tweets$party <- tweets$person %in% parties

# Make month
make_month <- function(x){
  as.Date(paste0(format(as.Date(x), '%Y-%m'), '-01'))
}


# Nationalism?
search_cat <- function(x){
  grepl('catal', tolower(x))
}
search_esp <- function(x){
  grepl('españ', tolower(x))
}
search_x <- function(x){
  grepl('diálog|diàleg|dialog', tolower(x))
}
search_y <- function(x){
  grepl('zzzzzz', tolower(x))
}
x <- tweets %>%
  mutate(keep_cat = search_cat(tweet),
         keep_esp = search_esp(tweet),
         keep_x = search_x(tweet),
         keep_y = search_y(tweet)) %>%
  group_by(date = make_month(date), party,person) %>%
  summarise(numerator_cat = length(which(keep_cat)),
            numerator_esp = length(which(keep_esp)),
            numerator_x = length(which(keep_x)),
            numerator_y = length(which(keep_y)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p_cat = numerator_cat / denominator * 100,
         p_esp = numerator_esp / denominator * 100,
         p_x = numerator_x / denominator * 100,
         p_y = numerator_y / denominator * 100) %>%
  # mutate(p_max = ifelse(p_cat > p_esp, p_cat, p_esp)) %>%
  # arrange(desc(p_x)) %>%
  # arrange(party)
  arrange(date)

# Dialogue total
y <- x %>%
  filter(date >= '2017-09-01') %>%
  filter(party |person %in%
           c('InesArrimadas',
             'Albert_Rivera',
             'Albiol_XG',
             'carrizosacarlos',
             'pablocasado_',
             'miqueliceta',
             'PPopular',
             'Santi_ABASCAL',
             'GirautaOficial',
             'Ciudadanos_Cs',
             'KRLS',
             'gabrielrufian',
             'jordi_canyas',
             'JordiGraupera',
             'ramoncotarelo',
             'sanchezcastejon',
             'yeyaboya',
             'Societatcc',
             'Nataliadipp',
             'ahorapodemos',
             'Pdemocratacat',
             'QuimTorraiPla',
             # 'perearagones',
             'JuntsXCat',
             'cupnacional',
             # 'elsa_artadi',
             # 'epaluzie',
             # 'AlbanoDante76',
             'JoanTarda'
             # 'Nataliadipp'
             )) %>%
  group_by(person) %>%
  summarise(numerator = sum(numerator_x),
            denominator = sum(denominator))%>%
  ungroup %>%
  mutate(p = numerator / denominator * 100) %>%
  arrange(desc(p))
y$person <- factor(y$person, levels = y$person)
View(y)

ggplot(data = y,# %>% filter(person %in% parties),
       aes(x = person,
           y = p)) +
  geom_bar(stat = 'identity') +
  theme_databrew() +
  theme(axis.text.x = element_text(angle = 90))

x_long <- tidyr::gather(x %>% dplyr::select(date, party, person, p_x, p_y), key, value, p_x:p_y) %>% 
  filter(date >= '2017-11-01') %>%
  mutate(key = ifelse(key == 'p_x', 'x',
                      ifelse(key == 'p_y', 'y', NA))) 
date_breaks1 <- seq(as.Date('2010-01-01'),
                   as.Date('2018-01-01'),
                   by = 'year')
date_breaks2 <- seq(as.Date('2010-07-01'),
                   as.Date('2018-07-01'),
                   by = 'year')
date_breaks <- sort(unique(c(date_breaks1, date_breaks2)))[sort(unique(c(date_breaks1, date_breaks2))) >= '2016-07-01']

ggplot(data = x_long %>%
         filter(party) %>%
         mutate(person = paste0('@', person)),
       aes(x = date,
           y = value,
           color = key,
           group = key)) +
  geom_line(stat = 'smooth', method = 'auto', size = 1.3, alpha = 0.9) +
  geom_point(alpha = 0.8,
             size = 0.4)+
  geom_line(alpha = 0.3) +
  facet_wrap(~person) +
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


month_breaks <- seq(as.Date('2017-01-01'),
                    Sys.Date(),
                    by = 'month')


ggplot(data = x_long %>% mutate(person = paste0('@', person)) %>%
         filter(key == 'x'),
       aes(x = date,
           y = value)) +
  # geom_line(stat = 'smooth', method = 'auto', size = 1.3, alpha = 0.9) +
  # geom_point(alpha = 0.8,
  #            size = 1.8)+
  geom_line(alpha = 0.7, 
            # size = 1.3, 
            color = 'darkorange') +
  geom_area(alpha = 0.4, fill = 'darkorange') +
  # geom_bar(stat = 'identity',
  #          alpha = 0.6) +
  facet_wrap(~person) +
  theme_databrew() +
  # scale_color_manual(name = '',
  #                    values = make_colors(n = length(unique(x_long$person)))) +
  # scale_x_date(#labels = scales::date_format("%b\n%Y"),
  #   breaks = date_breaks,
  #   labels = c('Jul\n2016', 'Gen\n2017', 'Jul\n2017', 'Gen\n2018', 'Jul\n2018')) +
  scale_x_date(labels = scales::date_format('%b\n%Y'),
               breaks = month_breaks) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 10,
                                   color = 'darkgrey'),
        plot.title = element_text(size = 18,
                                  face = 'bold'),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        # axis.text.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 15),
        plot.caption = element_text(size = 11, hjust = 0)) +
  labs(x = 'Mes',
       y = 'Porcentaje de sus tuits',
       title = 'Referencias a "lazos", "neutralidad" y "espacio público"',
       caption = 'Incluyendo variaciones en catalàn') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = date_breaks1,
             alpha = 0.3)  #+
  # ylim(0, 30)

x_agg <- x_long %>%
  group_by(date, key) %>%
  summarise(value = mean(value, na.rm = TRUE))


ggplot(data = x_agg,
       aes(x = date,
           y = value,
           color = key,
           group = key)) +
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

# for(i in 1:length(people)){
#   person <- people[i]
#   message('Working on ', person)
#   log <- capture.output({system(paste0('python3 ../../twint/Twint.py -u ', person,
#          ' -o ../data/',
#          person,
#          '_tweets.csv --csv'))})
#   # Sys.sleep(30)
# }



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
  filter(person %in% c('KRLS', 
                       # 'elsa_artadi', 
                       # 'Pdemocratacat',
                       'toni_comin', #'cupnacional', 
                       # 'Esquerra_ERC',
                       # 'JuntsXCat',
                       'lluis_llach', #'miriamnoguerasM',
                       'Nataliadipp',
                       # 'yeyaboya',
                       # 'RaholaOficial',
                       'perearagones', 'QuimTorraiPla')) %>%
  # filter(person %in% c('Albert_Rivera',
  #                      # 'anarosaq'
  #                      
  #                      'InesArrimadas',
  #                      'miqueliceta',
  #                      # 'pablocasado_',
  #                      'Santi_ABASCAL',
  #                      # 'Eva_Granados',
  #                      'carrizosacarlos'
  #                      # 'Societatcc',
  #                      # 'vox_es'
  #                      # 'Albiol_XG',
  #                      # 'GirautaOficial',
  #                      # 'GirautaOficial'#,
  #                      # 'jordi_canyas'#,
  #                      # 'anarosaq',
  #                      # 'sanchezcastejon'
  # )
  #        ) %>%
  filter(date >= '2017-01-01') #%>%
  # mutate(person = ifelse(grepl('ivera', person), 'Albert Rivera',
                         # ifelse(grepl('iceta', person), 'Miquel Iceta', NA)))

# dict <- data.frame(person = people,
#                    new_person = 
#                      c('Albert Rivera',
#                        'Ines Arrimadas', 
#                        'Miquel Iceta', 
#                        'Pablo Casado',
#                        'Santi Abascal',
#                        'Societat Civil Catalan',
#                        'VOX',
#                        'Xavier Garcia Albiol',
#                        'Juan Carlos Girauta',
#                        'Jordi Cañas',
#                        'Ana Rosa Quintana',
#                        'Pedro Sánchez',
#                        'Eva Granados',
#                        'Carlos Carrizosa'))
# x <- x %>% left_join(dict) %>%
#   dplyr::select(-person) %>%
#   dplyr::rename(person = new_person) #%>%
#   # filter(!person %in% c('VOX', 'Santi Abascal'))
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

# search for term 
search <- function(x){
  grepl('españ|catal|', tolower(x))
}

x <- tweets %>%
  mutate(keep = search(tweet)) %>%
  filter(keep) %>%
  filter(person == 'sanchezcastejon')
