

install.packages --- Load library
library(nycflights13)
library(tidyverse)
library(lubridate)


# 13.2.1 Exercises
# 1- Imagine you wanted to draw (approximately) the route each plane flies from its origin 
# to its destination. What variables would you need? What tables would you need to combine?

  route_flight <- flights %>% left_join (airports, by = c("origin" = "faa", "dest" = "faa"))
View(route)

# 2- I forgot to draw the relationship between weather and airports. What is the relationship and how should it appear in the diagram?
la relation se ferait entre weather$origin et airports$faa :
  
  # 3- Table weather only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with flights?
  une relation supplémentaire entre flights$dest et weather$origin et on aurait la météo à chaque aéroport US présent dans la table flight.

# 4- We know that some days of the year are “special”, and fewer people than usual fly on them. How might you represent that data as a data frame? What would be the primary keys of that table? How would it connect to the existing tables?
on ajouterait la table special_days qui aurait 'jour' et 'mois' pour variables par lesquelles elle serait liée à la table flights. 


# Vérifier si une clé est unique ou pas (doublons, duplicates) :
planes %>% 
  count(tailnum) %>% 
  filter(n > 1) # Affiche 0 ligne.

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter (n > 1) # Affiche 3 lignes.

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1) # Affiche 29,768 lignes.

airports %>% 
  count(faa) %>% filter(n > 1) # Affiche 0 ligne.


# 13.3.1 Exercises
# 1- Add a surrogate key to flights.
flights %>%
  mutate(id = row_number(year)) %>%
  select(id, everything())

2- Identify the keys in the following datasets
# Lahman::Batting
install.packages("Lahman")
library(Lahman)

Lahman::Batting %>% 
  count(playerID, yearID, stint) %>% 
  filter(n > 1) # Affiche 0 ligne.

# babynames::babynames
install.packages("babynames")
library(babynames)


babynames::babynames %>% 
  count(id) %>% 
  filter(n > 1)
??? Erreur MAIS pas de clé primaire.

# nasaweather::atmos
install.packages("nasaweather")
library(nasaweather)

nasaweather::atmos %>% 
  count(lat, long, year, month) %>% 
  filter(n > 1) # Affiche 0 ligne.

# fueleconomy::vehicles
install.packages("fueleconomy")
library(fueleconomy)

fueleconomy::vehicles %>% 
  count(id) %>% 
  filter(n > 1)	# Affiche 0 ligne (id : Unique EPA identifier)


ggplot2::diamonds
install.packages("ggplot2")
library(ggplot2)
Pas de clé primaire


  
  ### Mutating joins
  # Création d'un subset de flights
  flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

# Mutating join entre flights et airlines sur la variable "carrier".
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

# Même résutlat que si on avait fait :
flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])


### 13.4.6 Exercises
# 1- Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:

# 1a) Pour obtenir les retards moyens par destination :
flights %>% 
  mutate(retard_total = dep_delay + arr_delay) %>%
  group_by(dest) %>%
  summarize(retard_moyen = mean(tot_delay, na.rm = TRUE))

# 2b) Pour obtenir la distribution géographique des retards moyens :
install.packages("maps") # Pour dessiner la corte US.
library(maps)

flights %>%
  mutate(tot_delay = arr_delay + dep_delay) %>%
  group_by(dest) %>%
  summarize(avg_delay = mean(tot_delay, na.rm = TRUE)) %>%
  semi_join(select(airports, faa, lon, lat), c("dest" = "faa")) %>%
  ggplot(aes(x = lon, y = lat, size = avg_arr_delay, color = avg_arr_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# 2- Add the location of the origin and destination (i.e. the lat and lon) to flights.
flights %>% 
  left_join(airports, by = c('dest' = 'faa')) %>%
  left_join(airports, by = c('origin' = 'faa'), suffix = c('.dest', '.origin')) %>%
  select(origin, dest, matches("lat|lon"))

