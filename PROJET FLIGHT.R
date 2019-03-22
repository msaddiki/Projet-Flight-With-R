
# Chapitre 13 -- Relationnal Data

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
  filter(n > 1) # Affiche 0 ligne (id : Unique EPA identifier)


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




# # Chapitre 3 

# Prérequis :
# --> Installation de tydiverse
install.packages("tidyverse") 
library(tidyverse) 

install.packages(c("nycflights13", "gapminder", "Lahman"))

#DATA VISUALISATION
library(ggplot2) # Install le package ggplot2 fourni dans tidyverse.
ggplot2::mpg # 

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) # Crée un graphe via la fonction geom_point.


# 3.2.4 EXERCICE
# 1. Run ggplot(data = mpg). What do you see?
ggplot(data = mpg) # It shows an empty graph.

#2. How many rows are in mpg? How many columns?
nrow(mpg) # 234 rows (= observations)
ncol(mpg) # 11 columns (=variables)
dim(mpg) # affiche 234 (lignes) et 11 (colonnes)

# 3. What does the drv variable describe?
?mpg # drv indique la motrocité f = front-wheel drive, r = rear wheel drive, 4 = 4wd


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))

# color aestetic 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# alpha aestetic (transparence)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# shape aestetic (forme)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# aestetic (vert)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "green")

# aestetic (triangle)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 24)



# 3.3.1 EXERCICE
# 1. What’s gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue")) 
# Réponse : la couleur est un argument de aes et pas de geom_point.
# La bonne syntaxe est la suivante :
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue") 

# 2. Which variables in mpg are categorical? Which variables are continuous?
? mpg
# Réponse : categorical : manufacturer, model, year, trans, drv, fl, class
#           continuous :  displ, year, cyl, cty, hwy (type chr)
# How can you see this information when you run mpg?
# categorical = chr - continuous = int, dbl
mpg

# 3. Map a continuous variable to color, size, and shape. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = year))
# color s'affiche OK :

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = year))
#size s'affiche OK :

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))
#shape génère "Error: A continuous variable can not be mapped to shape" :

# How do these aesthetics behave differently for categorical vs. continuous variables?  
# Réponse : avec les variables continuous, les couleurs en légende s'affichent sous forme de spectre 
# alors qu'avec les variables categorical, les couleurs en légende s'affichent nominativement. 

# 4. What happens if you map the same variable to multiple aesthetics?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty, color = cty, size = cty))
# shape + color + size génère "Error: A continuous variable can not be mapped to shape".

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty, size = cty))
# color + size s'affiche OK (deux légendes s'affichent).

# 5. What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point).
?geom_point
# aestetic "troke" définit la largeur de la bordure et la couleur interne/externe des points du graphe.
# Test 1 :
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, stroke = 3 ))
# Test 2 :
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, stroke = 9 ))
 
# Test 3 avec aestetic "shape" :
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), stroke = 2, colour = "blue", fill = "red", shape = 27)

# 6. What happens if you map an aesthetic to something other than a variable name, 
# like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5 ))
# Réponse ; les points pour lesquels displ < 5 ont la valeur TRUE et les autre FALSE.
# FIN EXERCICE 3.3.1
#####



# CHAPITRE 5 DATA TRANSFORMATION 
# Prérequis :
nycflights13::flights # N'affiche que quelques lignes et colonnes de la df flights.
?flights

# Chargement des donneés :
library(nycflights13) # Chargement de la DB nycflights13.
# Après le chargement de la DB, plus besoin de préciser la DB nycflights13 pour
# afficher la df flights :
flights # Affiche quelques lignes et colonnnes de la df flights.

View(flights) # Affiche toutes les colonnes et lignes de la df flights (= tibble). 

# dplyr verbe 1 : filter()
filter(flights, month == 1, day == 1) # Affiche les vols du 1er Janvier
View(filter(flights, month == 1, day == 1)) # Même affichage en tibble.
jan1 <- filter(flights, month == 1, day == 1) # Stocke le résultat dans le dt jan1.
(dec25 <- filter(flights, month == 12, day == 25)) # Stocke ET affiche le resultat.

sqrt(2) ^ 2 == 2 # FALSE car R calcule d'abord la racine carrée de 2.
1 / 49 * 49 == 1 # FALSE car R calcule d'abord 1/49.
near(sqrt(2) ^ 2, 2) # TRUE
near(1 / 49 * 49, 1) # TRUE 

filter(flights, month == 11 | month == 12) # Affiche les vols de nov et dec. 
filter(flights, month == 11 | 12) # Affiche les vols de jan car résultat 11 | 12 = TRUE.

nov_dec <- filter(flights, month %in% c(11, 12)) # Stoke les vols de nov et dec.
nov_dec

# Toutes les opérations suivantes retournent NA (missing value)
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

# Explications :
x <- NA # x = âge de Robert
y <- NA # y = âge de Marcel
x == y # Retourne NA (on ne sait pas si Robert et Paul ont le même âge).
# is.na() : fonction qui indique si une variables est NA.
is.na(y)  # TRUE
is.na(NA) # TRUE
is.na(1)  # FALSE

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1) # Retourne la valeur 3
filter(df, x>= 1) # Retourne les valeurs 1 et 3
filter(df, is.na(x) | x > 1) # Retourne les valeurs NA et 3

####
names(flights) # Affiche le nom des colonnes de flights
length(flights) # Affiche le nombre de colonnes de flighs
ncol(flights)
nrow(flights)
attributes(flights) # Affiche les lignes, colonnes et data type.
####


#####
# 5.2.4 EXERCICE
# Find flights that :
# 1. had an arrival delay of two or more hours?
df1 <- dplyr::filter(flights, arr_delay  <= 120, dep_delay <= 120)

View(df1) # Affichage du tibble plus pratique.
View(select(df1, dep_delay, arr_delay)) # Affiche uniquement les colonnes sélectionnée.
summary(df1$arr_delay)
summarise (df1)
table(df1$arr_delay)

#2. flew to Houston (IAH or HOU)?
df <- dplyr::filter(flights, dest == 'IAH' | dest == 'HOU')
df
View(df)

View(df %>% select(dest))
table(df$dest)
df$dest
table(flights$dest) # Nombre de destinations dans flights
length(table(df$dest))

ndest <- table(flights$dest)
View(ndest)

install.packages("questionr") # Package pour créér des tableaux plats.
library(questionr)



#3. were operated by United, American, or Delta?
airlines # Affiche les compagnies aériennes (carriers).
df <- dplyr::filter(flights, carrier == "AA" | carrier == "DL" | carrier == "UA")
View(df)

#4. departed in summer (July, August, and September)?
df <- filter(flights, month == 7 | month == 8 | month == 9)
View(df)
# ou bien :
df <- dplyr::filter(flights, month > 6 & month < 10)
View(df)

df <- filter(flights, month %in% c(7:8))
df

View(flights %>% select(month) %>% dplyr::filter(month %in% c(7:9))) 
View(flights %>% select(dep_delay) %>% dplyr::filter(dep_delay < 0)) 

glimpse(flights)
summary(flights$month)

#5. arrived more than two hours late, but didn’t leave late?
df <- filter(flights, arr_time > 120 & dep_delay <=  0) 
# Note : l'avion peut avoir un dep_delay négatif dans le cas où il décolle en avance.
View(df)

#6. were delayed by at least an hour, but made up over 30 minutes in flight?
df <- dplyr::filter(flights, dep_delay >=  60 & (dep_delay - arr_delay) >= 30) 
df
View(df)

#7. departed between midnight and 6am (inclusive)?
df <- dplyr::filter(flights, dep_time >=0000, dep_time <= 0600)
View(df)

df  <- dplyr::filter (flights, dep_time <=600 | dep_time == 2400)


# ou bien :
df <- dplyr::filter(flights, between(dep_time, 0060, 0600)) 
View(df)

View(flights %>% select(dep_time) %>% dplyr::filter(dep_time %in% c(0000:))

glimpse(flights$dep_time)
 select(flights, dep_time == 0)

#8. How many flights have a missing dep_time?
df <- filter(flights, is.na(dep_time)) 
View(df)

sum(is.na(df)) # Compte 
sum(is.na(df$dep_time)) # Compte le nb de valeurs d'une colonne.
summary(df)
filter(flights, is.na(dep_time)) %>% select(dep_time) %>% nrow()

# What other variables are missing? 
df <- is.na(df) # Retourne TRUE si une valeur est NA sinon FALSE
View(df) # Un tri sur chaque colonne permet de voir si on a des TRUE (donc des NA). 
         # Les variables suivantes ont des NA : dep_time, dep_delay, arr_time, 
         # arr_delay, tailnum et air_time, 

# What might these rows represent?
# Réponse : Ces données sont manquantes car sûrement les 
# avions en question n'ont pas décollé (vols annulés).
View(df <- dplyr::filter(flights, is.na(dep_time))) # Ces vols correspondent aux vols n'ayant pas décollé.

for (i in (1:10)) {print(i)}

for (num_col in 1:ncol(flights)) {
    if(sum(is.na(flights[,num_col])) > 0) 
    {
    print(names(flights)[num_col])
    print(sum(is.na(flights[,num_col])))
}
}

apply (is.na(flights),2,sum)

sum_is_na<-function(x) {
  sum(is.na(x))
}
apply(flights, 2,sum_is_na)


NA ^ 0 # Retourne 1 car un nombre à la puissance zéro est toujours égal à 1.
NA | TRUE # Retourne TRUE car le OU logique est TRUE si l'une des valeurs au moins est TRUE.
FALSE & NA # Retourne FALSE car le ET logique retourne TRUE que si les deux valeurs sont TRUE.
NA * 0 # Retourne NA alors qu'un nombre multiplié par zéro est toujours égal à zéro.
       # Une opération arithmétique avec un NA retournera NA. 
       # Les opérations logiques (AND, OR) ignorent NA.
# FIN EXERCICE 3.3.1
#####

# dplyr verbe 2 : arrange ()
arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x) # NA vient en fin de liste.
arrange(df, desc(x)) # NA vient en fin de liste.

sum(is.na(flights)) # Affiche 46595 (TRUE)
which(is.na(flights$dep_time))
flights[which(is.na(flights$dep_time)), "dep_time"] # Ancienne commande

#####
# 5.3.1 EXERCICE

#1- How could you use arrange() to sort all missing values to the start? 
# (Hint: use is.na()).
arrange(flights, desc(is.na(dep_time)))

View(arrange(flights, desc(is.na(dep_time)), desc(dep_time)))

arrange(flights, !is.na(dep_time))

#2- Sort flights to find the most delayed flights.
arrange(flights, desc(dep_delay))
# ou bien :
arrange(flights, desc(arr_delay))

df-delayed <- arrange(arrange(flights, desc(dep_delay)), arrange(flights, desc(dep_delay)))
flights %>% select(flight,dep_delay,arr_delay) %>% arrange(desc(dep_delay+arr_delay))

flights %>% mutate(tot_delay=dep_delay+arr_delay) %>% select(flight,dep_delay,arr_delay,tot_delay) %>% arrange(desc(tot_delay))

> df.most <- flights %>% mutate(most_delayed = dep_delay + arr_delay) %>% arrange(most_delayed)
> View(df.most)

# --> Find the flights that left earliest.
flights %>% arrange(dep_delay) %>% select(flight,dep_delay)
df_earliest <- flights %>% filter(dep_delay < 0) %>% arrange(dep_delay) %>% select (flight, dep_delay)
df_earliest



#3- Sort flights to find the fastest flights.
arrange(flights, desc(distance / air_time))

#4- Which flights travelled the longest? 
arrange(flights, desc(arr_time - dep_time))
# ou bien :
arrange(flights, desc(distance))

# --> Which travelled the shortest?
arrange(flights, (arr_time - dep_time))
# ou bien :
arrange(flights, (distance))

# FIN EXERCICE 5.3.1
#####


# dplyr verbe 3 : select ()
?select

select(flights, year, month, day) # Affiche les colonnes spécifiées.
select(flights, year:day) # Affiche les colonnes de entre year et day incluses.
select(flights, -(year:day)) # Affiche toutes les colonnes sauf celles entre year et day inclusives.

# Utiliser select avec les fonctions :
# starts_with("abc")
# ends_with("xyz")
# contains("ijk")
# matches("(.)\\1") --> variables qui correspondent à une expression régulière.
# num_range("x", 1:3) --> Correspondra à x1, x2 et x3.

rename(flights, tail_num = tailnum) # Renomme tailnum en tail_num.

select(flights, time_hour, air_time, everything()) # Déplace les variables spécifiées en début de df.

#####
# 5.4.1 EXERCICE
#1- Brainstorm as many ways as possible to select dep_time, dep_delay, 
#   arr_time, and arr_delay from flights.

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, contains("tail"), contains("rrier"))
select(flights, starts_with("time"), starts_with("arr"))
select(flights, ends_with("hour"), ends_with("ance"))

# 2- What happens if you include the name of a variable multiple 
# times in a select() call?
select(flights, dep_time, dep_time, dep_time)
# Réponse : la colonne répétée ne s'affiche qu'une fois.

# 3- What does the one_of() function do? Why might it be helpful in 
# conjunction with this vector?
# vars <- c("year", "month", "day", "dep_delay", "arr_delay")
?one_of
# Réponse : one_of(): fera la correspondance avec les variables dans 'character vector'.
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

# 4- Does the result of running the following code surprise you? 
select(flights, contains("TIME"))

# --> How do the select helpers deal with case by default?
# Réponse : insensible à la casse car par défaut 'ignore.case = TRUE'.

# --> How can you change that default?
select(flights, contains("TIME", ignore.case = FALSE))  

# FIN EXERCICE 5.4.1
#####

# dplyr verbe 4 : mutate ()
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

# Pour n'afficher que les variables spécifiées utiliser transmute().
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)


transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

# Offsets :
(x <- 1:10)
lag(x) # Affiche  NA  1  2  3  4  5  6  7  8  9
lead(x) # Affiche  2  3  4  5  6  7  8  9  10 NA

# Cumulative and rolling aggregates: 
x # Affiche 1  2  3  4  5  6  7  8  9 10
cumsum(x) # Affiche  1  3  6 10 15 21 28 36 45 55
cummean(x) # Affiche 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5

# Ranking :
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y) # Affiche  1  2  2 NA  4  5
min_rank(desc(y)) # Affiche 5  3  3 NA  2  1
row_number(y) # Affiche 1  2  3 NA  4  5
dense_rank(y) # Affiche 1  2  2 NA  3  4
percent_rank(y) # Affiche 0.00 0.25 0.25   NA 0.75 1.00
cume_dist(y) # Affiche 0.2 0.6 0.6  NA 0.8 1.0

#####
# 5.5.2 EXERCICE

# 1- Currently dep_time and sched_dep_time are convenient to look at, but 
# hard to compute with because they’re not really continuous numbers. 
# Convert them to a more convenient representation of number of minutes 
# since midnight.
transmute(flights,
          sched_dep_time = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100,
          dep_time = (dep_time %/% 100) * 60 + dep_time %% 100)
# 2- Compare air_time with (arr_time - dep_time). 
# What do you expect to see? 
# Réponse : ils doivent être égaux.

# What do you see? 
flights_bis <- select(flights, air_time, arr_time, dep_time)
# Réponse : ils ne sont pas égaux (car arr_time et dep_time sont en 
#           HHMM ou HMM alors que air_time est en minutes.  

# What do you need to do to fix it?
mutate(flights_bis, air_time_new = arr_time - dep_time)
# Réponse : on crée une nouvelle variable = arr_time - dep_time.

# 3- Compare dep_time, sched_dep_time, and dep_delay. How would you 
# expect those three numbers to be related?
# Réponse : dep_time doit être égal à (sched_dep_time + dep_delay).

# 4- Find the 10 most delayed flights using a ranking function. How do you 
# want to handle ties? Carefully read the documentation for min_rank().
retard <- mutate(flights, le_plus_de_retard = min_rank(desc(arr_delay)))
arrange(retard, le_plus_de_retard)
# min_rank() permet de lier les plus bas rangs.

# 5- What does 1:3 + 1:10 return? Why?
1:3 + 1:10 # Affiche 2  4  6  5  7  9  8 10 12 11
