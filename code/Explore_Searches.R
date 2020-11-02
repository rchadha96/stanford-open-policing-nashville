# Exploring Searches, Jun 2020 -------------------------------

library(lubridate)
library(rjags)
library(R2jags)


## Read in the data
tn_nv <- read_csv("tn_nashville_2020_04_01.csv")


## Pull out data relevant for searches 
tn_search_raw <- tn_nv %>% 
  select(subject_race,search_conducted, contraband_found,date) %>% 
  mutate(year = lubridate::year(date)) 

## Get total searches accross years
total_searches <- tn_search_raw %>% 
  select(year, search_conducted) %>% 
  drop_na() %>% 
  group_by(year) %>% 
  summarise(total_searched = sum(search_conducted))

## Breakdown by race and look at probabilities per year (# searched per race/total)
search_race_long <- tn_search_raw  %>% 
  select(year, search_conducted, subject_race) %>% 
  drop_na() %>% 
  group_by(year,subject_race) %>% 
  summarise(race_searched = sum(search_conducted),
            total_searced = sum(race_searched)) %>% 
  ungroup() %>% 
  mutate(total_searched = rep(total_searches$total_searched,each = 6)) %>% 
  mutate(prob_search = race_searched/total_searched)

## plot probability by race
ggplot(search_race_long , aes(x = year, y = prob_search, colour = subject_race)) +
  geom_point() +
  geom_line()


## Wide format for modelling
search_race_wide <- search_race_long %>% 
  select(year,subject_race,race_searched) %>% 
  pivot_wider(values_from = race_searched, 
              names_from = subject_race) %>% 
  mutate(total_searches = rowSums(.[2:7]))

# prob contraband when searched (within race groups) ------------------------------------

tn_contraband <- tn_search_raw %>% drop_na() %>% 
  group_by(subject_race,year) %>% 
  summarise(n = n(),
            prob_contraband = sum(contraband_found)/n())


ggplot(tn_contraband, aes(x=year, y=prob_contraband, colour = subject_race)) +
  geom_point() +
  geom_line()
