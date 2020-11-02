# Exploring Searches, Raghav Chadha 18 Jun 2020 -------------------------------

library(lubridate)
library(tidyverse)


## Read in the data
tn_nv <- read_csv("tn_nashville_2020_04_01.csv")


## Pull out data relevant for searches 
tn_search_raw <- tn_nv %>% 
  select(subject_race,search_conducted, contraband_found,date) %>% 
  mutate(year = lubridate::year(date)) 

# prob contraband when searched (within race groups) ------------------------------------

tn_contraband <- tn_search_raw %>% 
  select(subject_race,year,contraband_found)%>%
  drop_na() %>% 
  group_by(subject_race,year) %>% 
  summarise(total_searches = n(),
            contraband_found = sum(contraband_found),
            contraband_notfound = total_searches - contraband_found,
            pi_hat = sum(contraband_found)/n()) %>%
  ungroup()

#Logistic---------------------------------------------------------------------------------

glm_data <- tn_contraband %>%
  select(contraband_found, contraband_notfound) %>%
  as.matrix()


fit_lr <- glm(glm_data ~ year + subject_race + total_searches, data = tn_contraband, family = binomial)
summary(fit_lr)
exp(coef(fit_lr))

#Interpretation---------------------------------------------------------------------------
#It can be seen that only year, intercept (subject_raceasian/pacific islander, taken as baseline), subject_raceblack, subject_racehispanic, 
#subject_raceother, subject_racewhite out of all the predictors are significantly associated to the outcome. 
#The intercept = -1.702e+02, which is interpreted as log odds of a driver with subject_race as asian having contraband_found when searched.
#The coefficient for year indicates that one unit increase in the year will increase the odds of contraband_found by exp(8.375e-02) 1.08 times ie 8%.
#The coefficient for subject_raceblack indicates increase the odds of contraband_found by exp(2.487e-01)= 28% increase in subject_raceblack compared to subject_raceasian.
# Similarly for others...


#Prediction----------------------------------------------------------
tn_contraband$pred <- predict(fit_lr, type = "response")

ggplot(data = tn_contraband, aes(x = year , y = pi_hat, color = subject_race)) +
  geom_point() +
  geom_line(aes(x = year, y = pred))

#There are a few outliers. The prob of contraband being found increases for each race as years increase!

df <- data.frame(year = rep(c(2020,2021,2022,2023,2024,2025),each=6),
                 subject_race = rep(c("asian/pacific islander","black", "hispanic","other","unknown","white"),each=1,times=6),
                 total_searches = sample(50:150,36,replace=TRUE)
                 )

df$pred<-predict(fit_lr, newdata = df, type = "response")
ggplot(data = df, aes(x = year , y = pred, color = subject_race)) +
  geom_point() +
  geom_line()
