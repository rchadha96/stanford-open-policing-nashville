# Exploring Models, Raghav Chadha 15 July 2020 -------------------------------

library(lubridate)
library(tidyverse)
library(ggplot2)
install.packages("gam")
library(gam)
library(mgcv)




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

## Remove year 2019 due to lack of data
tn_contraband_new <- tn_contraband %>%
  filter(year!=2019)

## plot probability of contraband_found by race
ggplot(tn_contraband_new , aes(x = year, y = pi_hat, color = subject_race)) +
  geom_point() +
  geom_line()

hist(tn_contraband_new$pi_hat, prob=TRUE, col="grey") 
lines(density(tn_contraband_new$pi_hat), col="blue", lwd=2) ## ad
lines(density(tn_contraband_new$pi_hat, adjust=2), lty="dotted", col="darkgreen", lwd=2) 


tn_contraband_new$subject_race <- as.factor(tn_contraband_new$subject_race)

#Logistic---------------------------------------------------------------------------------

glm_data <- tn_contraband_new %>%
  select(contraband_found, contraband_notfound) %>%
  as.matrix()

fit_lr <- glm(glm_data ~ year + subject_race, data = tn_contraband_new, family = binomial)
summary(fit_lr)
exp(coef(fit_lr))

#Interpretation---------------------------------------------------------------------------
#It can be seen that only year, intercept (subject_raceasian/pacific islander, taken as baseline), subject_raceblack, subject_racehispanic, 
#subject_raceother, subject_racewhite out of all the predictors are significantly associated to the outcome. 
#The intercept = -1.702e+02, which is interpreted as log odds of a driver with subject_race as asian having contraband_found when searched.
#The coefficient for year indicates that one unit increase in the year will increase the odds of contraband_found by exp(8.375e-02) 1.08 times ie 8%.
#The coefficient for subject_raceblack indicates increase the odds of contraband_found by exp(2.487e-01)= 28% increase in subject_raceblack compared to subject_raceasian.
# Similarly for others...

anova(fit_lr, test="Chisq")
# It appears all terms are making a significant contribution to the model.

#teststat <- sum(residuals(fit_lr, type = "deviance")^2)
#teststat
#teststat > qchisq(0.95,47)

#Prediction----------------------------------------------------------
tn_contraband_new$pred_lr <- predict(fit_lr, type = "response")

ggplot(tn_contraband_new, aes(x=year, y=pred_lr, colour= subject_race)) + 
  geom_smooth(method='lm', formula= y~x) +
  geom_point()

ggplot(data = tn_contraband_new, aes(x = year , y = pi_hat, color = subject_race)) +
  geom_point() +
  geom_line(aes(x = year, y = pred_lr))

## Future predictions-----------------------------------------------

MyDat <- data.frame(year=rep(c(2019,2020,2021,2022,2023,2024), each=6),
                    subject_race=rep(c("asian/pacific islander","black",
                                       "hispanic","other","unknown","white"), 
                                     each=1,times=6))

MyDat$pred_lr<-predict(fit_lr, newdata = MyDat, type="response")

ggplot(data = MyDat, aes(x = year , y = pred_lr, color = subject_race)) +
  geom_point() +
  geom_line()

#GAM------------------------------------

fit_g <- gam(glm_data ~ s(year) + subject_race, data = tn_contraband_new, family = binomial())
summary(fit_g)

par(mfrow = c(1,2))
plot(fit_g, se = T, col = "green")
par(mfrow = c(1,1))

anova(fit_g,test = "Chisq")

#Prediction----------------------------------------------------------

tn_contraband_new$pred_g <- predict.Gam(fit_g, type = "response", se.fit = "TRUE")


ggplot(tn_contraband_new, aes(x=year, y=pred_g$fit, colour= subject_race)) + 
  geom_smooth() + 
  geom_point()

ggplot(data = tn_contraband_new, aes(x = year , y = pi_hat, color = subject_race)) +
  geom_point() +
  geom_line(aes(x = year, y = pred_g$fit)) 

## Future years------------------------------------------------------------

MyDat$pred_gam<-predict.Gam(fit_g, newdata = MyDat, type="response")

ggplot(data = MyDat, aes(x = year , y = pred_gam, color = subject_race)) +
  geom_point() +
  geom_line()


## Comparing models--------------------------------------------------------
cm <- data.frame(Model = c("Logistic Regression", "GAM"),
                 AIC = c(fit_lr$aic, fit_g$aic))
cm
