# Exploring Models, Raghav Chadha 15 July 2020 -------------------------------

library(lubridate)
library(tidyverse)
library(ggplot2)
library(gam)
library(mgcv)
library(R2jags)

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
  filter(year!=2019) %>%
  filter(subject_race!="unknown")

## plot probability of contraband_found by race
ggplot(tn_contraband_new , aes(x = year, y = pi_hat, color = subject_race)) +
  geom_point() +
  geom_line()

hist(tn_contraband_new$pi_hat, prob=TRUE, col="grey") 
lines(density(tn_contraband_new$pi_hat), col="blue", lwd=2) ## ad
lines(density(tn_contraband_new$pi_hat, adjust=2), lty="dotted", col="darkgreen", lwd=2) 


tn_contraband_new$subject_race <- as.factor(tn_contraband_new$subject_race)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
tn_contraband_new$race_index <- as.numeric(as.factor(tn_contraband_new$subject_race))


## Specify the JAGS model (add quadratic term)
binom_model = "
model{

   for( i in 1:n_obs ) {
 #Binomial likelihood 
  y.i[i] ~ dbinom(p.i[i], N.i[i])
   }

    for( i in 1:n_obs ) {
  nu.i[i] <- alpha[race_index[i]] + beta[race_index[i]]*t.i[i] #separate regression for each race
  p.i[i] <- exp(nu.i[i])/(1+exp(-nu.i[i]))

    }
  
  # hierarchical priors with race specific parameters centered on overall parameter with cross race variation
  for( j in 1:n_race ){
   alpha[j] ~ dnorm(mu_alpha,sigma_alpha^-2)
   beta[j] ~ dnorm(mu_beta,sigma_beta^-2)
  }
  
 

# priors for overall parameter with cross race variation
mu_alpha ~ dnorm(0,0.001)
mu_beta ~ dnorm(0,0.001)
sigma_alpha ~ dt(1,1,10^-2)T(0,)
sigma_beta ~ dt(1,1,10^-2)T(0,)

}
"

## Specify the JAGS data
jags.data <- list(y.i = tn_contraband_new$contraband_found,
                  t.i = scale(tn_contraband_new$year)[,1], # had to standardise time to get the model to work
                  N.i = tn_contraband_new$total_searches,
                  n_obs = nrow(tn_contraband_new),
                  race_index = tn_contraband_new$race_index,
                  n_race = max(tn_contraband_new$race_index))


## monitor parameters
parnames <- c("p.i","alpha", "beta","mu_alpha","mu_beta")

library(rjags)
library(R2jags)
mod <- jags(data = jags.data, parameters.to.save=parnames, 
            model.file = textConnection(binom_model),
            n.iter = 15000,
            n.burnin = 3000,
            n.thin = 6)

plot(mod)

## create output objects
mcmc.array <- mod$BUGSoutput$sims.array
dim(mcmc.array)

## summary output
mod$BUGSoutput$summary
 
#library(shinystan)
 #shiny.array <- as.shinystan(mod$BUGSoutput$sims.array)
 #launch_shinystan(shiny.array)

## get output
p_mean <- as_tibble(mod$BUGSoutput$mean$p.i)
p_mean$year <- tn_contraband_new$year
p_mean$subject_race = tn_contraband_new$subject_race
p_mean$lwr = apply(mod$BUGSoutput$sims.list$p.i,2,quantile,probs = 0.025) 
p_mean$upr = apply(mod$BUGSoutput$sims.list$p.i,2,quantile,probs = 0.975) 

ggplot(tn_contraband_new , aes(x = year, y = pi_hat)) +
  geom_point() +
  geom_line(data = p_mean, aes(x= year, y = value)) +
  geom_line(data = p_mean, aes(x= year, y = lwr, colour = "red"), linetype="dashed")+
  geom_line(data = p_mean, aes(x= year, y = upr, colour = "red"), linetype="dashed")+
  facet_wrap(~subject_race) +
  theme_bw() +
  theme(legend.position = "none")

## Future Predictions

n_race = max(tn_contraband_new$race_index)
## scaled observation years
t.i = scale(tn_contraband_new$year)
## define predictions years
pred_year <- c(2019,2020,2021,2022,2023,2024)
# get center and scale from orginal scaled data to scale your prediction years
scaled_year <- (pred_year - 2014)/2.611165

## set up to store results
pred_nu <- pred_pi <- matrix(NA, ncol = n_race, nrow = length(pred_year))

## for each race get predictions for the prediction years (using the scaled years)
for(j in 1: n_race)
{
  pred_nu[,j] <- mod$BUGSoutput$mean$alpha[j] + mod$BUGSoutput$mean$beta[j]*scaled_year
  pred_pi[,j] <- exp(pred_nu[,j])/(1+exp(-pred_nu[,j]))
}

## create results table
pred_pi <- as_tibble(pred_pi)
names(pred_pi) <- as.character(tn_contraband_new$subject_race %>% unique)
pred_pi$year <- pred_year

pred_pi

