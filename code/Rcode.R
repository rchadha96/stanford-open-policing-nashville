## Loading Required Libraries
library(lubridate)
library(tidyverse)
library(ggplot2)
library(plotly)
library(mgcv)
library(R2jags)
library(rjags)
library(RColorBrewer)
library(shinystan)
library(viridis)

## Read in the data
tn_nv <- read_csv("tn_nashville_2020_04_01.csv")

## Computing summary statistics broken down by the race of the driver (TABLE 2.1)--------------
summary_stats <- function(search_conducted, contraband_found) {
  n_stops = length(search_conducted)
  n_searches = sum(search_conducted, na.rm = T)
  n_hits = sum(contraband_found, na.rm = T)
  search_rate = n_searches / n_stops
  hit_rate = n_hits / n_searches
  return(data.frame(n_stops, n_searches, n_hits, search_rate, hit_rate))
}

## Pull out data relevant for searches 
tn_search_raw <- tn_nv %>% 
  select(subject_race,search_conducted, contraband_found,date) %>% 
  mutate(year = lubridate::year(date)) 	

basic_summary_statistics_by_race = tn_search_raw %>%
  group_by(subject_race) %>%
  do(summary_stats(.$search_conducted, .$contraband_found)) %>%
  drop_na()
basic_summary_statistics_by_race

## Comparing drivers stopped in Nashville with neighbouring cities-----------------------------------
## Nashville
tn_nv_new <- tn_nv %>%
  select(subject_race, subject_sex, outcome, date, time) %>%
  filter(year(date) > 2013 & year(date) < 2016)
tn_nv_new$city <- "Nashville"

## Madison
wi_m <- read_csv("wi_madison_2019_12_17.csv")
wi_m <- wi_m %>%
  select(subject_race, subject_sex, outcome, date, time) %>%
  filter(year(date) > 2013 & year(date) < 2016)
wi_m$city <- "Madison"

## Columbus
oh_c <- read_csv("oh_columbus_2019_12_17.csv")
oh_c <- oh_c %>%
  select(subject_race, subject_sex, outcome, date, time) %>%
  filter(year(date) > 2013 & year(date) < 2016)
oh_c$city <- "Columbus"

## Charotte
nc_c <- read_csv("nc_charlotte_2020_04_01.csv")
nc_c <- nc_c %>%
  select(subject_race, subject_sex, outcome, date, time) %>%
  filter(year(date) > 2013 & year(date) < 2016)
nc_c$city <- "Charlotte"

## Creating a dataframe for all the cities to plot
df <- rbind(oh_c, wi_m,tn_nv_new,nc_c)
df1 <- data.frame(df)

## Summarise the results
stops_byyearcity <- df1 %>%
  group_by(city, year = year(date)) %>%
  summarise(stops = n())

## Plottting the results (FIGURE 2.1)----------------------------------------------------------------------------
plot1 <- ggplot(data = stops_byyearcity, mapping = (aes(x=reorder(city,stops), y= stops, fill = city)))+
  geom_bar(stat="identity") +
  ggtitle("Drivers Stopped in Cities by Year") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  xlab("City") +
  ylab("Stopped Drivers") +
  theme(legend.position = "none") +
  facet_wrap(~year) +
  theme_bw() +
  theme(legend.position = "none")
ggplotly(plot1)

## Calculating stops made by MNPD each year
stops_made <- tn_nv %>%
  group_by(year = year(date), subject_race) %>%
  summarise(stops = n()) %>%
  filter(year != 2019) %>%
  drop_na()

## Plotting the results of stop_made (FIGURE 2.2)-----------------------------------------------------------
plot2 <- ggplot(data = stops_made, mapping = aes(x = year, y = stops, colour = subject_race)) +
  scale_color_brewer(palette = "Dark2") +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("Stopped Driver") +
  ggtitle("Stopped Drivers from 2010-2018 according to Race") +
  theme(legend.title = element_blank())

ggplotly(plot2)

## Calculating Arrests made by MNPD each year
arrests_byyear <- tn_nv %>%
  group_by(year = year(date), arrest_made, subject_race) %>%
  summarise(arrest = sum(arrest_made)) %>%
  filter(arrest_made == "TRUE") %>%
  filter(year != 2019) %>%
  drop_na()

## Plotting the results of arrests_byyear (FIGURE 2.3)---------------------------------------------------------
plot3 <- ggplot(data = arrests_byyear, mapping = aes(x = year, y = arrest, colour = subject_race)) +
  scale_color_brewer(palette = "Dark2") +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("Arrested Drivers") +
  ggtitle("Arrested Drivers from 2010-2018 according to Race") +
  theme(legend.title = element_blank())

ggplotly(plot3)

## Scatterplot which compares search rates and hit rates or minority and white drivers within the same precinct-----------------------------------------------------------------------------------------------------
basic_summary_statistics_by_race_and_precinct = tn_nv %>%
  filter(!is.na(precinct)) %>%
  group_by(subject_race, precinct) %>%
  do(summary_stats(.$search_conducted, .$contraband_found))

basic_summary_statistics_by_race_and_precinct

data_for_plot <- basic_summary_statistics_by_race_and_precinct %>%
  filter(subject_race == 'white') %>%
  right_join(basic_summary_statistics_by_race_and_precinct %>%
               filter(subject_race != 'white'), by='precinct')

## Plot search rates (FIGURE 2.4)--------------------------------------------------------------------------------
max_val = max(basic_summary_statistics_by_race_and_precinct$search_rate) * 1.05
search_plot <- ggplot(data_for_plot) +
  geom_point(aes(x = search_rate.x, y = search_rate.y, size = n_stops.y)) + # specify data we want to plot
  facet_grid(.~subject_race.y) + # make one subplot for each minority race group
  geom_abline(slope = 1, intercept = 0, linetype='dashed') + # add a diagonal line to indicate parity
  scale_x_continuous('White search rate', limits=c(0, max_val), labels = scales::percent, expand=c(0,0)) +
  scale_y_continuous('Minority search rate', limits=c(0, max_val), labels = scales::percent, expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="none") +
  scale_size_area(max_size=5)
search_plot

## Now let’s compute hit rates by race and district (FIGURE 2.5)-----------------------------------------
hit_rates <- tn_nv %>%
  filter(search_conducted) %>%
  group_by(subject_race, precinct) %>%
  summarize(hit_rate = mean(contraband_found, na.rm = T)) %>%
  drop_na()
hit_rates

## Reshape table to show hit rates of minorities vs white drivers
hit_rates <-
  hit_rates %>%
  filter(subject_race %in% c("black", "white", "hispanic")) %>%
  spread(subject_race, hit_rate, fill = 0) %>%
  rename(white_hit_rate = white) %>%
  gather(minority_race, minority_hit_rate, c(black, hispanic)) %>%
  arrange(precinct)
hit_rates

## Get corresponding number of searches (to size points). 
#For each district we want to know the number of white+black searches 
#and white+Hispanic searches---------------------------------------------------
search_counts <- tn_nv %>%
  filter(
    search_conducted,
    subject_race %in% c("black", "white", "hispanic")
  ) %>%
  count(precinct, subject_race) %>%
  spread(subject_race, n, fill = 0) %>%
  rename(num_white_searches = white) %>%
  gather(minority_race, num_minority_searches, c(black, hispanic)) %>%
  mutate(num_searches = num_minority_searches + num_white_searches) %>%
  select(precinct, minority_race, num_searches) %>%
  drop_na()
## We'll use this just to make our axes' limits nice and even
max_hit_rate <- hit_rates %>%
  select(ends_with("hit_rate")) %>%
  max()
hit_rates %>%
  ggplot(aes(
    x = white_hit_rate,
    y = minority_hit_rate
  )) +
  geom_point() +
  # This sets a diagonal reference line (line of equal hit rates)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # These next few lines just make the axes pretty and even
  scale_x_continuous("White hit rate",
                     limits = c(0, max_hit_rate + 0.01),
                     labels = scales::percent) +
  scale_y_continuous("Minority hit rate",
                     limits = c(0, max_hit_rate + 0.01),
                     labels = scales::percent
  ) +
  # This makes sure that 1% on the x-axis is the same as 1% on the y-axis
  coord_fixed() +
  # This allows us to compare black v. white and Hispanic v. white side by side, in panels
  facet_grid(. ~ minority_race)
hit_rates %>%
  left_join(
    search_counts,
    by = c("precinct", "minority_race")
  ) %>%
  ggplot(aes(
    x = white_hit_rate,
    y = minority_hit_rate
  )) +
  geom_point(aes(size = num_searches), pch = 21) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous("White hit rate",
                     limits = c(0, max_hit_rate + 0.01),
                     labels = scales::percent) +
  scale_y_continuous("Minority hit rate",
                     limits = c(0, max_hit_rate + 0.01),
                     labels = scales::percent) +
  coord_fixed() +
  facet_grid(. ~ minority_race) +
  theme_bw()

## Prob contraband when searched (within race groups)
tn_contraband <- tn_search_raw %>%
  select(subject_race,year,contraband_found)%>%
  drop_na() %>%
  group_by(subject_race,year) %>%
  summarise(total_searches = n(),
            contraband_found = sum(contraband_found),
            contraband_notfound = total_searches - contraband_found,
            pi_hat = sum(contraband_found)/n()) %>%
  ungroup()

## LOGISTSIC REGRESSION-------------------------------------------------------------------------------------
## Removing year 2019 due to lack of data and subject_race “unknown”
tn_contraband_new <- tn_contraband %>%
  filter(year!=2019) %>%
  filter(subject_race!="unknown")

## Removing 2018 to check accuracy of model on latest year using CI
tn_contraband_new_2018 <- tn_contraband_new %>%
  filter(year!=2018)

## Creating matrix of contraband found and not found to give as response
glm_data <- tn_contraband_new_2018 %>%
  select(contraband_found, contraband_notfound) %>%
  as.matrix()

## Fitting the Logistic Regression Model
fit_lr <- glm(glm_data ~ year + subject_race, data = tn_contraband_new_2018, family = binomial)
summary(fit_lr)
exp(coef(fit_lr))
anova(fit_lr, test="Chisq")

# It appears all terms are making a significant contribution to the model.
teststat <- sum(residuals(fit_lr, type = "deviance")^2)
teststat
teststat > qchisq(0.95,34)

## Predictions
pred_lr <- as.tibble(predict(fit_lr, type = "response",se.fit = T))
pred_lr$year = tn_contraband_new_2018$year
pred_lr$subject_race = tn_contraband_new_2018$subject_race
pred_lr <- pred_lr %>%
  mutate(upr = fit+(1.96*se.fit),
         lwr = fit-(1.96*se.fit))

## Plot of contraband_found with Overlaid Predictions from Logistic Regression for each Race & Year (FIGURE 4.1)
ggplot(tn_contraband_new_2018 , aes(x = year, y = pi_hat)) +
  geom_point() +
  geom_line(data = pred_lr, aes(x = year, y = fit))+
  geom_line(data = pred_lr, aes(x= year, y = lwr, colour = "red"), linetype="dashed")+
  geom_line(data = pred_lr, aes(x= year, y = upr, colour = "red"), linetype="dashed")+
  facet_wrap(~subject_race) +
  theme_bw() +
  theme(legend.position = "none")

## Diagnostic plots (FIGURE 4.2)
par(mar=c(1,1,1,1))
plot(fit_lr)
par(mfrow=c(1,1))

## GENERALIZED ADDITIVE MODELS (GAMS) ----------------------------------------------------------
fit_g <- mgcv::gam(glm_data ~ s(year, bs = "cr", k = 3) + subject_race, data = tn_contraband_new_2018, family = binomial(), method = "REML")
summary(fit_g)

## Predictions
pred_gam <- as.tibble(predict(fit_g, type = "response", se = TRUE))
pred_gam$year = tn_contraband_new_2018$year
pred_gam$subject_race = tn_contraband_new_2018$subject_race
pred_gam <- pred_gam %>%
  mutate(upr = fit+(1.96*se.fit),
         lwr = fit-(1.96*se.fit))

## Plot of contraband_found with Overlaid Predictions from Generalized Additive Model for each Race & Year (FIGURE 4.4)
ggplot(tn_contraband_new_2018 , aes(x = year, y = pi_hat)) +
  geom_point() +
  geom_line(data = pred_gam, aes(x = year, y = fit))+
  geom_line(data = pred_gam, aes(x= year, y = lwr, colour = "red"), linetype="dashed")+
  geom_line(data = pred_gam, aes(x= year, y = upr, colour = "red"), linetype="dashed")+
  facet_wrap(~subject_race) +
  theme_bw() +
  theme(legend.position = "none")

## Diagnostic Plots (FIGURE 4.5)
par(mfrow=c(2,2))
mgcv::gam.check(fit_g)

## BAYESIAN HEIRARICHAL MODEL----------------------------------------------------------------------------
tn_contraband_new_2018$subject_race <- as.factor(tn_contraband_new_2018$subject_race)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
tn_contraband_new_2018$race_index <- as.numeric(as.factor(tn_contraband_new_2018$subject_race))

## Specify the JAGS model 
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
## Hierarchical priors with race specific parameters centered on overall parameter with cross race variation
for( j in 1:n_race ){
alpha[j] ~ dnorm(mu_alpha,sigma_alpha^-2)
beta[j] ~ dnorm(mu_beta,sigma_beta^-2)
}
## Priors for overall parameter with cross race variation
mu_alpha ~ dnorm(0,0.001)
mu_beta ~ dnorm(0,0.001)sigma_alpha ~ dt(1,1,10^-2)T(0,)
sigma_beta ~ dt(1,1,10^-2)T(0,)
}
"
## Specify the JAGS data
jags.data <- list(y.i = tn_contraband_new_2018$contraband_found,
                  t.i = scale(tn_contraband_new_2018$year)[,1], #to standardise time to get the model to work
                  N.i = tn_contraband_new_2018$total_searches,
                  n_obs = nrow(tn_contraband_new_2018),
                  race_index = tn_contraband_new_2018$race_index,
                  n_race = max(tn_contraband_new_2018$race_index))

## monitor parameters
parnames <- c("p.i","alpha", "beta","mu_alpha","mu_beta")
mod <- jags(data = jags.data, parameters.to.save=parnames,
            model.file = textConnection(binom_model),
            n.iter = 15000,
            n.burnin = 3000,
            n.thin = 6)
plot(mod)

## Create output objects
mcmc.array <- mod$BUGSoutput$sims.array
dim(mcmc.array)

## Summary output
mod$BUGSoutput$summary

## Plot of contraband_found with Overlaid Predictions from Bayes Model for each Race & Year (FIGURE 4.6)
p_mean <- as_tibble(mod$BUGSoutput$mean$p.i)
p_mean$year <- tn_contraband_new_2018$year
p_mean$subject_race = tn_contraband_new_2018$subject_race
p_mean$lwr = apply(mod$BUGSoutput$sims.list$p.i,2,quantile,probs = 0.025)
p_mean$upr = apply(mod$BUGSoutput$sims.list$p.i,2,quantile,probs = 0.975)

ggplot(tn_contraband_new_2018 , aes(x = year, y = pi_hat)) +
  geom_point() +
  geom_line(data = p_mean, aes(x= year, y = value)) +
  geom_line(data = p_mean, aes(x= year, y = lwr, colour = "red"), linetype="dashed")+
  geom_line(data = p_mean, aes(x= year, y = upr, colour = "red"), linetype="dashed")+
  facet_wrap(~subject_race) +
  theme_bw() +
  theme(legend.position = "none")

## Trace Plots (FIGURE 4.7, 4.8, 4.9, 4.10)
shiny.array <- as.shinystan(mod$BUGSoutput$sims.array)
launch_shinystan(shiny.array)
