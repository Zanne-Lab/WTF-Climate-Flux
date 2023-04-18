# Construct a Bayesian model to predict CO2 flux from FMC

# Load packages and set working directory
# setwd("WTF-Climate-Flux")
library(tidyverse)
library(Metrics)
library(brms)
library(tidybayes)
library(modelr)
library(lubridate)

########## Load in data ##########

# Only take positive resp rates so beta model can be run
# Normalize FMC values between 0 and 1 to reduce divergent models
pine_flux <- read_csv("weather_flux/data/processed/wood_respiration/pine_CO2_clean.csv") %>%
  filter(CO2_resp_rate > 0)
pine_flux <- mutate(pine_flux,FMC_nor = FMC/max(pine_flux$FMC))

FMC_sim <- read_csv("FMC_mechanistic_model/fuel_moisture_output.csv") %>%
  separate(1,into=c("site","date","fuel_stick","fuel_block"),sep=";") %>%
  mutate_at(c("fuel_stick","fuel_block"),as.numeric) %>%
  mutate(date = as_datetime(date)) %>%
  filter(site!="HQ_AWC") %>%
  mutate(FMC_nor = fuel_block/max(pine_flux$FMC))
FMC_sim$site[FMC_sim$site=="STICK"] <- "STCK"
write_csv(FMC_sim,"FMC_mechanistic_model/fuel_moisture_output_rf.csv")



########## Bayesian model ##########

# Model = FMC + site, with dependent variable with a beta distribution
m1 <- brm(CO2_resp_rate ~ FMC_nor + (1|site),
          data = pine_flux,iter = 5000,family="beta")
summary(m1)
m1$fit

# Posterior predictive check
pp_check(m1,ndraws=100)
pp_check(m1, type = "scatter_avg_grouped", group = "site") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)

# Check MCMC chain
plot(m1)

# Information criteria
loo_m1 = loo(m1)

# Assessing uncertainty
conditional_effects(m1,"FMC_nor")
conditional_effects(m1,"FMC_nor",spaghetti=T,ndraws=500)
m_fit = conditional_effects(m1,"FMC_nor")

conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(m1, conditions = conditions,
                              re_formula = NULL, method = "predict")

plot(me_fit, ncol = 5, points = TRUE)
plot(me_fit, plot = FALSE)[[1]] + facet_wrap(~site)

bm_fits <- me_fit[[1]] %>%
  mutate(effect1__ = effect1__*max(pine_flux$FMC))
write_csv(bm_fits,"bayesian_model/bm_fits.csv")



########## Prediction on mechanistic model output ##########

# Use model to create time-resoled respiration rates
pred <- predict(m0_n, newdata = FMC_sim)
pred <- as.data.frame(pred.2)
write_csv(pred,"bayesian_model/FMC_pred.csv")

