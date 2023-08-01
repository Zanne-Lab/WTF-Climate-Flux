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
pine_flux <- pine_flux %>%
  mutate(FMC_nor = FMC/max(pine_flux$FMC),
         T_nor = mean_Tcham/max(pine_flux$mean_Tcham))

FMC_sim <- read_csv("FMC_mechanistic_model/fuel_moisture_output.csv") %>%
  separate(1,into=c("site","date","fuel_stick","fuel_block",
                    "temp_stick","temp_wood"),sep=";") %>%
  mutate_at(c("fuel_stick","fuel_block",
              "temp_stick","temp_wood"),as.numeric) %>%
  mutate(date = as_datetime(date)) %>%
  filter(site!="HQ_AWC") %>%
  mutate(FMC_nor = fuel_block/max(pine_flux$FMC))
FMC_sim$site[FMC_sim$site=="STICK"] <- "STCK"
write_csv(FMC_sim,"FMC_mechanistic_model/fuel_moisture_output_t3.csv")



########## Bayesian model ##########

# Model = FMC + site, with dependent variable with a beta distribution
m1 <- brm(CO2_resp_rate ~ FMC_nor*T_nor + (1|site),
          data = pine_flux,iter = 3000,family="beta",
          control = list(adapt_delta = 0.96),seed=123)
summary(m1)
m1$fit

# Figure aesthetics for uncertainty plots
library(cowplot)
fig_aes <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="gray95"),
        strip.text.x = element_text(size = 11))

# Posterior predictive check
p1 <- pp_check(m1,ndraws=100) + 
  fig_aes +
  theme(legend.position = c(0.8,0.8))

p2 <- pp_check(m1, type = "scatter_avg_grouped", group = "site") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)

png("figures/S2_BM_pp.png",width=3000,height=1500,res=180)
plot_grid(p1,p2,
          ncol=2,nrow=1)
dev.off()

# Check MCMC chain
plot(m1)

png("figures/S2_BM_MCMC.png",width=3000,height=1800,res=250)
plot(m1)
dev.off()

# Information criteria
loo_m1 = loo(m1)

# Assessing uncertainty
conditional_effects(m1,"FMC_nor")
conditional_effects(m1,"T_nor")
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
bm_fits_t <- me_fit[[2]] %>%
  mutate(effect1__ = effect1__*max(pine_flux$mean_Tcham))
write_csv(bm_fits,"bayesian_model/bm_fits_m.csv")
write_csv(bm_fits_t,"bayesian_model/bm_fits_t.csv")



########## Prediction on mechanistic model output ##########

# Use model to create time-resoled respiration rates
pred <- predict(m0_n, newdata = FMC_sim)
pred <- as.data.frame(pred.2)
write_csv(pred,"bayesian_model/FMC_pred.csv")

