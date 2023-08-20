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
pine_flux <- read_csv("weather_flux/data/processed/wood_respiration/pine_CO2_clean.csv") %>%
  filter(CO2_resp_rate > 0)

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

# Normalize variables
pine_flux <- pine_flux %>%
  mutate(FMC_nor = FMC/max(pine_flux$FMC),
         T_nor = (mean_Tcham+273.15)/max(FMC_sim$temp_wood))



########## Bayesian model ##########

# Model = FMC + site, with dependent variable with a beta distribution
model_3  = brm(CO2_resp_rate ~ FMC_nor * T_nor + (1|site),data = pine_flux,iter = 3500,family ="beta", 
               control = list(adapt_delta = 0.96),seed=123)
summary(model_3)
model_3$fit
loo3 = loo(model_3)

# Model 3 ####

p_check_test = pp_check(model_3,ndraws=100)
pdf("figures/R_figs/p_check_model_3.pdf")
p_check_test
dev.off()

conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(model_3, conditions = conditions,
                              re_formula = NULL, method = "predict")

model_m0_test = plot(me_fit, plot = FALSE)[[1]] + facet_wrap(~site)			
pdf("model_3_CO2_FMC.pdf")
model_m0_test
dev.off()

model_m0_test = plot(me_fit, plot = FALSE)[[2]] + facet_wrap(~site)			
pdf("model_3_CO2_Temp.pdf")
model_m0_test
dev.off()

pcheck_m0_test = pp_check(model_3, type = "scatter_avg_grouped", group = "site") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
pdf("p_check_model_3_site.pdf")
pcheck_m0_test
dev.off()

plot_chain = plot(model_3)
pdf("chain_model_3.pdf")
plot_chain
dev.off()

effects_m <- me_fit[[1]] %>%
  mutate(effect1__ = effect1__*max(pine_flux$FMC))
effects_t <- me_fit[[2]] %>%
  mutate(effect1__ = effect1__*max(FMC_sim$temp_wood) - 273.15)
effects_mt <- me_fit[[3]] %>%
  mutate(effect1__ = effect1__*max(pine_flux$FMC),
         effect2__ = (as.numeric(as.character(effect2__))*max(FMC_sim$temp_wood)) - 273.15)

# Save files for plottin
write_csv(effects_m,"bayesian_model/bm_fits_m.csv")
write_csv(effects_t,"bayesian_model/bm_fits_t.csv")
write_csv(effects_mt,"bayesian_model/bm_fits_mt.csv")


# Archive ####
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
plot(model_3)

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

