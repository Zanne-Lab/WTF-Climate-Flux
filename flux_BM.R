# Construct a Bayesian model to predict CO2 flux from FMC

# Load packages and set working directory
# setwd("WTF-Climate-Flux")
library(tidyverse)
library(Metrics)
library(brms)
library(tidybayes)
library(modelr)
library(lubridate)
library(bayesplot)

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

# Models ####

model_1  = brm(CO2_resp_rate ~ FMC_nor + (1|site),data = pine_flux,iter = 3500,family ="beta",
               control = list(adapt_delta = 0.96),seed=123)
summary(model_1)
model_1$fit
loo1 = loo(model_1)

model_2  = brm(CO2_resp_rate ~ FMC_nor + T_nor + (1|site),data = pine_flux,iter = 5000,family ="beta", 
               control = list(adapt_delta = 0.96),seed=123)
summary(model_2)
model_2$fit
loo2 = loo(model_2)

model_3  = brm(CO2_resp_rate ~ FMC_nor * T_nor + (1|site),data = pine_flux,iter = 5000,family ="beta", 
               control = list(adapt_delta = 0.96),seed=123)
summary(model_3)
model_3$fit
loo3 = loo(model_3)

library(bayestestR)
p_map(model_3)

# Model comparison ####

# LOO ####
loo_comp = loo_compare(loo1,loo2,loo3)
print(loo_comp, digits = 2)

# Model weights ####
fit1 <- add_criterion(model_1, "loo")
fit2 <- add_criterion(model_2, "loo")
fit3 <- add_criterion(model_3, "loo")
model_weights(fit1,fit2,fit3, weights = "loo")

# IC comparison ####
compare_ic(loo1,loo2,loo3)

# Posterior predictive check ####

#..Model 1 ####

p_check_test = pp_check(model_1,ndraws=100)
pdf("bayesian_model/p_check_model_1.pdf")
p_check_test
dev.off()

conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(model_1, conditions = conditions,
                              re_formula = NULL, method = "predict")

model_m0_test = plot(me_fit, plot = FALSE)[[1]] + facet_wrap(~site)			
pdf("bayesian_model/model_1_CO2_FMC.pdf")
model_m0_test
dev.off()

pcheck_m0_test = pp_check(model_1, type = "scatter_avg_grouped", group = "site") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
pdf("bayesian_model/p_check_model_1_site.pdf")
pcheck_m0_test
dev.off() 

plot_chain = plot(model_1)
pdf("bayesian_model/chain_model_1.pdf")
plot_chain
dev.off()

#..Model 2 ####

p_check_test = pp_check(model_2,ndraws=100)
pdf("bayesian_model/p_check_model_2.pdf")
p_check_test
dev.off()

conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(model_2, conditions = conditions,
                              re_formula = NULL, method = "predict")

model_m0_test = plot(me_fit, plot = FALSE)[[1]] + facet_wrap(~site)			
pdf("bayesian_model/model_2_CO2_FMC.pdf")
model_m0_test
dev.off()

model_m0_test = plot(me_fit, plot = FALSE)[[2]] + facet_wrap(~site)			
pdf("bayesian_model/model_2_CO2_Temp.pdf")
model_m0_test
dev.off()

pcheck_m0_test = pp_check(model_2, type = "scatter_avg_grouped", group = "site") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
pdf("bayesian_model/p_check_model_2_site.pdf")
pcheck_m0_test
dev.off()

plot_chain = plot(model_2)
pdf("bayesian_model/chain_model_2.pdf")
plot_chain
dev.off()

#..Model 3 ####

p_check_test = pp_check(model_3,ndraws=100)
pdf("figures/R_figs/S2_A.pdf",width=6,height=6)
p_check_test
dev.off()

conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(model_3, conditions = conditions,
                              re_formula = NULL, method = "predict")

model_m0_test = plot(me_fit, plot = FALSE)[[1]] + facet_wrap(~site)			
pdf("bayesian_model/model_3_CO2_FMC.pdf")
model_m0_test
dev.off()

model_m0_test = plot(me_fit, plot = FALSE)[[2]] + facet_wrap(~site)			
pdf("bayesian_model/model_3_CO2_Temp.pdf")
model_m0_test
dev.off()

pcheck_m0_test = pp_check(model_3, type = "scatter_avg_grouped", group = "site") + 
  geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
pdf("figures/R_figs/S2_B.pdf",width=12,height=6)
pcheck_m0_test
dev.off()

plot_chain = plot(model_3)[[1]]
pdf("figures/R_figs/S2_C.pdf",width=12,height=10)
plot_chain
dev.off()

# Prediction for mass loss ####

#..Model 1 ####

for_mass_loss = predict(model_1, newdata = FMC_sim,ndraws = 300)
for_mass_loss = as.data.frame(for_mass_loss)
write_csv(for_mass_loss,"for_mass_loss_model_1_300.csv")

#..Model 2 ####

for_mass_loss = predict(model_2, newdata = FMC_sim,ndraws = 300)
for_mass_loss = as.data.frame(for_mass_loss)
write_csv(for_mass_loss,"for_mass_loss_model_2_300.csv")

#..Model 3 ####

for_mass_loss = predict(model_3, newdata = FMC_sim,ndraws = 300)
for_mass_loss = as.data.frame(for_mass_loss)
write_csv(for_mass_loss,"for_mass_loss_model_3_300.csv")

# Prediction for time series ####

#..Model 1 ####

pred.1a 	= predict(model_1, newdata = FMC_sim[1:17652,],ndraws = 2000)
pred.1a     = as.data.frame(pred.1a)
write_csv(pred.1a,"pred.1a.csv")

pred.1b 	= predict(model_1, newdata = FMC_sim[17653:35304,],ndraws = 2000)
pred.1b     = as.data.frame(pred.1b)
write_csv(pred.1b,"pred.1b.csv")

pred.1c 	= predict(model_1, newdata = FMC_sim[35305:52956,],ndraws = 2000)
pred.1c     = as.data.frame(pred.1c)
write_csv(pred.1c,"pred.1c.csv")

pred.1d 	= predict(model_1, newdata = FMC_sim[52957:70608,],ndraws = 2000)
pred.1d     = as.data.frame(pred.1d)
write_csv(pred.1d,"pred.1d.csv")

pred.1e 	= predict(model_1, newdata = FMC_sim[70609:88260,],ndraws = 2000)
pred.1e     = as.data.frame(pred.1e)
write_csv(pred.1e,"pred.1e.csv")

pred.1f 	= predict(model_1, newdata = FMC_sim[88261:105912,],ndraws = 2000)
pred.1f     = as.data.frame(pred.1f)
write_csv(pred.1f,"pred.1f.csv")

pred.1g 	= predict(model_1, newdata = FMC_sim[105913:123564,],ndraws = 2000)
pred.1g     = as.data.frame(pred.1g)
write_csv(pred.1g,"pred.1g.csv")

pred.1h 	= predict(model_1, newdata = FMC_sim[123565:141216,],ndraws = 2000)
pred.1h     = as.data.frame(pred.1h)
write_csv(pred.1h,"pred.1h.csv")

pred.1i 	= predict(model_1, newdata = FMC_sim[141217:158868,],ndraws = 2000)
pred.1i     = as.data.frame(pred.1i)
write_csv(pred.1i,"pred.1i.csv")

pred.1j 	= predict(model_1, newdata = FMC_sim[158869:176525,],ndraws = 2000)
pred.1j     = as.data.frame(pred.1j)
write_csv(pred.1j,"pred.1j.csv")

M1.1  <- read_csv("pred.1a.csv")
M2.1  <- read_csv("pred.1b.csv")
M3.1  <- read_csv("pred.1c.csv")
M4.1  <- read_csv("pred.1d.csv")
M5.1  <- read_csv("pred.1e.csv")
M6.1  <- read_csv("pred.1f.csv")
M7.1  <- read_csv("pred.1g.csv")
M8.1  <- read_csv("pred.1h.csv")
M9.1  <- read_csv("pred.1i.csv")
M10.1 <- read_csv("pred.1j.csv")

pred_model_1_2000  = as.data.frame(rbind(M1.1,M2.1,M3.1,M4.1,M5.1,M6.1,M7.1,M8.1,M9.1,M10.1))
write_csv(pred_model_1_2000,"pred_model_1_2000.csv")

h_M1 <- ggplot(FMC_sim, aes(as.Date(date))) + geom_ribbon(aes(ymin = pred_model_1_2000$Q2.5, ymax = pred_model_1_2000$Q97.5), fill = "grey70") + 
  geom_line(aes(y = pred_model_1_2000$Estimate)) + facet_wrap(~FMC_sim$site)
pdf("M1_time_series.pdf")
h_M1
dev.off()

h_M2A_1000 <- ggplot(FMC_sim, aes(FMC)) + geom_ribbon(aes(ymin = pred_model_1_2000$Q2.5, ymax = pred_model_1_2000$Q97.5), fill = "grey70") + 
  geom_line(aes(y = pred_model_1_2000$Estimate)) + facet_wrap(~FMC_sim$site)
pdf("M1_FMC.pdf")
h_M2A_1000
dev.off()

#..Model 2 ####

pred.2a 	= predict(model_2, newdata = FMC_sim[1:17652,],ndraws = 2000)
pred.2a     = as.data.frame(pred.2a)
write_csv(pred.2a,"pred.2a.csv")

pred.2b 	= predict(model_2, newdata = FMC_sim[17653:35304,],ndraws = 2000)
pred.2b     = as.data.frame(pred.2b)
write_csv(pred.2b,"pred.2b.csv")

pred.2c 	= predict(model_2, newdata = FMC_sim[35305:52956,],ndraws = 2000)
pred.2c     = as.data.frame(pred.2c)
write_csv(pred.2c,"pred.2c.csv")

pred.2d 	= predict(model_2, newdata = FMC_sim[52957:70608,],ndraws = 2000)
pred.2d     = as.data.frame(pred.2d)
write_csv(pred.2d,"pred.2d.csv")

pred.2e 	= predict(model_2, newdata = FMC_sim[70609:88260,],ndraws = 2000)
pred.2e     = as.data.frame(pred.2e)
write_csv(pred.2e,"pred.2e.csv")

pred.2f 	= predict(model_2, newdata = FMC_sim[88261:105912,],ndraws = 2000)
pred.2f     = as.data.frame(pred.2f)
write_csv(pred.2f,"pred.2f.csv")

pred.2g 	= predict(model_2, newdata = FMC_sim[105913:123564,],ndraws = 2000)
pred.2g     = as.data.frame(pred.2g)
write_csv(pred.2g,"pred.2g.csv")

pred.2h 	= predict(model_2, newdata = FMC_sim[123565:141216,],ndraws = 2000)
pred.2h     = as.data.frame(pred.2h)
write_csv(pred.2h,"pred.2h.csv")

pred.2i 	= predict(model_2, newdata = FMC_sim[141217:158868,],ndraws = 2000)
pred.2i     = as.data.frame(pred.2i)
write_csv(pred.2i,"pred.2i.csv")

pred.2j 	= predict(model_2, newdata = FMC_sim[158869:176525,],ndraws = 2000)
pred.2j     = as.data.frame(pred.2j)
write_csv(pred.2j,"pred.2j.csv")

M1.2  <- read_csv("pred.2a.csv")
M2.2  <- read_csv("pred.2b.csv")
M3.2  <- read_csv("pred.2c.csv")
M4.2  <- read_csv("pred.2d.csv")
M5.2  <- read_csv("pred.2e.csv")
M6.2  <- read_csv("pred.2f.csv")
M7.2  <- read_csv("pred.2g.csv")
M8.2  <- read_csv("pred.2h.csv")
M9.2  <- read_csv("pred.2i.csv")
M10.2 <- read_csv("pred.2j.csv")

pred_model_2_2000  = as.data.frame(rbind(M1.2,M2.2,M3.2,M4.2,M5.2,M6.2,M7.2,M8.2,M9.2,M10.2))
write_csv(pred_model_2_2000,"pred_model_2_2000.csv")

h_M1 <- ggplot(FMC_sim, aes(as.Date(date))) + geom_ribbon(aes(ymin = pred_model_2_2000$Q2.5, ymax = pred_model_2_2000$Q97.5), fill = "grey70") + 
  geom_line(aes(y = pred_model_2_2000$Estimate)) + facet_wrap(~FMC_sim$site)
pdf("M2_time_series.pdf")
h_M1
dev.off()

h_M2A_1000 <- ggplot(FMC_sim, aes(FMC)) + geom_ribbon(aes(ymin = pred_model_2_2000$Q2.5, ymax = pred_model_2_2000$Q97.5), fill = "grey70") + 
  geom_line(aes(y = pred_model_2_2000$Estimate)) + facet_wrap(~FMC_sim$site)
pdf("M2_FMC.pdf")
h_M2A_1000
dev.off()

#h_M2A_1000 <- ggplot(FMC_sim, aes(T_wood)) + geom_ribbon(aes(ymin = pred_model_2_2000$Q2.5, ymax = pred_model_2_2000$Q97.5), fill = "grey70") + 
#  geom_line(aes(y = pred_model_2_2000$Estimate)) + facet_wrap(~FMC_sim$site)
#pdf("M2_Temp.pdf")
#h_M2A_1000
#dev.off()

#..Model 3 ####

pred.3a 	= predict(model_3, newdata = FMC_sim[1:17652,],ndraws = 2000)
pred.3a     = as.data.frame(pred.3a)
write_csv(pred.3a,"bayesian_model/pred.3a.csv")

pred.3b 	= predict(model_3, newdata = FMC_sim[17653:35304,],ndraws = 2000)
pred.3b     = as.data.frame(pred.3b)
write_csv(pred.3b,"bayesian_model/pred.3b.csv")

pred.3c 	= predict(model_3, newdata = FMC_sim[35305:52956,],ndraws = 2000)
pred.3c     = as.data.frame(pred.3c)
write_csv(pred.3c,"bayesian_model/pred.3c.csv")

pred.3d 	= predict(model_3, newdata = FMC_sim[52957:70608,],ndraws = 2000)
pred.3d     = as.data.frame(pred.3d)
write_csv(pred.3d,"bayesian_model/pred.3d.csv")

pred.3e 	= predict(model_3, newdata = FMC_sim[70609:88260,],ndraws = 2000)
pred.3e     = as.data.frame(pred.3e)
write_csv(pred.3e,"bayesian_model/pred.3e.csv")

pred.3f 	= predict(model_3, newdata = FMC_sim[88261:105912,],ndraws = 2000)
pred.3f     = as.data.frame(pred.3f)
write_csv(pred.3f,"bayesian_model/pred.3f.csv")

pred.3g 	= predict(model_3, newdata = FMC_sim[105913:123564,],ndraws = 2000)
pred.3g     = as.data.frame(pred.3g)
write_csv(pred.3g,"bayesian_model/pred.3g.csv")

pred.3h 	= predict(model_3, newdata = FMC_sim[123565:141216,],ndraws = 2000)
pred.3h     = as.data.frame(pred.3h)
write_csv(pred.3h,"bayesian_model/pred.3h.csv")

pred.3i 	= predict(model_3, newdata = FMC_sim[141217:158868,],ndraws = 2000)
pred.3i     = as.data.frame(pred.3i)
write_csv(pred.3i,"bayesian_model/pred.3i.csv")

pred.3j 	= predict(model_3, newdata = FMC_sim[158869:176525,],ndraws = 2000)
pred.3j     = as.data.frame(pred.3j)
write_csv(pred.3j,"bayesian_model/pred.3j.csv")

M1.3  <- read_csv("pred.3a.csv")
M2.3  <- read_csv("pred.3b.csv")
M3.3  <- read_csv("pred.3c.csv")
M4.3  <- read_csv("pred.3d.csv")
M5.3  <- read_csv("pred.3e.csv")
M6.3  <- read_csv("pred.3f.csv")
M7.3  <- read_csv("pred.3g.csv")
M8.3  <- read_csv("pred.3h.csv")
M9.3  <- read_csv("pred.3i.csv")
M10.3 <- read_csv("pred.3j.csv")


pred_model_3_2000  = as.data.frame(rbind(M1.3,M2.3,M3.3,M4.3,M5.3,M6.3,M7.3,M8.3,M9.3,M10.3))
write_csv(pred_model_3_2000,"bayesian_model/pred_model_3_2000.csv")

h_M1 <- ggplot(FMC_sim, aes(as.Date(date))) + geom_ribbon(aes(ymin = pred_model_3_2000$Q2.5, ymax = pred_model_3_2000$Q97.5), fill = "grey70") + 
  geom_line(aes(y = pred_model_3_2000$Estimate)) + facet_wrap(~FMC_sim$site)
pdf("bayesian_model/M3_time_series.pdf")
h_M1
dev.off()

h_M2A_1000 <- ggplot(FMC_sim, aes(FMC)) + geom_ribbon(aes(ymin = pred_model_3_2000$Q2.5, ymax = pred_model_3_2000$Q97.5), fill = "grey70") + 
  geom_line(aes(y = pred_model_3_2000$Estimate)) + facet_wrap(~FMC_sim$site)
pdf("bayesian_model/M3_FMC.pdf")
h_M2A_1000
dev.off()

#h_M2A_1000 <- ggplot(FMC_sim, aes(T_wood)) + geom_ribbon(aes(ymin = pred_model_3_2000$Q2.5, ymax = pred_model_3_2000$Q97.5), fill = "grey70") + 
#  geom_line(aes(y = pred_model_3_2000$Estimate)) + facet_wrap(~FMC_sim$site)
#pdf("bayesian_model/M3_Temp.pdf")
#h_M2A_1000
#dev.off()


conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(model_3, conditions = conditions,re_formula = NULL, method = "predict")


bayes_R2(model_3) %>% round(digits = 3) 




# Final model: Model 3 (FMC*T_nor + site, with dependent variable with a beta distribution) ####
# Save files for plotting
effects_m <- me_fit[[1]] %>%
  mutate(effect1__ = effect1__*max(pine_flux$FMC))
effects_t <- me_fit[[2]] %>%
  mutate(effect1__ = effect1__*max(FMC_sim$temp_wood) - 273.15)
effects_mt <- me_fit[[3]] %>%
  mutate(effect1__ = effect1__*max(pine_flux$FMC),
         effect2__ = (as.numeric(as.character(effect2__))*max(FMC_sim$temp_wood)) - 273.15)
write_csv(effects_m,"bayesian_model/bm_fits_m.csv")
write_csv(effects_t,"bayesian_model/bm_fits_t.csv")
write_csv(effects_mt,"bayesian_model/bm_fits_mt.csv")

