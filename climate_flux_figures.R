# Create figures for climate/flux paper

# Load packages and set working directory
# setwd("WTF-Climate-Flux")
library(tidyverse)
library(lubridate)
library(ggh4x)
library(forcats)
library(zoo)
library(cowplot)
library(ggpubr)

########## Load in data ##########
wthr_FMC <- read_csv("weather_flux/data/processed/weather_stations/wthr_1hr_FMC.csv") %>%
  filter(site!="HQ_AWC")
SILO <- read_csv("weather_flux/data/external_data/SILO_processed.csv") %>%
  filter(site!="HQ_AWC")
pine_flux <- read_csv("weather_flux/data/processed/wood_respiration/pine_CO2_clean.csv") %>%
  filter(CO2_resp_rate > 0)
native_flux <- read_csv("weather_flux/data/processed/wood_respiration/native_CO2_clean.csv")
bm_fits <- read_csv("bayesian_model/bm_fits.csv")
FMC_sim <- read_csv("FMC_mechanistic_model/fuel_moisture_output_rf.csv")
time_flux <- read_csv("bayesian_model/FMC_pred.csv") %>%
  separate(1,into=c("Estimate","Est.Error","Q2.5","Q97.5"),sep=",") %>%
  mutate_at(c("Estimate","Est.Error","Q2.5","Q97.5"),as.numeric)
int_mass_loss <- read_csv("mass_loss.csv") %>%
  separate(1,into=c("site","months","Carbon_por"),sep=",") %>%
  mutate_at(c("site"),as.character) %>%
  mutate_at(c("months","Carbon_por"),as.numeric)

# HQ_AWC is removed from plotting as it was not used in analysis

# SILO historical dataset
SILO_hist <- SILO %>%
  mutate(year = year(day),
         mean_temp = (max_temp+min_temp)/2) %>%
  group_by(site,year) %>%
  summarize(an_Rain = sum(daily_rain, na.rm = TRUE),
            an_AirTC = mean(mean_temp),
            an_AirTC_max = max(max_temp),
            an_AirTC_min = min(min_temp)) %>%
  ungroup() %>%
  group_by(site) %>%
  summarize(hist_Rain_max = max(an_Rain),
            hist_Rain_min = min(an_Rain),
            hist_Rain_mean = mean(an_Rain),
            hist_AirTC_mean = mean(an_AirTC),
            hist_AirTC_max = max(an_AirTC_max),
            hist_AirTC_min = min(an_AirTC_min)) %>%
  ungroup()



########## Set color palettes and common aesthetics ##########
library(palettetown)

# Site palettes
p_site <- pokepal(254,5)
p_DRO <- p_site[1]
p_PNW <- p_site[4]
p_site_a <- c(p_site[1],p_site[3],p_site[2],p_site[5],p_site[4])
p_site_strip <- strip_themed(background_x = elem_list_rect(fill = p_site_a))

# Species palettes
p_pira <- "#700A1F"
p_DRO_sp <- pokepal(44,10)
p_PNW_sp <- pokepal(141,6)
p_DRO_sp_all <- c("#7090A0","#F8C050","#486878","#603000","#A07030","#D0A060",
                     "#E84800","#F87000",p_pira,"#103058","#805010")
p_PNW_sp_all <- c("#D8C088","#F8E0A8","#B09860","#686868","#685820",
                  p_pira,"#D8D8D0")
p_all_sp <- c("#7090A0","#F8C050","#486878","#603000","#A07030","#D0A060",
                    "#D8C088","#F8E0A8","#B09860","#686868","#E84800","#F87000",
                    "#685820","#103058","#805010","#D8D8D0")

# Colors for weather plots
p_WTF <- "#7870C8"
p_POWER <- "#A84890"
p_CHRS <- "#680850"
p_SILO <- "#333333"
p_calc <- "#303088"

# Colors for simulation plots
p_stick <- "#709890"
p_block <- "#A81048"

# Common aesthetics
fig_aes <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="gray95"))



########## Main text figures ##########

#..Fig 1. Bayesian model from pine blocks ####
bm <- ggplot() + 
  geom_smooth(bm_fits,mapping=aes(effect1__,estimate__,color=site)) + 
  scale_color_manual(name="Site",values=p_site) +
  geom_ribbon(bm_fits,mapping=aes(x=effect1__,y=estimate__,
                                  ymin=lower__,ymax=upper__,fill=site),
              alpha=0.2,color=NA) +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  scale_fill_manual(name="Site",values=p_site) +
  xlab("FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") + 
  fig_aes
print(bm)

png("figures/S_BM_pines.png",width=2500,height=1500,res=250)
bm + geom_point(pine_flux,mapping=aes(FMC,CO2_resp_rate),
                color=p_pira,shape=17,alpha=0.8)
dev.off()



#..Fig 2. FMC stick calibration vs. block simulations ####
FMC_sim_p <- FMC_sim %>%
  select(-FMC_nor) %>%
  pivot_longer(!c(date,site),names_to="var",values_to="FMC") %>%
  mutate(Model = ifelse(var=="fuel_stick",
                            "Stick Calibration","Block Simulation"))

png("figures/Fig_FMC_cal_sim_rain.png",width=2500,height=1500,res=250)
ggplot() + 
  geom_line(data=FMC_sim_p,mapping=aes(date,FMC,color=Model),
            alpha=0.5) +
  scale_color_manual(values=c(p_block,p_stick)) +
  geom_bar(data=wthr_FMC,mapping=aes(date,Rain_mm_Tot),
           stat="identity",color="blue") +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block),
            alpha=0.5,color=p_block) +
  xlab("Date") + 
  scale_y_continuous(name="FMC (%)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  fig_aes
dev.off()

#......DRO and PNW only ####
png("figures/Fig_FMC_cal_sim_rain_DP.png",width=2500,height=1200,res=250)
ggplot() + 
  geom_line(data=filter(FMC_sim_p,site%in%c("DRO","PNW")),
            mapping=aes(date,FMC,color=Model),
            alpha=0.5) +
  scale_color_manual(values=c(p_block,p_stick)) +
  geom_bar(data=filter(wthr_FMC,site%in%c("DRO","PNW")),
           mapping=aes(date,Rain_mm_Tot),
           stat="identity",color="blue") +
  geom_line(data=filter(FMC_sim,site%in%c("DRO","PNW")),
            mapping=aes(date,fuel_block),
            alpha=0.5,color=p_block) +
  xlab("Date") + 
  scale_y_continuous(name="FMC (%)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  facet_wrap(~site) +
  fig_aes
dev.off()



#..Fig 3. Time-resolved CO2 Flux ####
time_flux2 <- data.frame(site = wthr_FMC$site,
                         date = wthr_FMC$date,
                         Estimate = time_flux$Estimate,
                         Est.Error = time_flux$Est.Error,
                         Q2.5 = time_flux$Q2.5,
                         Q97.5 = time_flux$Q97.5)

#......All sites ####
png("figures/Fig_CO2_time.png",width=2500,height=1500,res=250)
ggplot(time_flux2) +
  geom_line(mapping=aes(date,Estimate,color=site),alpha=0.5) +
  geom_ribbon(mapping=aes(x=date,y=Estimate,ymin=Q2.5,ymax=Q97.5,
                          fill=site),
              alpha=0.3,color=NA) +
  scale_color_manual(name="Site",values=p_site) +
  scale_fill_manual(name="Site",values=p_site) +
  xlab("Date") + ylab("CO2 Flux (ug CO2/s/g)") +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  fig_aes
dev.off()

#......DRO and PNW only ####
png("figures/Fig_CO2_time_DP.png",width=2500,height=1200,res=250)
ggplot(filter(time_flux2,site%in%c("DRO","PNW"))) +
  geom_line(mapping=aes(date,Estimate,color=site),alpha=0.5) +
  geom_ribbon(mapping=aes(x=date,y=Estimate,ymin=Q2.5,ymax=Q97.5,
                          fill=site),
              alpha=0.3,color=NA) +
  scale_color_manual(name="Site",values=c(p_DRO,p_PNW)) +
  scale_fill_manual(name="Site",values=c(p_DRO,p_PNW)) +
  xlab("Date") + ylab("CO2 Flux (ug CO2/s/g)") +
  facet_grid(~site) +
  fig_aes
dev.off()



#..Fig 4. Natives and FMC/Flux Simulations ####
sim_flux <- data.frame(site = FMC_sim$site,
                       Block_FMC = FMC_sim$fuel_block,
                       Sim_CO2 = time_flux$Estimate,
                       Sim_Q2.5 = time_flux$Q2.5,
                       Sim_Q97.5 = time_flux$Q97.5)

# DRO + BM
b1 <- ggplot() + 
  geom_smooth(filter(bm_fits,site=="DRO"),
              mapping=aes(effect1__,estimate__),color=p_DRO) + 
  geom_ribbon(filter(bm_fits,site=="DRO"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.3,color=NA,fill=p_DRO) +
  geom_point(filter(native_flux,site=="DRO"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_DRO_sp_all) +
  xlab("Measured Block FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") + 
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

# PNW + BM
b2 <- ggplot() + 
  geom_smooth(filter(bm_fits,site=="PNW"),
              mapping=aes(effect1__,estimate__),color=p_PNW) + 
  geom_ribbon(filter(bm_fits,site=="PNW"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,
                         color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_PNW_sp_all) +
  xlab("Measured Block FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") + 
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes


# DRO + simulations 
d1 <- ggplot() +
  geom_point(data=filter(sim_flux,site=="DRO"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.3,color=p_DRO) +
  geom_ribbon(data=filter(sim_flux,site=="DRO"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.3,color=NA,fill=p_DRO) +
  geom_point(data=filter(native_flux,site=="DRO"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Simulated Block FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") +
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes


# PNW + simulations
d2 <- ggplot() +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.3,color=p_PNW) +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.85) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  xlab("Simulated Block FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") +
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

# PNW + simulations without scale
ggplot() +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.3,color=p_PNW) +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.85) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  #xlab("Simulated Block FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") +
  fig_aes +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


#....... Combined Plot ####
png("figures/Fig_natives_BM_sim.png",width=2500,height=1800,res=250)
plot_grid(b1 + theme(legend.position="none",
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()),
          d1 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.title.x = element_blank()),
          b2 + theme(legend.position="none"),
          d2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank()),
          nrow=2,ncol=2,
          labels=c("A","B","C","D"),
          align="hv",
          axis="tblr")
dev.off()



#..Fig 5. Natives and flux vs. mass loss ####
ML_com <- pine_flux %>%
  mutate(pro.mass.loss = 1 - harvest_dry_wt/init_dry_wt) %>%
  group_by(site,months) %>%
  summarize(mean_pro_ML = mean(pro.mass.loss,na.rm=TRUE),
            sd_pro_ML = sd(pro.mass.loss,na.rm=TRUE)) %>%
  right_join(int_mass_loss,by=c("site","months")) %>%
  mutate(mean_pro_ML = round(mean_pro_ML,2),
         sd_pro_ML = round(sd_pro_ML,2),
         Carbon_por = round(Carbon_por,2))

# Pines
png("figures/Fig_pine_mass_loss_comparison.png",width=2500,height=1600,res=250)
ggplot(ML_com,aes(mean_pro_ML,Carbon_por,color=months)) +
  geom_abline(intercept=0, slope=1) +
  geom_point(size=2.2,alpha=0.95,shape=17) +
  scale_color_gradient(low="#a2b3ba",high="#2c3133") +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  xlab("Carbon Loss (measured)") +
  ylab("Carbon Flux (simulated)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes
dev.off()

ML_natives <- native_flux %>%
  #mutate(pro.mass.loss = 1 - harvest_dry_wt/init_dry_wt) %>%
  group_by(site,months,Species.Code) %>%
  summarize(mean_pro_ML = mean(pro.mass.loss,na.rm=TRUE),
            sd_pro_ML = sd(pro.mass.loss,na.rm=TRUE)) %>%
  left_join(int_mass_loss,by=c("site","months")) %>%
  mutate(mean_pro_ML = round(mean_pro_ML,2),
         sd_pro_ML = round(sd_pro_ML,2),
         Carbon_por = round(Carbon_por,2)) %>%
  mutate(months = as.character(months))

png("figures/Fig_native_mass_loss_comparison.png",width=2500,height=1200,res=250)
ggplot(ML_natives,aes(mean_pro_ML,Carbon_por,color=Species.Code)) +
  geom_abline(intercept=0,slope=1) +
  geom_point(alpha=0.80,size=2.2) +
  scale_color_manual(values=p_all_sp,
                     name="Species") +
  facet_wrap(~site) +
  xlab("Carbon Loss (measured)") +
  ylab("Carbon Flux (simulated)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes
dev.off()



########## Supplementary Figures ##########

#..FMC mechanistic model ####
#......FMC stick observations and calibration ####
FMC_cal <- wthr_FMC %>%
  select(site,date,FMC_norm) %>%
  left_join(filter(FMC_sim_p,var=="fuel_stick"),
            by=c("site","date")) %>%
  select(-Model,-var) %>%
  pivot_longer(!c(date,site),names_to="Stick_FMC",values_to="FMC") %>%
  mutate(Stuck_FMC = ifelse(Stick_FMC=="FMC_norm",
                        "Measurements","Simulations"))

png("figures/S_stick_FMC.png",width=2500,height=1500,res=250)
ggplot(FMC_cal,aes(date,FMC,color=Stick_FMC)) +
  geom_line(alpha=0.5) +
  scale_color_manual(values=c(p_stick,p_WTF),
                     name="Stick FMC",
                     labels=c("Measurements (normalized)","Model Calibrations")) +
  xlab("Date") + ylab("Fuel moisture content (%)") +
  facet_wrap(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
dev.off()



#......Block simulations + field points ####
library(googledrive)
library(googlesheets4)
# Get sheet with times and pine block dry weight
FMC_cal <-drive_get("wtf_radiata_blocks_weight.csv") %>% 
  read_sheet(sheet = "calibrate_to_fuel_moisture_stick", col_names=T, 
             col_types=c("ccDnnn"))
detach(package:googledrive)
detach(package:googlesheets4)

FMC_cal2 <- FMC_cal %>%
  mutate(time = paste(time,"00",sep=":"),
         datetime = as_datetime(paste(date,time,sep=" ")),
         date = floor_date(datetime, "1 hour"),
         FMC = ((weight-dry_wt)/dry_wt)*100,
         site = case_when(site == "awc_hq" ~ "HQ_AWC", site == "dro" ~ "DRO",
                          site == "mtlwrf" ~ "MLRF", site == "mtlwsc" ~ "MLES",
                          site == "pnw" ~ "PNW", site == "stck" ~ "STCK")) %>%
  select(site,date,FMC)

block_FMC <- pine_flux %>%
  mutate(date = as_datetime(paste(harvest_date,"12:00:00"))) %>%
  group_by(site,date) %>%
  summarize(FMC = mean(FMC,na.rm=TRUE),
            sd_FMC = sd(FMC,na.rm=TRUE)) %>%
  rbind(FMC_cal2) %>%
  filter(site!="HQ_AWC")

png("figures/S_block_FMC.png",width=2500,height=1700,res=250)
ggplot() +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block),
            alpha=0.5,color=p_block) +
  geom_point(data=block_FMC,mapping=aes(date,FMC),
             color=p_pira,shape=17,size=1.8) +
  xlab("Date") + ylab("Fuel moisture content (%)") +
  facet_wrap(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
dev.off()

#..Weather data ####
#......Weather for each site (rainfall and temperature) ####
png("figures/S_temp_rain.png",width=2000,height=2000,res=250)
ggplot(wthr_FMC) + 
  #geom_point(aes(date,AirTC_Avg),color="red",alpha=0.2,size=0.25) +
  geom_line(aes(date,AirTC_Avg),stat="identity",color="red",alpha=0.5) + 
  geom_bar(aes(date,Rain_mm_Tot),stat="identity",color="blue") + 
  facet_grid2(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  xlab(label = "Date") + 
  scale_y_continuous(name="Air Temperature (C)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  fig_aes
dev.off()



#......Gap-filled weather data ####
pdf(file="figures/S_wthr_gap_filled.pdf",width=11,height=8.5)
ggplot(wthr_FMC,aes(date,AirTC_Avg,color=AirTC_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c(p_POWER,p_WTF)) + 
  xlab("Date") + ylab("Air temperature (C)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,ib_AirTC_Avg,color=ib_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c(p_calc,p_WTF)) +
  xlab("Date") + ylab("ibutton Air temperature (C)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot() + 
  geom_point(data=wthr_FMC,
             aes(date,Rain_mm_Tot,color=Rain_source),alpha=0.3,size=0.3) + 
  scale_color_manual(values=c(p_CHRS,p_POWER,p_WTF)) +
  geom_point(data=filter(wthr_FMC,Rain_source=="NASA_POWER"),
             aes(date,Rain_mm_Tot),alpha=0.3,size=0.3,color=p_POWER) +
  xlab("Date") + ylab("Rainfall (mm/hr)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,BP_mbar_Avg,color=BP_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c(p_POWER,p_WTF)) +
  xlab("Date") + ylab("Barometric pressure (mbar)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,RH_Avg,color=RH_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c(p_POWER,p_WTF)) +
  xlab("Date") + ylab("Relative humidity (%)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,WS2M)) + 
  geom_point(alpha=0.3,size=0.3,color=p_POWER) + 
  xlab("Date") + ylab("Wind speed (m/s)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,ALLSKY_SFC_SW_DWN)) + 
  geom_point(alpha=0.3,size=0.3,color=p_POWER) + 
  xlab("Date") + ylab("Shortwave radiation (W/m^2)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,ALLSKY_SFC_LW_DWN)) + 
  geom_point(alpha=0.3,size=0.3,color=p_POWER) + 
  xlab("Date") + ylab("Longwave radiation (W/m^2)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,solar_elevation)) + 
  geom_point(alpha=0.3,size=0.3,color=p_POWER) + 
  xlab("Date") + ylab("Solar elevation (degrees)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,solar_azimuth)) + 
  geom_point(alpha=0.3,size=0.3,color=p_calc) + 
  xlab("Date") + ylab("Solar azimuth (degrees)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
ggplot(wthr_FMC,aes(date,FMC_norm)) + 
  geom_point(alpha=0.3,size=0.3,color=p_WTF) + 
  xlab("Date") + ylab("Fuel moisture content, normalized (%)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
dev.off()



#......Table of annual rainfall and precipiation across experiment ####
wthr_rain_avg <- wthr_FMC %>%
  mutate(year = year(date),
         month = month(date),
         Rain_mm_f = ifelse(is.na(Rain_mm),PRECTOTCORR,Rain_mm)) %>%
  mutate(year_half = case_when(month%in%c(6:12) ~ "second",
                            TRUE ~ "first")) %>%
  group_by(site, year, year_half) %>%
  summarize(Rain_tot = sum(Rain_mm_Tot),
            Rain_tot_CCS = sum(Rain_mm_f)) %>%
  ungroup() %>%
  mutate(Rain_tot_yr = Rain_tot + dplyr::lag(Rain_tot),
         Rain_tot_yr_CCS = Rain_tot_CCS + dplyr::lag(Rain_tot_CCS)) %>%
  filter(year_half=="first") %>%
  group_by(site) %>%
  summarize(Rain_avg = mean(Rain_tot_yr),
            Rain_avg_CCS = mean(Rain_tot_yr_CCS))

wthr_TC_avg <- wthr_FMC %>%
  mutate(year = year(date)) %>%
  group_by(site,year) %>%
  summarize(AirTC_mean = mean(AirTC_Avg),
            AirTC_max = max(AirTC_Avg),
            AirTC_min = min(AirTC_Avg),
            ibTC_mean = mean(ib_AirTC_Avg),
            ibTC_max = max(ib_AirTC_Avg),
            ibTC_min = min(ib_AirTC_Avg))



#......Rainfall gap-filling comparison with SILO data ####
SILO_rain_f <- SILO %>%
  filter(day > as.POSIXlt("2018-06-04")) %>%
  filter(day < as.POSIXlt("2022-06-14")) %>%
  mutate(year_mon = as.yearmon(day)) %>%
  group_by(site,year_mon) %>%
  summarize(SILO = sum(daily_rain, na.rm = TRUE))

wthr_rain_val <- wthr_FMC %>%
  mutate(Rain_CP = ifelse(is.na(Rain_mm),PRECTOTCORR,Rain_mm)) %>%
  mutate(day = date(date),
         year_mon = as.yearmon(day)) %>%
  group_by(site,year_mon) %>%
  summarize(CHRS_filled = sum(Rain_mm_Tot),
            POWER_filled = sum(Rain_mm_Tot_P),
            CP_only = sum(Rain_CP))
wthr_rain <- wthr_FMC %>%
  mutate(day = date(date),
         year_mon = as.yearmon(day)) %>%
  group_by(site,year_mon) %>%
  count(Rain_source) %>%
  top_n(1) %>%
  select(-n) %>%
  left_join(wthr_rain_val,by=c("site","year_mon"))

png("figures/S_wthr_rain_comparison.png",width=2000,height=2000,res=250)
ggplot() + 
  geom_point(data=SILO_rain_f,aes(year_mon,SILO),color=p_SILO) + 
  geom_line(data=SILO_rain_f,aes(year_mon,SILO),color=p_SILO,linetype="dashed") + 
  geom_point(data=wthr_rain,aes(year_mon,CHRS_filled,color=Rain_source)) +
  geom_line(data=wthr_rain,aes(year_mon,CHRS_filled),color=p_WTF) + 
  scale_color_manual(name="Rainfall source",values =c(p_CHRS,p_WTF),
                     labels=c("CHRS CCS", "WTF Stations")) + 
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  xlab("Month") + ylab("Rainfall (mm/month)") +
  fig_aes
dev.off()








# Scratch ####
#..FMC stick v block: Simulations only ####
png("figures/Fig_FMC_cal_sim.png",width=2500,height=1500,res=250)
ggplot(FMC_sim_p,aes(date,FMC,color=Model)) + 
  geom_line(alpha=0.5) +
  scale_color_manual(values=c(p_block,p_stick)) +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block),
            alpha=0.3,color=p_block) +
  xlab("Date") + ylab("FMC (%)") +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  fig_aes
dev.off()

#..Natives + BM only ####
png("figures/S_BM_natives.png",width=2500,height=1000,res=250)
plot_grid(b1,b2,
          nrow=1,ncol=2,
          labels=c("A","B"))
dev.off()

#..Natives + Simulations only ####
png("figures/Fig_native_FMC_flux.png",width=2500,height=1000,res=250)
plot_grid(d1,d2,
          nrow=1,ncol=2,
          labels=c("A","B"))
dev.off()
