# Create figures for climate/flux paper

# Load packages and set working directory
# setwd("WTF-Climate-Flux")
library(tidyverse)
library(lubridate)
library(ggh4x)
library(forcats)
library(zoo)
library(cowplot)

########## Load in data ##########
wthr_FMC <- read_csv("weather_flux/data/processed/weather_stations/wthr_1hr_FMC.csv")
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



########## Set color palettes and common aesthetics ##########
library(palettetown)

# Site palettes
p_site <- pokepal(254,5)
p_DRO <- p_site[1]
p_PNW <- p_site[4]
p_site_a <- c(p_site[1],p_site[3],p_site[2],p_site[5],p_site[4])
p_site_strip <- strip_themed(background_x = elem_list_rect(fill = p_site_a))


# Species palettes
# Set species level
sp.order <- c("ALSC","ARPE","CAAU","CASU","CLOB",
              "DYPA","MYGL","NONO","ROAN","SYSA",
              "EUCU","EULE","MEST","MEVI","PEBA","TEAR")
native_flux$Species.Code<-factor(native_flux$Species.Code, levels = sp.order)

p_pira <- "#700A1F"
p_DRO_sp <- pokepal(44,10)
p_PNW_sp <- pokepal(141,6)
p_DRO_sp_all <- c("#7090A0","#F8C050","#486878","#603000","#A07030","#D0A060",
                     "#E84800","#F87000",p_pira,"#103058","#805010")
p_PNW_sp_all <- c("#D8C088","#F8E0A8","#B09860","#686868","#685820",
                  p_pira,"#D8D8D0")
p_all_sp <- c(p_DRO_sp,p_PNW_sp)

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

#..Fig 3. Bayesian model from pine blocks ####
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

png("figures/Fig3_BM_pines.png",width=3000,height=2000,res=300)
bm + geom_point(pine_flux,mapping=aes(FMC,CO2_resp_rate),
                color=p_pira,shape=17,alpha=0.8) +
  guides(color=FALSE, fill=FALSE)
dev.off()



#..Fig 4. FMC stick calibration vs. block simulations ####
FMC_sim_p <- FMC_sim %>%
  select(-FMC_nor) %>%
  pivot_longer(!c(date,site),names_to="var",values_to="FMC") %>%
  mutate(Model = ifelse(var=="fuel_stick",
                            "Stick FMC","Block Moisture Content"))

png("figures/Fig4_FMC_sim_rain.png",width=3000,height=1900,res=300)
ggplot() + 
  geom_line(data=FMC_sim_p,mapping=aes(date,FMC,color=Model),
            alpha=0.5) +
  scale_color_manual(values=c(p_block,p_stick)) +
  geom_bar(data=filter(wthr_FMC, Rain_mm_Tot > 0),
           mapping=aes(date,Rain_mm_Tot),
           stat="identity",color="blue") +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block),
            alpha=0.5,color=p_block) +
  xlab("Date") + 
  scale_y_continuous(name="FMC (%)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  fig_aes +
  theme(legend.position = "top")
dev.off()



#..Fig 5. Time-resolved CO2 Flux ####
time_flux2 <- data.frame(site = wthr_FMC$site,
                         date = wthr_FMC$date,
                         Estimate = time_flux$Estimate,
                         Est.Error = time_flux$Est.Error,
                         Q2.5 = time_flux$Q2.5,
                         Q97.5 = time_flux$Q97.5)

png("figures/Fig5_CO2_time.png",width=3000,height=2000,res=300)
ggplot(time_flux2) +
  geom_line(mapping=aes(date,Estimate,color=site),alpha=0.5) +
  geom_ribbon(mapping=aes(x=date,y=Estimate,ymin=Q2.5,ymax=Q97.5,
                          fill=site),
              alpha=0.3,color=NA) +
  scale_color_manual(name="Site",values=p_site) +
  scale_fill_manual(name="Site",values=p_site) +
  xlab("Date") + ylab("CO2 Flux (ug CO2/s/g)") +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  fig_aes +
  guides(color=FALSE, fill=FALSE)
dev.off()



#..Natives and FMC/Flux Simulations ####
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
png("figures/Fig6_PNW_sim.png",width=1100,height=1100,res=320)
ggplot() +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.3,color=p_PNW) +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.85,size=2) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  #xlab("Simulated Block FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") +
  fig_aes +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
dev.off()

#.....Fig 6. Combined Plot ####
png("figures/Fig6_natives_BM_sim.png",width=3000,height=2300,res=300)
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



#..Flux vs. mass loss ####
ML_com_t <- pine_flux %>%
  filter(!is.na(pro.mass.loss)) %>%
  group_by(site,months,termite.attack) %>%
  summarize(mean_pro_ML = mean(pro.mass.loss),
            sd_pro_ML = sd(pro.mass.loss),
            n = n()) %>%
  mutate(n = as.numeric(n),
         se_pro_ML = sd_pro_ML/sqrt(n)) %>%
  right_join(int_mass_loss,by=c("site","months")) %>%
  mutate(mass_to_flux = (Carbon_por/mean_pro_ML)*100,
         mean_pro_ML = round(mean_pro_ML,2),
         sd_pro_ML = round(sd_pro_ML,2),
         Carbon_por = round(Carbon_por,2),
         termite.attack = ifelse(termite.attack==1,"Yes","No")) %>%
  filter(!is.na(termite.attack))

#....Fig 7. Pines ####
png("figures/Fig7_pine_mass_loss_comparison.png",
    width=3000,height=2000,res=300)
ggplot(ML_com_t,aes(mean_pro_ML,Carbon_por,
                    color=termite.attack,label=months)) +
  geom_abline(intercept=0, slope=1) +
  geom_point(size=2.2,alpha=0.95,shape=17) +
  geom_errorbarh(mapping=aes(xmin=mean_pro_ML-se_pro_ML,
                             xmax=mean_pro_ML+se_pro_ML,
                             color=termite.attack),
                 alpha=0.85) +
  #geom_text(nudge_y=0.08) +
  #scale_color_gradient(low="#a2b3ba",high="#2c3133") +
  scale_color_manual(name="Termite discovery",
                     values=c("gray30","salmon")) +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  xlab("Carbon Loss (g/g/hr)") +
  ylab("Carbon Flux (g/g/hr)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes +
  theme(legend.position = "top")
dev.off()



# Natives
ML_natives <- native_flux %>%
  filter(!is.na(pro.mass.loss)) %>%
  group_by(site,months,Species.Code,termite.attack) %>%
  summarize(mean_pro_ML = mean(pro.mass.loss,na.rm=TRUE),
            sd_pro_ML = sd(pro.mass.loss,na.rm=TRUE),
            n = n()) %>%
  mutate(n = as.numeric(n),
         se_pro_ML = sd_pro_ML/sqrt(n)) %>%
  left_join(int_mass_loss,by=c("site","months")) %>%
  mutate(mean_pro_ML = round(mean_pro_ML,2),
         sd_pro_ML = round(sd_pro_ML,2),
         Carbon_por = round(Carbon_por,2),
         termite.attack = ifelse(termite.attack==1,"Yes","No")) %>%
  filter(!is.na(termite.attack)) %>%
  mutate(months = as.character(months))

#....Fig 8. Natives ####
png("figures/Fig8_native_mass_loss_comparison.png",
    width=3000,height=1600,res=300)
ggplot(ML_natives,aes(mean_pro_ML,Carbon_por,
                      color=Species.Code, shape=termite.attack)) +
  geom_abline(intercept=0,slope=1) +
  geom_point(alpha=0.80,size=2.2) +
  geom_errorbarh(mapping=aes(xmin=mean_pro_ML-se_pro_ML,
                             xmax=mean_pro_ML+se_pro_ML,
                             color=Species.Code),
                 alpha=0.60) +
  scale_color_manual(values=p_all_sp,
                     name="Species") +
  scale_shape_manual(name="Termite discovery",
                     values=c(19,1)) +
  facet_wrap(~site) +
  xlab("Carbon Loss (g/g/hr)") +
  ylab("Carbon Flux (g/g/hr)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes
dev.off()



########## Supplementary Figures ##########

#......S2. FMC stick observations and calibration ####
FMC_m <- wthr_FMC %>%
  select(site,date,FMC_norm) %>%
  left_join(filter(FMC_sim_p,var=="fuel_stick"),
            by=c("site","date")) %>%
  select(-Model,-var) %>%
  pivot_longer(!c(date,site),names_to="Stick_FMC",values_to="FMC") %>%
  mutate(Stuck_FMC = ifelse(Stick_FMC=="FMC_norm",
                        "Measurements","Simulations"))

png("figures/S2_stick_FMC.png",width=3000,height=2000,res=300)
ggplot(FMC_m,aes(date,FMC,color=Stick_FMC)) +
  geom_line(alpha=0.5) +
  scale_color_manual(values=c(p_stick,p_WTF),
                     name="Stick FMC",
                     labels=c("Model Calibrations",
                              "Measurements (normalized)")) +
  xlab("Date") + ylab("Fuel moisture content (%)") +
  facet_wrap(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes +
  theme(legend.position = "top")
dev.off()



#......S3. Block simulations + field points ####
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

png("figures/S3_block_FMC.png",width=3000,height=2000,res=300)
ggplot() +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block),
            alpha=0.5,color=p_block) +
  geom_point(data=block_FMC,mapping=aes(date,FMC),
             color=p_pira,shape=17,size=1.8) +
  xlab("Date") + ylab("Fuel moisture content (%)") +
  facet_wrap(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  fig_aes
dev.off()


