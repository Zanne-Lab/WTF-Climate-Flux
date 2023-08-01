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
# Add site descriptions
wthr_FMC <- read_csv("weather_flux/data/processed/weather_stations/wthr_1hr_FMC.csv") %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna"))
pine_flux <- read_csv("weather_flux/data/processed/wood_respiration/pine_CO2_clean.csv") %>%
  filter(CO2_resp_rate > 0) %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna"))
native_flux <- read_csv("weather_flux/data/processed/wood_respiration/native_CO2_clean.csv") %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="PNW" ~ "Dry savanna"))
bm_fits_m <- read_csv("bayesian_model/bm_fits_m.csv") %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna"))
bm_fits_t <- read_csv("bayesian_model/bm_fits_t.csv") %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna"))
FMC_sim <- read_csv("FMC_mechanistic_model/fuel_moisture_output_t3.csv") %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna")) %>%
  filter(site!="HQ_AWC")
time_flux <- read_csv("bayesian_model/pred_model_3_2000.csv") %>%
  #separate(1,into=c("Estimate","Est.Error","Q2.5","Q97.5"),sep=",") %>%
  mutate_at(c("Estimate","Est.Error","Q2.5","Q97.5"),as.numeric) 
int_mass_loss <- read_csv("mass_loss_model_3.csv") %>%
  #separate(1,into=c("site","months","Carbon_por"),sep=",") %>%
  rename(months = Months,
         Carbon_por = V3) %>%
  mutate_at(c("site"),as.character) %>%
  mutate_at(c("months","Carbon_por"),as.numeric) %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna"))

# HQ_AWC is removed from plotting as it was not used in analysis

# Retrieve stick/block calibration data from Google
library(googledrive)
library(googlesheets4)
# Get sheet with times and pine block dry weight
FMC_cal <-drive_get("wtf_radiata_blocks_weight.csv") %>% 
  read_sheet(sheet = "calibrate_to_fuel_moisture_stick", col_names=T, 
             col_types=c("ccDnnn"))
detach(package:googledrive)
detach(package:googlesheets4)



########## Set color palettes and common aesthetics ##########
library(palettetown)

# Site palettes
p_site <- pokepal(254,5)
p_DRO <- p_site[1]
p_PNW <- p_site[4]
p_MLES <- p_site[2]


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
p_WTF <- "#303088"
p_POWER <- "#A84890"
p_CHRS <- "#680850"
p_SILO <- "#333333"
p_calc <- "#7870C8"

# Colors for simulation plots
p_stick <- "#7870C8"
p_block <- "#333333"

# Common aesthetics
fig_aes <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="gray95"),
        strip.text.x = element_text(size = 12))



########## Main text figures ##########

#..Fig 3. Model validation ####

#....Moisture content + rainfall ####
FMC_sim_m <- FMC_sim %>%
  select(-FMC_nor,-temp_stick,-temp_wood) %>%
  pivot_longer(!c(date,site,site_desc),names_to="var",values_to="FMC") %>%
  mutate(Model = case_when(var=="fuel_stick" ~ "Stick FMC",
                           var=="fuel_block" ~ "Block Moisture Content"))

# Reformat stick/block calibration data
FMC_cal2 <- FMC_cal %>%
  mutate(time = paste(time,"00",sep=":"),
         datetime = as_datetime(paste(date,time,sep=" ")),
         date = floor_date(datetime, "1 hour"),
         FMC = ((weight-dry_wt)/dry_wt)*100,
         site = case_when(site == "awc_hq" ~ "HQ_AWC", site == "dro" ~ "DRO",
                          site == "mtlwrf" ~ "MLRF", site == "mtlwsc" ~ "MLES",
                          site == "pnw" ~ "PNW", site == "stck" ~ "STCK")) %>%
  select(site,date,FMC) %>%
  mutate(site_desc = case_when(site=="DRO" ~ "Wet rainforest",
                               site=="MLRF" ~ "Dry rainforest",
                               site=="MLES" ~ "Sclerophyll",
                               site=="STCK" ~ "Wet savanna",
                               site=="PNW" ~ "Dry savanna"))

block_FMC <- pine_flux %>%
  mutate(date = as_datetime(paste(harvest_date,"12:00:00"))) %>%
  group_by(site,site_desc,date) %>%
  summarize(FMC = mean(FMC,na.rm=TRUE),
            sd_FMC = sd(FMC,na.rm=TRUE)) %>%
  rbind(FMC_cal2) %>%
  filter(site!="HQ_AWC")

sim_rain <- ggplot() + 
  geom_bar(data=filter(wthr_FMC, Rain_mm_Tot > 0),
           mapping=aes(date,Rain_mm_Tot),
           stat="identity",color="blue") +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block,color="Simulations"),
            alpha=0.7) +
  geom_point(data=block_FMC,mapping=aes(date,FMC,color="Measurements"),
             shape=17,size=1.8) +
  scale_color_manual(values=c("Simulations" = p_block,
                              "Measurements"=p_pira)) +
  xlab("Date") + 
  scale_y_continuous(name="Moisture content (%)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
             nrow=1) +
  fig_aes +
  theme(legend.position = "top",
        legend.title = element_blank())

#....Wood temp + surface air temp ####
FMC_sim_t <- FMC_sim %>%
  select(-FMC_nor,-fuel_stick,-fuel_block) %>%
  pivot_longer(!c(date,site,site_desc),names_to="var",values_to="wood_temp") %>%
  mutate(Model = case_when(var=="temp_stick" ~ "Stick Temperature",
                           var=="temp_wood" ~ "Block Temperature"),
         wood_temp = wood_temp-273.15) %>%
  filter(var=="temp_wood") %>%
  select(-var,-Model) %>%
  left_join(select(wthr_FMC,site,date,ib_AirTC_Avg),
            by=c("site","date")) %>%
  pivot_longer(!c(date,site,site_desc),names_to="var",values_to="temp") %>%
  mutate(Model = case_when(var=="wood_temp" ~ "Simulations",
                           var=="ib_AirTC_Avg" ~ "iButton"))

sim_temp <- ggplot() + 
  geom_line(data=FMC_sim_t,mapping=aes(date,temp,color=Model),
            alpha=0.5) +
  scale_color_manual(values=c("red",p_block)) +
  xlab("Date") +
  scale_y_continuous(name="Wood temperature (°C)",
                     sec.axis=sec_axis(~.,name="Air temperature (°C)")) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
             nrow=1) +
  fig_aes +
  theme(legend.position = "top",
        legend.title = element_blank())

#....Final figure ####
pdf("figures/Fig3_microclimate_sim.pdf",width=12,height=7)
plot_grid(sim_rain,sim_temp,
          nrow=2,ncol=1,
          labels = c("A","B"),
          label_size = 16)
dev.off()



#..Fig 4. Bayesian model from pine blocks ####

#....Moisture content + flux ####
bm_ma <- ggplot() + 
  geom_smooth(bm_fits_m,mapping=aes(effect1__,estimate__,color=site)) + 
  scale_color_manual(name="Site",values=p_site) +
  geom_ribbon(bm_fits_m,mapping=aes(x=effect1__,y=estimate__,
                                  ymin=lower__,ymax=upper__,fill=site),
              alpha=0.2,color=NA) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
             nrow=1) +
  scale_fill_manual(name="Site",values=p_site) +
  fig_aes + 
  geom_point(pine_flux,mapping=aes(FMC,CO2_resp_rate),
                color=p_pira,shape=17,alpha=0.8) +
  xlab("Moisture Content (%)") + 
  ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  guides(color=FALSE, fill=FALSE)

#....Temp + flux ####
bm_ta <- ggplot() + 
  geom_smooth(bm_fits_t,mapping=aes(effect1__,estimate__,color=site)) + 
  scale_color_manual(name="Site",values=p_site) +
  geom_ribbon(bm_fits_t,mapping=aes(x=effect1__,y=estimate__,
                                    ymin=lower__,ymax=upper__,fill=site),
              alpha=0.2,color=NA) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),nrow=1) +
  scale_fill_manual(name="Site",values=p_site) +
  fig_aes + 
  geom_point(pine_flux,mapping=aes(mean_Tcham,CO2_resp_rate),
             color=p_pira,shape=17,alpha=0.8) +
  xlab("Chamber Temperature (°C)") + 
  ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  guides(color=FALSE, fill=FALSE)

pdf("figures/Fig4_BM.pdf",width=12,height=6)
plot_grid(bm_ma,bm_ta,
          nrow=2,ncol=1,
          labels=c("A","B"),
          label_size = 16)
dev.off()



#..Fig 5. Time-resolved CO2 Flux ####
time_flux2 <- data.frame(site = wthr_FMC$site,
                         site_desc = wthr_FMC$site_desc,
                         date = wthr_FMC$date,
                         Estimate = time_flux$Estimate,
                         Est.Error = time_flux$Est.Error,
                         Q2.5 = time_flux$Q2.5,
                         Q97.5 = time_flux$Q97.5)


#....No uncertainty ####
pdf("figures/Fig5_CO2_time.pdf",width=9,height=6)
ggplot(time_flux2) +
  geom_line(mapping=aes(date,Estimate,color=site),alpha=0.5) +
  scale_color_manual(name="Site",values=p_site) +
  scale_fill_manual(name="Site",values=p_site) +
  xlab("Date") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna")) +
  fig_aes +
  guides(color=FALSE, fill=FALSE)
dev.off()

#....S4: with uncertainty ####
pdf("figures/S4_CO2_time_un.pdf",width=9,height=6)
ggplot(time_flux2) +
  geom_line(mapping=aes(date,Estimate,color=site),alpha=0.5) +
  geom_ribbon(mapping=aes(x=date,y=Estimate,ymin=Q2.5,ymax=Q97.5,
                          fill=site),
              alpha=0.3,color=NA) +
  scale_color_manual(name="Site",values=p_site) +
  scale_fill_manual(name="Site",values=p_site) +
  xlab("Date") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna")) +
  fig_aes +
  guides(color=FALSE, fill=FALSE)
dev.off()



#..Fig 6. Natives and FMC/Flux Simulations ####
#### NEEDS EDITING ####
sim_flux <- data.frame(site = FMC_sim$site,
                       site_desc = FMC_sim$site_desc,
                       Block_FMC = FMC_sim$fuel_block,
                       Sim_CO2 = time_flux$Estimate,
                       Sim_Q2.5 = time_flux$Q2.5,
                       Sim_Q97.5 = time_flux$Q97.5)

# DRO + BM
b1 <- ggplot() + 
  geom_smooth(filter(bm_fits_m,site=="DRO"),
              mapping=aes(effect1__,estimate__),color=p_DRO) + 
  geom_ribbon(filter(bm_fits_m,site=="DRO"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.3,color=NA,fill=p_DRO) +
  geom_point(filter(native_flux,site=="DRO"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_DRO_sp_all) +
  xlab("Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

# PNW + BM
b2 <- ggplot() + 
  geom_smooth(filter(bm_fits_m,site=="PNW"),
              mapping=aes(effect1__,estimate__),color=p_PNW) + 
  geom_ribbon(filter(bm_fits_m,site=="PNW"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,
                         color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_PNW_sp_all) +
  xlab("Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
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
  geom_point(data=filter(sim_flux,site=="DRO"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.01,color=p_DRO) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Simulated Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
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
  xlab("Simulated Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
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
  #xlab("Simulated Block FMC (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  fig_aes +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
dev.off()

#....Final figure ####
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







#..Fig 7. Pine flux vs. mass loss ####
ML_com_t <- pine_flux %>%
  filter(!is.na(pro.mass.loss)) %>%
  group_by(site,site_desc,months,termite.attack) %>%
  summarize(mean_pro_ML = mean(pro.mass.loss),
            sd_pro_ML = sd(pro.mass.loss),
            n = n()) %>%
  mutate(n = as.numeric(n),
         se_pro_ML = sd_pro_ML/sqrt(n)) %>%
  right_join(int_mass_loss,by=c("site","site_desc","months")) %>%
  mutate(mass_to_flux = (Carbon_por/mean_pro_ML)*100,
         mean_pro_ML = round(mean_pro_ML,2),
         sd_pro_ML = round(sd_pro_ML,2),
         Carbon_por = round(Carbon_por,2),
         termite.attack = ifelse(termite.attack==1,"Yes","No")) %>%
  filter(!is.na(termite.attack))

pdf("figures/Fig7_pine_mass_loss_comparison.pdf",
    width=9,height=6.5)
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
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna")) +
  xlab("Carbon Loss (g C/g C)") +
  ylab("Carbon Flux (g C/g C)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes +
  theme(legend.position = "top")
dev.off()



#..Fig 8. Natives flux vs. mass loss ####
ML_natives <- native_flux %>%
  filter(!is.na(pro.mass.loss)) %>%
  group_by(site,site_desc,months,Species.Code,termite.attack) %>%
  summarize(mean_pro_ML = mean(pro.mass.loss,na.rm=TRUE),
            sd_pro_ML = sd(pro.mass.loss,na.rm=TRUE),
            n = n()) %>%
  mutate(n = as.numeric(n),
         se_pro_ML = sd_pro_ML/sqrt(n)) %>%
  left_join(int_mass_loss,by=c("site","site_desc","months")) %>%
  mutate(mean_pro_ML = round(mean_pro_ML,2),
         sd_pro_ML = round(sd_pro_ML,2),
         Carbon_por = round(Carbon_por,2),
         termite.attack = ifelse(termite.attack==1,"Yes","No")) %>%
  filter(!is.na(termite.attack)) %>%
  mutate(months = as.character(months))

pdf("figures/Fig8_native_mass_loss_comparison.pdf",
    width=10,height=6)
ggplot(ML_natives,aes(mean_pro_ML,Carbon_por,
                      color=Species.Code, shape=termite.attack)) +
  geom_abline(intercept=0,slope=1) +
  geom_jitter(alpha=0.80,size=2.2) +
  geom_errorbarh(mapping=aes(xmin=mean_pro_ML-se_pro_ML,
                             xmax=mean_pro_ML+se_pro_ML,
                             color=Species.Code),
                 alpha=0.60) +
  scale_color_manual(values=p_all_sp,
                     name="Species") +
  scale_shape_manual(name="Termite discovery",
                     values=c(16,9)) +
  facet_wrap(~fct_relevel(site_desc,"Wet rainforest","Dry savanna")) +
  xlab("Carbon Loss (g C/g C)") +
  ylab("Carbon Flux (g C/g C)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes
dev.off()




########## Supplementary Figures ##########

#..S1. Flux sample cleaning ####
check <- read_csv("weather_flux/data/processed/wood_respiration/cleaning_stats.csv")
IRGA_ex <- read_csv("weather_flux/data/processed/wood_respiration/IRGA_ex.csv") %>%
  mutate(name = ifelse(SampleID=="523","Sample 523: Nonsignificant fit",
                       "Sample 524: Kept"))

# Proportions plot
pdf("figures/S1_flux_cleaning.pdf",width=3.5,height=4)
ggplot(check, aes(fill=Data, y=Proportion, x=Dataset)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name="Sample type",
                    values=pokepal(9,3),
                    labels=c("Low % wood","Nonsignificant fit","Kept")) +
  fig_aes
dev.off()

# Example measurement plot
pdf("figures/S1_flux_cleaning2.pdf",width=8.5,height=4)
ggplot(IRGA_ex,aes(Etime,CO2d_ppm,color=CO2_resp_outlier)) +
  geom_point(alpha=0.9,size=1.5) +
  scale_color_manual(values=c(pokepal(9,3)[3],pokepal(9,3)[2])) +
  facet_wrap(~name, scales="free") +
  xlab("Time (seconds)") + ylab("CO2 (ppm)") +
  fig_aes +
  theme(legend.position = "none")
dev.off()



#..S3. Natives vs models temp ####
#### NEEDS EDITING ####
sim_flux <- data.frame(site = FMC_sim$site,
                       site_desc = FMC_sim$site_desc,
                       Block_FMC = FMC_sim$fuel_block,
                       Block_temp = FMC_sim$temp_wood-273.15,
                       Sim_CO2 = time_flux$Estimate,
                       Sim_Q2.5 = time_flux$Q2.5,
                       Sim_Q97.5 = time_flux$Q97.5)

#...DRO temp ####
# BM
bt1 <- ggplot() + 
  geom_smooth(filter(bm_fits_t,site=="DRO"),
              mapping=aes(effect1__,estimate__),color=p_DRO) + 
  geom_ribbon(filter(bm_fits_t,site=="DRO"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.3,color=NA,fill=p_DRO) +
  geom_point(filter(native_flux,site=="DRO"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_DRO_sp_all) +
  xlab("Chamber Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes

# Simulations
dt1 <- ggplot() +
  geom_point(data=filter(sim_flux,site=="DRO"),
             mapping=aes(Block_temp,Sim_CO2),
             alpha=0.3,color=p_DRO) +
  geom_ribbon(data=filter(sim_flux,site=="DRO"),
              mapping=aes(x=Block_temp,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.3,color=NA,fill=p_DRO) +
  geom_point(data=filter(sim_flux,site=="DRO",Block_FMC>100),
             mapping=aes(Block_temp,Sim_CO2),
             alpha=0.3,color="darkgreen") +
  geom_point(data=filter(native_flux,site=="DRO"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Simulated Wood Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes


#...PNW temp ####
# BM
bt2 <- ggplot() + 
  geom_smooth(filter(bm_fits_t,site=="PNW"),
              mapping=aes(effect1__,estimate__),color=p_PNW) + 
  geom_ribbon(filter(bm_fits_t,site=="PNW"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(filter(native_flux,site=="PNW"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_PNW_sp_all) +
  xlab("Chamber Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes

# Simulations
dt2 <- ggplot() +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_temp,Sim_CO2),
             alpha=0.3,color=p_PNW) +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_temp,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.3,color=NA,fill=p_PNW) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.8) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  xlab("Simulated Wood Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes

png("figures/Fig6_natives_BM_sim_t.png",width=3000,height=2300,res=300)
plot_grid(bt1 + theme(legend.position="none",
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank()),
          dt1 + theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank()),
          bt2 + theme(legend.position="none"),
          dt2 + theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.y = element_blank()),
          nrow=2,ncol=2,
          labels=c("A","B","C","D"),
          align="hv",
          axis="tblr")
dev.off()


