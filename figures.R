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
bm_fits_mt <- read_csv("bayesian_model/bm_fits_mt.csv") %>%
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
int_mass_loss <- read_csv("bayesian_model/mass_loss_model_3.csv") %>%
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
library(colorspace)

# Site palettes
p_site <- c("#4b3f83","#75b493","#627e8c","#fcf0b8","#b9d2a6")
p_site_d <- darken(p_site,amount=0.9)
p_site_a <- c("#4b3f83","#627e8c","#75b493","#b9d2a6","#fcf0b8")
p_DRO <- p_site[1]
p_PNW <- p_site[4]
site_str <- strip_themed(background_x = elem_list_rect(fill = adjustcolor(p_site_a,alpha.f=0.4)))


# Species palettes
# Set species level
sp.order <- c("ALSC","ARPE","CAAU","CASU","CLOB",
              "DYPA","MYGL","NONO","ROAN","SYSA",
              "EUCU","EULE","MEST","MEVI","PEBA","TEAR")
native_flux$Species.Code<-factor(native_flux$Species.Code, levels = sp.order)

p_pira <- "#700A1F"
p_DRO_sp <- pokepal(1,10)
p_PNW_sp <- pokepal(44,10)[1:6]

# Colors for simulation plots
p_stick <- "#7870C8"
p_block <- "#333333"

# Common aesthetics
fig_aes <- theme_bw() +
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="gray95"),
        strip.text.x = element_text(size = 14, face="bold"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))



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

block_MC <- pine_flux %>%
  mutate(date = as_datetime(paste(harvest_date,"12:00:00"))) %>%
  group_by(site,site_desc,date) %>%
  rename(FMC_block = FMC) %>%
  summarize(FMC = mean(FMC_block,na.rm=TRUE),
            sd_FMC = sd(FMC_block,na.rm=TRUE),
            n = n()) %>%
  mutate(n = as.numeric(n),
         se_FMC = sd_FMC/sqrt(n)) %>%
  rbind(FMC_cal2) %>%
  filter(site!="HQ_AWC")

sim_rain <- ggplot() + 
  geom_bar(data=filter(wthr_FMC, Rain_mm_Tot > 0),
           mapping=aes(date,Rain_mm_Tot),
           stat="identity",color="blue") +
  geom_line(data=FMC_sim,mapping=aes(date,fuel_block,color="Simulations"),
            alpha=0.7) +
  geom_errorbar(data=block_MC,mapping=aes(x=date,
                                          ymin=FMC-se_FMC,
                                          ymax=FMC+se_FMC),
                color=p_pira,alpha=0.75) +
  geom_point(data=block_MC,mapping=aes(date,FMC,color="Measurements"),
             shape=17,size=2) +
  scale_color_manual(values=c("Simulations" = p_block,
                              "Measurements"=p_pira)) +
  xlab("Date") + 
  scale_y_continuous(name="Moisture content (%)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
             nrow=1,strip=site_str) +
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
                           var=="ib_AirTC_Avg" ~ "Air temperature"))

block_T <- pine_flux %>%
  mutate(date = as_datetime(paste(harvest_date,"12:00:00"))) %>%
  group_by(site,site_desc,date) %>%
  summarize(Tcham = mean(mean_Tcham,na.rm=TRUE),
            sd_Tcham = sd(mean_Tcham,na.rm=TRUE),
            n = n()) %>%
  mutate(n = as.numeric(n),
         se_Tcham = sd_Tcham/sqrt(n))

sim_temp <- ggplot() + 
  geom_line(data=FMC_sim_t,mapping=aes(date,temp,color=Model),
            alpha=0.5) +
  geom_point(data=block_T,mapping=aes(date,Tcham,fill="Measurements"),
             shape=24,size=2,color=p_pira,stroke=0.3) +
  geom_errorbar(data=block_T,mapping=aes(x=date,
                                         ymin=Tcham-se_Tcham,
                                         ymax=Tcham+se_Tcham),
                color=p_pira,alpha=0.75) +
  scale_color_manual(values=c("red",p_block)) +
  scale_fill_manual(values="gray90") +
  xlab("Date") +
  scale_y_continuous(name="Wood temperature (°C)",
                     sec.axis=sec_axis(~.,name="Air temperature (°C)")) +
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
             nrow=1,strip=site_str) +
  fig_aes +
  theme(legend.position = "top",
        legend.title = element_blank())

#....Final figure ####
fig3 <- plot_grid(sim_rain,sim_temp,
                  nrow=2,ncol=1,
                  labels = c("A","B"),
                  label_size = 16)

pdf("figures/R_figs/Fig3.pdf",width=12,height=6.75)
print(fig3)
dev.off()



#..Fig 4. Bayesian model from pine blocks ####

#....Moisture content + flux ####
bm_ma <- ggplot() + 
  geom_ribbon(bm_fits_m,mapping=aes(x=effect1__,y=estimate__,
                                  ymin=lower__,ymax=upper__),
              alpha=0.4,fill="gray",color=NA) +
  geom_smooth(bm_fits_m,mapping=aes(effect1__,estimate__),
              color=p_block) + 
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
             nrow=1,strip=site_str) +
  geom_point(pine_flux,mapping=aes(FMC,CO2_resp_rate,color="Measurements"),
                shape=17,alpha=0.8) +
  scale_color_manual(values=c("Measurements"=p_pira)) +
  xlab("Moisture Content (%)") + 
  ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  ylim(0,0.161) +
  fig_aes + 
  theme(legend.position = "top",
        legend.title = element_blank())

#....S3: Temp + flux ####
bm_ta <- ggplot() + 
  geom_ribbon(bm_fits_t,mapping=aes(x=effect1__,y=estimate__,
                                    ymin=lower__,ymax=upper__),
              alpha=0.4,fill="gray",color=NA) +
  geom_smooth(bm_fits_t,mapping=aes(effect1__,estimate__),
              color=p_block) + 
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                          "Dry rainforest","Sclerophyll",
                          "Wet savanna","Dry savanna"),
              nrow=1,strip=site_str) +
  geom_point(pine_flux,mapping=aes(mean_Tcham,CO2_resp_rate,color="Measurements"),
             shape=17,alpha=0.8) +
  scale_color_manual(values=c("Measurements"=p_pira)) +
  xlab("Temperature (°C)") + 
  ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  ylim(0,0.161) +
  fig_aes + 
  theme(legend.position = "top",
        legend.title = element_blank())

png("figures/PNG/S3_BM_temp.png",width=3500,height=984,res=300)
print(bm_ta)
dev.off()

pdf("figures/S3_BM_temp.pdf",width=12,height=3.375)
print(bm_ta)
dev.off()



#....Interaction + flux ####
p_temp <- c("#f13e3e","#9d2627","#501111")
bm_fits_mt$effect2__ <- as.factor(round(bm_fits_mt$effect2__,0))
bm_int <- ggplot() +
  #geom_ribbon(bm_fits_mt,mapping=aes(x=effect1__,y=estimate__,ymin=lower__,ymax=upper__,fill=effect2__),alpha=0.4,color=NA) +
  geom_point(data=bm_fits_mt,
             mapping=aes(x=effect1__,y=estimate__,
                         color=effect2__),
             alpha=0.8,size=1.5) +
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                           "Dry rainforest","Sclerophyll",
                           "Wet savanna","Dry savanna"),
              nrow=1,strip=site_str) +
  scale_color_manual(name="Ambient Temperature (°C)",
                         values=p_temp) +
  #scale_fill_manual(values=p_temp) +
  xlab("Moisture Content (%)") + 
  ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  fig_aes +
  theme(legend.position = "top") +
  guides(fill=FALSE)

#....Final figure ####
fig4 <- plot_grid(bm_ma,bm_int,
                  nrow=2,ncol=1,
                  labels=c("A","B"),
                  label_size = 16)

png("figures/PNG/Fig4_BM.png",width=3500,height=1969,res=300)
print(fig4)
dev.off()

pdf("figures/Fig4_BM.pdf",width=12,height=6.75)
print(fig4)
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
fig5 <- ggplot(time_flux2) +
  geom_line(mapping=aes(date,Estimate),color=p_block,alpha=0.6) +
  xlab("Date") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                           "Dry rainforest","Sclerophyll",
                           "Wet savanna","Dry savanna"),
              nrow=1,strip=site_str) +
  fig_aes +
  guides(color=FALSE, fill=FALSE)

png("figures/PNG/Fig5_CO2_time.png",width=3500,height=831,res=300)
print(fig5)
dev.off()

pdf("figures/Fig5_CO2_time.pdf",width=12,height=2.85)
print(fig5)
dev.off()



#....S4: with uncertainty ####
s4 <- ggplot(time_flux2) +
  geom_ribbon(mapping=aes(x=date,y=Estimate,ymin=Q2.5,ymax=Q97.5),
              alpha=0.4,fill="gray",color=NA) +
  geom_line(mapping=aes(date,Estimate),color=p_block,alpha=0.6) +
  scale_fill_manual(name="Site",values=p_site) +
  xlab("Date") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                           "Dry rainforest","Sclerophyll",
                           "Wet savanna","Dry savanna"),
              nrow=1,strip=site_str) +
  fig_aes +
  guides(color=FALSE, fill=FALSE)

png("figures/PNG/S4_CO2_time_un.png",width=3500,height=831,res=300)
print(s4)
dev.off()

pdf("figures/S4_CO2_time_un.pdf",width=12,height=2.85)
print(s4)
dev.off()



#..Fig 6. Natives and FMC/Flux Simulations ####
sim_flux <- data.frame(site = FMC_sim$site,
                       site_desc = FMC_sim$site_desc,
                       Block_FMC = FMC_sim$fuel_block,
                       Sim_CO2 = time_flux$Estimate,
                       Sim_Q2.5 = time_flux$Q2.5,
                       Sim_Q97.5 = time_flux$Q97.5)

# DRO + BM
b1 <- ggplot() + 
  geom_ribbon(filter(bm_fits_m,site=="DRO"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.4,color=NA,fill=p_DRO) +
  geom_smooth(filter(bm_fits_m,site=="DRO"),
              mapping=aes(effect1__,estimate__),color=p_block) + 
  geom_point(filter(native_flux,site=="DRO"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

# DRO + simulations 
d1 <- ggplot() +
  geom_point(data=filter(sim_flux,site=="DRO"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.4,color=darken(p_DRO,0.1)) +
  geom_ribbon(data=filter(sim_flux,site=="DRO"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.4,color=NA,fill=p_DRO) +
  geom_point(data=filter(native_flux,site=="DRO"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Simulated Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

# PNW + BM
b2 <- ggplot() + 
  geom_ribbon(filter(bm_fits_m,site=="PNW"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.4,color=NA,fill=p_PNW) +
  geom_smooth(filter(bm_fits_m,site=="PNW"),
              mapping=aes(effect1__,estimate__),color=p_block) + 
  geom_point(filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,
                         color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  xlab("Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

# PNW + simulations
d2 <- ggplot() +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.4,color=NA,fill=p_PNW) +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.4,color=darken(p_PNW,0.1)) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  xlab("Simulated Moisture Content (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  xlim(0,650) + ylim(-0.001,0.125) +
  fig_aes

#....Final figures ####
pdf("figures/R_figs/Fig6_A.pdf",width=5.5,height=4.2)
plot_grid(b1,labels="A",label_size = 16)
dev.off()

pdf("figures/R_figs/Fig6_B.pdf",width=5.5,height=4.2)
plot_grid(d1,labels="B",label_size = 16)
dev.off()

pdf("figures/R_figs/Fig6_C.pdf",width=5.5,height=4.2)
plot_grid(b2,labels="C",label_size = 16)
dev.off()

pdf("figures/R_figs/Fig6_D.pdf",width=5.5,height=4.2)
plot_grid(d2,labels="D",label_size = 16)
dev.off()


# PNW + simulations without scale
pdf("figures/R_figs/Fig6_D_zoom.pdf",width=4,height=3.8)
ggplot() +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_FMC,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.4,color=NA,fill=p_PNW) +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_FMC,Sim_CO2),
             alpha=0.4,color=darken(p_PNW,0.1)) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(FMC,CO2_resp_rate,color=Species.Code),
             alpha=0.9,size=2) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  #xlab("Simulated Block FMC (%)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  fig_aes +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
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

fig7 <- ggplot(filter(ML_com_t,termite.attack=="No"),
               aes(x=mean_pro_ML,y=Carbon_por,color="No")) +
  geom_abline(intercept=0, slope=1) +
  stat_smooth(method = "lm", se=FALSE, linetype="dashed",
              linewidth=0.5, color="gray5") +
  stat_regline_equation(aes(label=paste(after_stat(adj.rr.label))),
                        show.legend=FALSE,label.y=0.95,color="gray5") +
  stat_smooth(data=ML_com_t,
              mapping=aes(x=mean_pro_ML,y=Carbon_por),
              method = "lm", se=FALSE, linetype="dashed",
              linewidth=0.5, color="brown3") +
  stat_regline_equation(data=ML_com_t,
                        mapping=aes(x=mean_pro_ML,y=Carbon_por,
                                    label=paste(after_stat(adj.rr.label))),
                        show.legend=FALSE,label.y=0.8,color="brown3") +
  geom_point(size=2.2,alpha=0.95,shape=17) +
  geom_point(filter(ML_com_t,termite.attack=="Yes"),
             mapping=aes(x=mean_pro_ML,y=Carbon_por,color="Yes"),
             size=2.2,alpha=0.95,shape=17) +
  geom_errorbarh(ML_com_t,mapping=aes(xmin=mean_pro_ML-se_pro_ML,
                                      xmax=mean_pro_ML+se_pro_ML,
                                      color=termite.attack),
                 alpha=0.85) +
  scale_color_manual(name="Termite discovery",
                     values=c("No"="gray5","Yes"="brown3")) +
  facet_wrap2(~fct_relevel(site_desc,"Wet rainforest",
                           "Dry rainforest","Sclerophyll",
                           "Wet savanna","Dry savanna"),
              nrow=1,strip=site_str) +
  xlab("Carbon Loss (g C/g C)") +
  ylab("Carbon Flux (g C/g C)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes +
  theme(legend.position = "top")

png("figures/PNG/Fig7_pine_mass_loss_comparison.png",
    width=3500,height=984,res=300)
print(fig7)
dev.off()

pdf("figures/Fig7_pine_mass_loss_comparison.pdf",
    width=12,height=3.375)
print(fig7)
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
  mutate(months = as.character(months),
         title = paste0(site_desc,": ",Species.Code))

# Aesthetics for coloring facet strip
p_native_site <- c(rep(adjustcolor(p_DRO,alpha.f = 0.4),10),
                   rep(adjustcolor(p_PNW,alpha.f = 0.4),6))
native_str <- strip_themed(background_x = elem_list_rect(fill = p_native_site))

ML_sp_order <- unique(arrange(ML_natives,site,Species.Code)$title)
ML_natives$title<-factor(ML_natives$title, levels = ML_sp_order)

fig8 <- ggplot(filter(ML_natives,termite.attack=="No"),
               aes(mean_pro_ML,Carbon_por,
                   color="No")) +
  geom_abline(intercept=0,slope=1) +
  stat_smooth(method = "lm", se=FALSE, linetype="dashed",
              linewidth=0.5) +
  stat_regline_equation(aes(label=paste(after_stat(adj.rr.label))),
                        show.legend=FALSE,color="gray5",
                        label.y=0.95) +
  stat_smooth(data=ML_natives,
              mapping=aes(x=mean_pro_ML,y=Carbon_por),
              method = "lm", se=FALSE, linetype="dashed",
              linewidth=0.5, color="brown3") +
  stat_regline_equation(data=ML_natives,
                        mapping=aes(x=mean_pro_ML,y=Carbon_por,
                                    label=paste(after_stat(adj.rr.label))),
                        show.legend=FALSE,label.y=0.8,color="brown3") +
  geom_point(alpha=0.80,size=2.2) +
  geom_errorbarh(mapping=aes(xmin=mean_pro_ML-se_pro_ML,
                             xmax=mean_pro_ML+se_pro_ML,
                             color=termite.attack),
                 alpha=0.60) +
  geom_point(filter(ML_natives,termite.attack=="Yes"),
             mapping=aes(mean_pro_ML,Carbon_por,
                         color="Yes")) +
  scale_color_manual(name="Termite discovery",
                     values=c("No"="gray5","Yes"="brown3")) +
  facet_wrap2(~title,
              strip = native_str) +
  xlab("Carbon Loss (g C/g C)") +
  ylab("Carbon Flux (g C/g C)") +
  xlim(0,1) + ylim(0,1) +
  fig_aes +
  theme(legend.position = "top")

png("figures/PNG/Fig8_native_mass_loss_comparison.png",
    width=3000,height=2864,res=300)
print(fig8)
dev.off()

pdf("figures/Fig8_native_mass_loss_comparison.pdf",
    width=11,height=10.5)
print(fig8)
dev.off()




########## Supplementary Figures ##########

#..S1. Flux sample cleaning ####
check <- read_csv("weather_flux/data/processed/wood_respiration/cleaning_stats.csv")
IRGA_ex <- read_csv("weather_flux/data/processed/wood_respiration/IRGA_ex.csv") %>%
  mutate(name = ifelse(SampleID=="523","Sample 523: Nonsignificant fit",
                       "Sample 524: Kept"))

# Proportions plot
S1_A <- ggplot(check, aes(fill=Data, y=Proportion, x=Dataset)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name="Sample type",
                    values=pokepal(9,3),
                    labels=c("Low % wood","Nonsignificant fit","Kept")) +
  fig_aes

pdf("figures/R_figs/S1_A.pdf",width=3.5,height=4)
plot_grid(S1_A,labels = "A",label_size=16)
dev.off()

# Example measurement plot
S1_B <- ggplot(IRGA_ex,aes(Etime,CO2d_ppm,color=CO2_resp_outlier)) +
  geom_point(alpha=0.9,size=1.5) +
  scale_color_manual(values=c(pokepal(9,3)[3],pokepal(9,3)[2])) +
  facet_wrap(~name, scales="free") +
  xlab("Time (seconds)") + ylab("CO2 (ppm)") +
  fig_aes +
  theme(legend.position = "none")

pdf("figures/R_figs/S1_B.pdf",width=8.5,height=4)
plot_grid(S1_B,labels = "B",label_size=16)
dev.off()



#..S5. Natives vs models temp ####
sim_flux <- data.frame(site = FMC_sim$site,
                       site_desc = FMC_sim$site_desc,
                       Block_FMC = FMC_sim$fuel_block,
                       Block_temp = FMC_sim$temp_wood-273.15,
                       Sim_CO2 = time_flux$Estimate,
                       Sim_Q2.5 = time_flux$Q2.5,
                       Sim_Q97.5 = time_flux$Q97.5)

# DRO BM
bt1 <- ggplot() + 
  geom_ribbon(filter(bm_fits_t,site=="DRO"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.4,color=NA,fill=p_DRO) +
  geom_smooth(filter(bm_fits_t,site=="DRO"),
              mapping=aes(effect1__,estimate__),color=p_block) + 
  geom_point(filter(native_flux,site=="DRO"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes 

# DRO sim
dt1 <- ggplot() +
  geom_ribbon(data=filter(sim_flux,site=="DRO"),
              mapping=aes(x=Block_temp,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.4,color=NA,fill=p_DRO) +
  geom_point(data=filter(sim_flux,site=="DRO"),
             mapping=aes(Block_temp,Sim_CO2),
             alpha=0.4,color=p_DRO) +
  geom_point(data=filter(native_flux,site=="DRO"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_DRO_sp) +
  xlab("Simulated Wood Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes 

# PNW BM
bt2 <- ggplot() + 
  geom_ribbon(filter(bm_fits_t,site=="PNW"),
              mapping=aes(x=effect1__,y=estimate__,
                          ymin=lower__,ymax=upper__),
              alpha=0.4,color=NA,fill=p_PNW) +
  geom_smooth(filter(bm_fits_t,site=="PNW"),
              mapping=aes(effect1__,estimate__),color=p_block) + 
  geom_point(filter(native_flux,site=="PNW"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  xlab("Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) + 
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes

# PNW Sim
dt2 <- ggplot() +
  geom_ribbon(data=filter(sim_flux,site=="PNW"),
              mapping=aes(x=Block_temp,y=Sim_CO2,ymin=Sim_Q2.5,ymax=Sim_Q97.5),
              alpha=0.4,color=NA,fill=p_PNW) +
  geom_point(data=filter(sim_flux,site=="PNW"),
             mapping=aes(Block_temp,Sim_CO2),
             alpha=0.4,color=darken(p_PNW,0.1)) +
  geom_point(data=filter(native_flux,site=="PNW"),
             mapping=aes(mean_Tcham,CO2_resp_rate,color=Species.Code),
             alpha=0.9) +
  scale_color_manual(name="Species",values=p_PNW_sp) +
  xlab("Simulated Wood Temperature (°C)") + ylab(bquote(~CO[2]~ 'Flux('*mu~'g'~CO[2]~ s^-1~g^-1*')')) +
  xlim(0,70) + ylim(-0.001,0.125) +
  fig_aes 

#....Final figures ####

pdf("figures/R_figs/S5_A.pdf",width=5.5,height=4.2)
plot_grid(bt1,labels="A",label_size=16)
dev.off()

pdf("figures/R_figs/S5_B.pdf",width=5.5,height=4.2)
plot_grid(dt1,labels="B",label_size=16)
dev.off()

pdf("figures/R_figs/S5_C.pdf",width=5.5,height=4.2)
plot_grid(bt2,labels="C",label_size=16)
dev.off()

pdf("figures/R_figs/S5_D.pdf",width=5.5,height=4.2)
plot_grid(dt2,labels="D",label_size=16)
dev.off()


#..Table S4: C loss model ####
ML_r <- ML_com_t %>%
  rename('Termite Discovery' = termite.attack,
         'Carbon Flux' = Carbon_por,
         'Carbon Loss' = mean_pro_ML,
         Site = site_desc)

ML_f <- lm(formula = `Carbon Flux` ~ `Carbon Loss` * Site * `Termite Discovery`, 
           data = ML_r)
summary(ML_f)

library(gtsummary)
tbl_regression(ML_f) %>%
  bold_p() %>%
  as_kable_extra(escape=FALSE,format="latex") %>%
  cat()

