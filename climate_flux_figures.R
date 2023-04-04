# Create figures for climate/flux paper

# Load packages
library(tidyverse)
library(lubridate)
library(ggh4x)
library(forcats)
library(zoo)

########## Load in data ##########
wthr_FMC <- read_csv("data/processed/weather_stations/wthr_1hr_FMC.csv") %>%
  filter(site!="HQ_AWC")
SILO <- read_csv("data/external_data/SILO_processed.csv") %>%
  filter(site!="HQ_AWC")
pine_flux <- read_csv("data/processed/wood_respiration/pine_CO2_clean.csv") %>%
  filter(CO2_resp_rate > 0)
native_flux <- read_csv("data/processed/wood_respiration/native_CO2_clean.csv")
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

# Set color palettes
library(palettetown)
site_palette <- pokepal(254,5)
site_palette_a <- c("#98D048","#386020","#C08038","#F8E068","#D05038")
site_strip <- strip_themed(background_x = elem_list_rect(fill = site_palette_a))
# Blue gradient to show rainfall gradient
site_palette2 <- c("#303870","#4880D0","#3860B0","#98C8F8","#58A0F8")
site_palette_a2 <- c("#303870","#3860B0","#4880D0","#58A0F8","#98C8F8")
site_strip2 <- strip_themed(background_y = elem_list_rect(fill = site_palette_a2))

# Species palette
DRO_sp_palette <- pokepal(44,10)
PNW_sp_palette <- pokepal(141,6)
all_DRO_palette <- c("#7090A0","#F8C050","#486878","#603000","#A07030","#D0A060",
                     "#E84800","#F87000","#700A1F","#103058","#805010")
all_PNW_palette <- c("#D8C088","#F8E0A8","#B09860","#686868","#685820",
                     "#700A1F","#D8D8D0")
all_sp_palette <- c("#7090A0","#F8C050","#486878","#603000","#A07030","#D0A060",
                    "#D8C088","#F8E0A8","#B09860","#686868","#E84800","#F87000",
                    "#685820","#700A1F","#103058","#805010","#D8D8D0")


########## Weather figures ##########

#..Main ####
#......Weather for each site (rainfall and temperature) ####
ggplot(wthr_FMC) + 
  #geom_point(aes(date,AirTC_Avg),color="red",alpha=0.2,size=0.25) +
  geom_line(aes(date,AirTC_Avg),stat="identity",color="red",alpha=0.5) + 
  geom_bar(aes(date,Rain_mm_Tot),stat="identity",color="blue") + 
  facet_grid2(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) +
  xlab(label = "Date") + 
  scale_y_continuous(name="Air Temperature (C)",
                     sec.axis=sec_axis(~.,name="Rainfall (mm/hr)")) +
  theme_bw() +
  theme(panel.grid.major = element_blank())


#..Supplement #### 
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
  group_by(site.year) %>%
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

ggplot() + 
  geom_point(data=SILO_rain_f,aes(year_mon,SILO),color="green") + 
  geom_line(data=SILO_rain_f,aes(year_mon,SILO),color="green",linetype="dashed") + 
  geom_point(data=wthr_rain,aes(year_mon,CHRS_filled,color=Rain_source)) +
  geom_line(data=wthr_rain,aes(year_mon,CHRS_filled),color="ivory4") + 
  scale_color_manual(name="Rainfall source",values =c("maroon","ivory4"),
                     labels=c("CHRS CCS", "WTF Stations")) + 
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  xlab("Month") + ylab("Rainfall (mm/month)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank())


#......Gap-filled weather data ####
pdf(file="figures/wthr_gap_filled.pdf",width=11,height=8.5)
ggplot(wthr_FMC,aes(date,AirTC_Avg,color=AirTC_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c("slateblue1","ivory4")) + 
  xlab("Date") + ylab("Air temperature (C)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,ib_AirTC_Avg,color=ib_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c("gray35","ivory4")) +
  xlab("Date") + ylab("ibutton Air temperature (C)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot() + 
  geom_point(data=wthr_FMC,
             aes(date,Rain_mm_Tot,color=Rain_source),alpha=0.3,size=0.3) + 
  scale_color_manual(values=c("maroon","slateblue1","ivory4")) +
  geom_point(data=filter(wthr_FMC,Rain_source=="NASA_POWER"),
             aes(date,Rain_mm_Tot),alpha=0.3,size=0.3,color="slateblue1") +
  xlab("Date") + ylab("Rainfall (mm/hr)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,BP_mbar_Avg,color=BP_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c("slateblue1","ivory4")) +
  xlab("Date") + ylab("Barometric pressure (mbar)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,RH_Avg,color=RH_source)) + 
  geom_point(alpha=0.3,size=0.3) + 
  scale_color_manual(values=c("slateblue1","ivory4")) +
  xlab("Date") + ylab("Relative humidity (%)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,WS2M)) + 
  geom_point(alpha=0.3,size=0.3,color="slateblue1") + 
  xlab("Date") + ylab("Wind speed (m/s)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,ALLSKY_SFC_SW_DWN)) + 
  geom_point(alpha=0.3,size=0.3,color="slateblue1") + 
  xlab("Date") + ylab("Shortwave radiation (W/m^2)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,ALLSKY_SFC_LW_DWN)) + 
  geom_point(alpha=0.3,size=0.3,color="slateblue1") + 
  xlab("Date") + ylab("Longwave radiation (W/m^2)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,solar_elevation)) + 
  geom_point(alpha=0.3,size=0.3,color="slateblue1") + 
  xlab("Date") + ylab("Solar elevation (degrees)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,solar_azimuth)) + 
  geom_point(alpha=0.3,size=0.3,color="gray35") + 
  xlab("Date") + ylab("Solar azimuth (degrees)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggplot(wthr_FMC,aes(date,FMC_norm)) + 
  geom_point(alpha=0.3,size=0.3,color="ivory4") + 
  xlab("Date") + ylab("Fuel moisture content, normalized (%)") +
  facet_grid(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")~.) + 
  theme_bw() +
  theme(panel.grid.major = element_blank())
dev.off()



########## Flux figures ##########

#..Main #### 
#......Bayesian model ####
library(brms)
library(tidybayes)

# Bayesian model 1
m1 <- brm(CO2_resp_rate ~ FMC + (1|site),data = pine_flux,iter = 5000,family="beta")
summary(m1)
m1$fit

# Posterior predictive check
pp_check(m1,ndraws=100)

# Check MCMC chain
plot(m1)

# Information criteria
loo_m1 = loo(m1)

# Assessing uncertainty
conditional_effects(m1,"FMC")
conditional_effects(m1,"FMC",spaghetti=T,ndraws=500)
m_fit = conditional_effects(m1,"FMC")

conditions <- data.frame(site = unique(pine_flux$site))
rownames(conditions) <- unique(pine_flux$site)
me_fit <- conditional_effects(m1, conditions = conditions,
                              re_formula = NULL, method = "predict")
bm_fits <- me_fit[[1]]

bm <- ggplot() + 
  geom_smooth(bm_fits,mapping=aes(effect1__,estimate__,color=site)) + 
  scale_color_manual(name="Site",values=site_palette) +
  geom_ribbon(bm_fits,mapping=aes(x=effect1__,y=estimate__,
    ymin=lower__,ymax=upper__,fill=site),
    alpha=0.2,color=NA) +
  facet_wrap(~fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW")) +
  scale_fill_manual(name="Site",values=site_palette) +
  xlab("FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank())

# With pine points plotted on
bm + geom_point(pine_flux,mapping=aes(FMC,CO2_resp_rate),
                color="#700A1F",shape=17,alpha=0.85)

#......Natives + Bayesian model ####
all_flux <- rbind(pine_flux,native_flux) %>%
  mutate(Type = ifelse(Species.Code=="PIRA","Pine","Native")) %>%
  mutate_at(c("fungi_present","insect_present","root_present","termite_present"),
            as.factor)
DRO_flux <- filter(all_flux,site=="DRO")
PNW_flux <- filter(all_flux,site=="PNW")

DRO_bm <- filter(bm_fits,site=="DRO")
PNW_bm <- filter(bm_fits,site=="PNW")

ggplot() + 
  geom_smooth(DRO_bm,mapping=aes(effect1__,estimate__),color="#98D048") + 
  geom_ribbon(DRO_bm,mapping=aes(x=effect1__,y=estimate__,
                                      ymin=lower__,ymax=upper__),
              alpha=0.2,color=NA,fill="#98D048") +
  geom_point(DRO_flux,mapping=aes(FMC,CO2_resp_rate,
                                  color=Species.Code,shape=Type),
             alpha=0.85) +
  scale_color_manual(name="Species",values=all_DRO_palette) +
  xlab("FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank())

ggplot() + 
  geom_smooth(PNW_bm,mapping=aes(effect1__,estimate__),color="#D05038") + 
  geom_ribbon(PNW_bm,mapping=aes(x=effect1__,y=estimate__,
                                 ymin=lower__,ymax=upper__),
              alpha=0.2,color=NA,fill="#D05038") +
  geom_point(PNW_flux,mapping=aes(FMC,CO2_resp_rate,
                                  color=Species.Code,shape=Type),
             alpha=0.85) +
  scale_color_manual(name="Species",values=all_PNW_palette) +
  xlab("FMC (%)") + ylab("CO2 Flux (ug CO2/s/g)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank())



#..Supplement #### 
#......Fluxes recorded at each site ####
ggplot(pine_flux,aes(fct_relevel(site,"DRO","MLRF","MLES","STCK","PNW"),
                     CO2_resp_rate,fill=site)) + 
  geom_boxplot() +
  scale_fill_manual(name="Site",values=site_palette) +
  xlab("Site") + ylab("CO2 Flux (ug CO2/s/g)") + 
  theme_bw()




