# Gap fill weather data using external databases

# Load packages and set working directory
setwd("weather-flux")
library(tidyverse)
library(lubridate)
library(modelr)

# Read in data
wthr_clean <- read_csv("data/processed/weather_stations/wthr_1hr_clean.csv") %>%
  arrange(site,date)
POWER <- read_csv("data/external_data/POWER_processed.csv") %>%
  arrange(site,date)
CHRS <- read_csv("data/external_data/CHRS_processed.csv") %>%
  arrange(site,date)



########## Test linear models for gap-filling ########## 

# Check potential linear models with NASA POWER
wthr_POWER <- left_join(wthr_clean,POWER,by=c("date","site"))

AirTC_f <- lm(AirTC_Avg ~ T2M*site, data=wthr_POWER)
summary(AirTC_f)
par(mfrow=c(2,2))
plot(AirTC_f)
# r^2 ~0.88

BP_f <- lm(BP_mbar_Avg ~ PS*site, data=wthr_POWER)
summary(BP_f)
plot(BP_f)
# r^2 ~0.99

RH_f <- lm(RH_Avg ~ RH2M*site, data=wthr_POWER)
summary(RH_f)
plot(RH_f)
# r^2 ~0.65

WS_f <- lm(WS_ms_Avg ~ WS2M*site, data=wthr_POWER)
summary(WS_f)
plot(WS_f)
# r^2 ~0.40
# Low r^2 likely due to height differences between measurements

# Remove 0 values for rainfall linear models
Rain <- select(wthr_clean,site,date,Rain_mm_Tot) %>%
  left_join(select(POWER,site,date,PRECTOTCORR),by=c("site","date")) %>%
  left_join(select(CHRS,site,date,Rain_mm),by=c("site","date"))

Rain_P_lm <- lm(Rain_mm_Tot ~ PRECTOTCORR*site, data = filter(Rain, Rain_mm_Tot > 0 & PRECTOTCORR > 0))
summary(Rain_P_lm)
Rain_C_lm <- lm(Rain_mm_Tot ~ Rain_mm*site, data = filter(Rain, Rain_mm_Tot > 0 & Rain_mm > 0))
summary(Rain_C_lm)
Rain_PC_lm <- lm(Rain_mm ~ PRECTOTCORR*site, data = filter(Rain, PRECTOTCORR > 0 & Rain_mm > 0))
summary(Rain_PC_lm)
# r^2 are all low, >0.012

# Air temp and pressure can be filled with reliable linear models for all sites



########## Gap-fill weather data, marking sources ##########

# Add linear models for air temp and pressure
wthr_f <- wthr_POWER %>%
  add_predictions(model=AirTC_f, var="AirTC_Avg_lm") %>%
  add_predictions(model=BP_f, var="BP_mbar_Avg_lm") %>%
  left_join(CHRS,by=c("site","date")) %>%
  select(-day)

# Rainfall will be filled directly with CHRS first, and then POWER for remaining gaps
# Relative humidity will be filled by adjusting values to the max RH values of WTF stations
# Wind speed will not be filled--all NASA POWER values will be used instead

minmax_norm <- function(var,a,b){
  var_min <- min(var,na.rm=TRUE)
  var_max <- max(var,na.rm=TRUE)
  var_norm <- a + ((var-var_min)*(b-a)/(var_max-var_min))
  return(var_norm)
}

wthr_f2 <- wthr_f %>%
  group_by(site) %>%
  mutate(RH2M_norm = minmax_norm(RH2M,min(RH_Avg,na.rm = TRUE),max(RH_Avg,na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(FMC_norm = minmax_norm(FuelM_Avg,0,70)) %>%
  mutate(AirTC_source = ifelse(is.na(AirTC_Avg),"NASA_POWER (lm)","WTF_Station"),
         BP_source = ifelse(is.na(BP_mbar_Avg),"NASA_POWER (lm)","WTF_Station"),
         RH_source = ifelse(is.na(RH_Avg),"NASA_POWER (adjusted)","WTF_Station"),
         Rain_P_source = ifelse(is.na(Rain_mm_Tot),"NASA_POWER","WTF_Station"),
         Rain_C_source = ifelse(is.na(Rain_mm_Tot),"CHRS_CCS","WTF_Station"),
         Rain_source = ifelse(is.na(Rain_mm_Tot),
                              ifelse(is.na(Rain_mm),"NASA_POWER","CHRS_CCS"),"WTF_Station"),
         AirTC_Avg = ifelse(is.na(AirTC_Avg),AirTC_Avg_lm,AirTC_Avg),
         BP_mbar_Avg = ifelse(is.na(BP_mbar_Avg),BP_mbar_Avg_lm,BP_mbar_Avg),
         RH_Avg = ifelse(is.na(RH_Avg),RH2M_norm,RH_Avg),
         Rain_mm_Tot_P = ifelse(is.na(Rain_mm_Tot),PRECTOTCORR,Rain_mm_Tot),
         Rain_mm_Tot_C = ifelse(is.na(Rain_mm_Tot),Rain_mm,Rain_mm_Tot),
         Rain_mm_Tot = ifelse(is.na(Rain_mm_Tot_C),PRECTOTCORR,Rain_mm_Tot_C))



########## Extend ibutton data across all dates ##########

# Check linear model
ib_f <- lm(ib_AirTC_Avg ~ AirTC_Avg*site, data=wthr_f2)
summary(ib_f)
plot(ib_f)
# r^2 ~0.92

# Extend observations
wthr_f3 <- wthr_f2 %>%
  add_predictions(model=ib_f,var="ib_AirTC_lm") %>%
  mutate(ib_source = ifelse(is.na(ib_AirTC_Avg),"Extended","Observations"),
         ib_AirTC_Avg = ifelse(is.na(ib_AirTC_Avg),ib_AirTC_lm,ib_AirTC_Avg)) %>%
  # Set extended values under 4 to 4 (historical min across all sites)
  mutate(ib_AirTC_Avg = ifelse(ib_AirTC_Avg<4,4,ib_AirTC_Avg))



########## Calculate solar elevation and azimuth  ##########

# Load packages to calculate julian day and azimuth
library(insol)
library(solarPos)

# Add elevation data, taken from SILO grid
wthr_solar <- wthr_f3 %>%
  mutate(julian_day = JD(date),
         elevation = case_when(site == "DRO" ~ 277.6, site == "PNW" ~ 339.3,
                               site == "STCK" ~ 674.6, site == "MLES" ~ 674.6,
                               site == "MLRF" ~ 730.2, site == "HQ_AWC" ~ 367.1)) %>%
  mutate(solar = solarPosition(jd=julian_day,lon=LON,lat=LAT,elev=elevation,delta_t = 32.184,
                               temp=AirTC_Avg,pres=BP_mbar_Avg)) %>%
  select(site,date,solar,elevation)

# Merge to main dataframe
wthr_f4 <- do.call(data.frame,wthr_solar) %>%
  select(-solar.zenith) %>%
  rename(solar_azimuth = solar.azimuth) %>%
  right_join(wthr_f3,by=c("site","date")) %>%
  mutate(solar_elevation = 90 - SZA) 



########## Save final datasets ##########

# Dataset with all variables
wthr_f5 <- wthr_f4 %>%
  mutate(WS_height = ifelse(is.na(WS_ms_Avg),NA,case_when(site=="DRO" ~ 50, TRUE ~ 1.5))) %>%
  select(site,LON,LAT,elevation,date,YEAR,MM,DD,HR,
         PA_uS_Avg,FuelM_Avg,FMC_norm,VWC_Avg,EC_Avg,TSoil_Avg,Perm,PerAvg,
         AirTC_Avg,AirTC_source,AirTC_Max,AirTC_Min,Rain_mm_Tot,Rain_source,
         BP_mbar_Avg,BP_source,BP_mbar_Max,BP_mbar_Min,RH_Avg,RH_source,RH_Max,RH_Min,
         WS_ms_Avg,WS_height,WS_ms_Max,WindDir_DU_WVT,ib_AirTC_Avg,ib_source,
         ALLSKY_SFC_SW_DWN,CLRSKY_SFC_SW_DWN,ALLSKY_SFC_LW_DWN,
         solar_elevation,solar_azimuth,SZA,T2M,PRECTOTCORR,PS,RH2M,WS2M,Rain_mm,
         RH2M_norm,AirTC_Avg_lm,BP_mbar_Avg_lm,ib_AirTC_lm,Rain_mm_Tot_P,Rain_P_source,Rain_mm_Tot_C,Rain_C_source)

write_csv(wthr_f5,"data/processed/weather_stations/wthr_1hr_final.csv")

# Dataset with variables only relevant to the climate-flux project
wthr_FMC <- wthr_f5 %>%
  select(site,date,LON,LAT,FuelM_Avg,FMC_norm,AirTC_Avg,AirTC_source,Rain_mm_Tot,Rain_source,
         BP_mbar_Avg,BP_source,RH_Avg,RH_source,WS_ms_Avg,WS_height,ib_AirTC_Avg,ib_source,
         ALLSKY_SFC_SW_DWN,CLRSKY_SFC_SW_DWN,ALLSKY_SFC_LW_DWN,
         solar_elevation,solar_azimuth,SZA,T2M,PRECTOTCORR,PS,RH2M,WS2M,RH2M_norm,AirTC_Avg_lm,BP_mbar_Avg_lm,
         Rain_mm,Rain_mm_Tot_P,Rain_P_source,Rain_mm_Tot_C,Rain_C_source)
write_csv(wthr_FMC,"data/processed/weather_stations/wthr_1hr_FMC.csv")


