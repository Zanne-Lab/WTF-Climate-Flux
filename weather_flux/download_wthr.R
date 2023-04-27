# Download climate data from WTF-Climate repo

library(tidyverse)

wthr_url <- "https://github.com/Zanne-Lab/WTF-Climate/raw/main/wthr_1hr_final.csv"
wthr_final <- read_csv(url(wthr_url))

# Select variables for FMC-flux model
wthr_FMC <- wthr_final %>%
  filter(site!="HQ_AWC") %>%
  select(site,date,LON,LAT,FuelM_Avg,FMC_norm,
         AirTC_Avg,AirTC_source,Rain_mm_Tot,Rain_source,
         BP_mbar_Avg,BP_source,RH_Avg,RH_source,ib_AirTC_Avg,ib_source,WS2M,
         ALLSKY_SFC_SW_DWN,CLRSKY_SFC_SW_DWN,ALLSKY_SFC_LW_DWN,
         solar_elevation,solar_azimuth)
write_csv(wthr_FMC,"weather_flux/data/processed/weather_stations/wthr_1hr_FMC.csv")
