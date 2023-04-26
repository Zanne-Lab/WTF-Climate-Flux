# Download weather data from WTF-Climate repo

library(readr)
library(dplyr)

git_file <- "https://github.com/Zanne-Lab/WTF-Climate/raw/main/wthr_1hr_final.csv"
wthr_final <- read_csv(url(git_file))

# Select variables for FMC-flux model
wthr_FMC <- wthr_final %>%
  select(site,date,LON,LAT,FuelM_Avg,FMC_norm,AirTC_Avg,AirTC_source,Rain_mm_Tot,Rain_source,
         BP_mbar_Avg,BP_source,RH_Avg,RH_source,WS_ms_Avg,WS_height,ib_AirTC_Avg,ib_source,
         ALLSKY_SFC_SW_DWN,CLRSKY_SFC_SW_DWN,ALLSKY_SFC_LW_DWN,
         solar_elevation,solar_azimuth,SZA,T2M,PRECTOTCORR,PS,RH2M,WS2M,RH2M_norm,AirTC_Avg_lm,BP_mbar_Avg_lm,
         Rain_mm,Rain_mm_Tot_P,Rain_P_source,Rain_mm_Tot_C,Rain_C_source)
write_csv(wthr_FMC,"data/processed/weather_stations/wthr_1hr_FMC.csv")
