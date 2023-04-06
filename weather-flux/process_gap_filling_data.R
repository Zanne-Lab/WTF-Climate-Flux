# Process external data used for gap filling

# Load packages and set working directory
setwd("weather-flux")
library(tidyverse)
library(lubridate)

########## NASA POWER ##########
# This is the primary database used to gap-fill
# Wind speed and radiation will be used directly, as wind speed needs to be
# extrapolated due to height different and we did not collect radiation data


# Retrieve data using 'nasapower' R package
library(nasapower)
sites_lonlat <- tibble(site = c("DRO","MLRF","MLES","STCK","HQ_AWC","PNW")) %>%
  mutate( longitude = case_when(site == "DRO" ~ 145.4478, site == "PNW" ~ 144.9158,
                                site == "STCK" ~ 145.2406, site == "MLES" ~ 145.2615,
                                site == "MLRF" ~ 145.2754, site == "HQ_AWC" ~ 145.1860),
          latitude = case_when(site == "DRO" ~ -16.1012, site == "PNW" ~ -16.57427,
                              site == "STCK" ~ -16.61158, site == "MLES" ~ -16.58312,
                              site == "MLRF" ~ -16.5927, site == "HQ_AWC" ~ -16.5831))

POWER_dat <- data.frame()
for(x in 1:6){
  P1 <- get_power(community = "sb",
                  lonlat = c(sites_lonlat[[x,2]],sites_lonlat[[x,3]]),
                  pars = c("T2M","RH2M","PRECTOTCORR","PS","WS2M","ALLSKY_SFC_SW_DWN",
                           "CLRSKY_SFC_SW_DWN","SZA","ALLSKY_SFC_LW_DWN"),
                  dates = c("2018-06-01","2022-06-30"),
                  temporal_api = "hourly",
                  time_standard = "LST") %>%
    mutate(site = sites_lonlat[[x,1]])
  POWER_dat <- rbind(POWER_dat,P1)
}

# Reformat data for merging with WTF station data
POWER_dat2 <- POWER_dat %>%
  mutate(date = as_datetime(paste(YYYYMMDD," ",HR,"00:00"),tz="UTC")) %>%
  select(site,date,YEAR,MM,DD,HR,T2M,RH2M,PRECTOTCORR,PS,WS2M,ALLSKY_SFC_SW_DWN,
         CLRSKY_SFC_SW_DWN,SZA,ALLSKY_SFC_LW_DWN,LON,LAT) %>%
  filter(date > as.POSIXlt("2018-06-04 11:00:00",tz="UTC")) %>%
  filter(date < as.POSIXlt("2022-06-14 13:00:00",tz="UTC")) %>%
  # Set NA SZA to 90 when radiation is 0
  mutate(SZA = ifelse(ALLSKY_SFC_SW_DWN==0,90,SZA))

# Address solar data gap from 2022-01-07 to 2022-01-08
# Fill with averages from previous and proceeding 2 days
POWER_s <- POWER_dat2 %>%
  filter(date > as.POSIXlt("2022-01-05 11:00:00",tz="UTC")) %>%
  filter(date < as.POSIXlt("2022-01-10 13:00:00",tz="UTC")) %>%
  group_by(site,YEAR,MM,HR) %>%
  summarize(ALLSKY_SFC_SW_DWN_mean = mean(ALLSKY_SFC_SW_DWN,na.rm = TRUE),
            CLRSKY_SFC_SW_DWN_mean = mean(CLRSKY_SFC_SW_DWN,na.rm = TRUE),
            SZA_mean = mean(SZA,na.rm = TRUE),
            ALLSKY_SFC_LW_DWN_mean = mean(ALLSKY_SFC_LW_DWN,na.rm = TRUE))

POWER <- POWER_dat2 %>%
  left_join(POWER_s,by=c("site","YEAR","MM","HR")) %>%
  mutate(ALLSKY_SFC_SW_DWN = ifelse(is.na(ALLSKY_SFC_SW_DWN),ALLSKY_SFC_SW_DWN_mean,ALLSKY_SFC_SW_DWN),
         CLRSKY_SFC_SW_DWN = ifelse(is.na(CLRSKY_SFC_SW_DWN),CLRSKY_SFC_SW_DWN_mean,CLRSKY_SFC_SW_DWN),
         SZA = ifelse(is.na(SZA),SZA_mean,SZA),
         ALLSKY_SFC_LW_DWN = ifelse(is.na(ALLSKY_SFC_LW_DWN),ALLSKY_SFC_LW_DWN_mean,ALLSKY_SFC_LW_DWN)) %>%
  select(-ALLSKY_SFC_SW_DWN_mean,-CLRSKY_SFC_SW_DWN_mean,-SZA_mean,-ALLSKY_SFC_LW_DWN_mean)

write_csv(POWER,"data/external_data/POWER_processed.csv")


########## CHRS CCS ##########
# This data will be used to gap-fill rainfall data

# Read in files downloaded from website
CHRS_files <-  list.files(path = "data/external_data/CHRS",
                          pattern = "CCS_2018060100_2022063000_",full.names = TRUE)

CHRS_dat <- data.frame()
for(x in 1:6) {
  site <- gsub("data/external_data/CHRS/CCS_2018060100_2022063000_|.csv","",CHRS_files[x])
  file <- read_csv(CHRS_files[x]) %>% 
    select(1,2) %>%
    mutate(site = site)
  CHRS_dat <- rbind(CHRS_dat,file)
}

# Reformat data for merging with WTF station data
CHRS <- CHRS_dat %>%
  mutate(daytime = (gsub("CCS_1h","",Time)),
         date = ymd_h(daytime)) %>%
  separate(daytime,into=c("day","hour"),sep=8,remove=FALSE) %>%
  mutate(day = ymd(day)) %>%
  rename(Rain_mm = `Rain(mm)`) %>%
  select(site,date,Rain_mm,day) %>%
  mutate(Rain_mm = ifelse(Rain_mm<0,NA,Rain_mm))

write_csv(CHRS,"data/external_data/CHRS_processed.csv")


########## SILO Longpaddock Grid ##########
# This data will be used to calculate historical averages for comparisons

# Read in files downloaded from website
SILO_files <- list.files(path = "data/external_data/silo_grid_data/",
                         pattern = "SILO_historical_",full.names = TRUE)

# Reformat date for merging
SILO <- data.frame()
for(x in 1:6) {
  site <- gsub("data/external_data/silo_grid_data//SILO_historical_|.csv","",SILO_files[x])
  file <- read_csv(SILO_files[x]) %>% 
    select(1:4,6,8,10,12,14,16) %>%
    mutate(site = site) %>%
    rename(day = 'YYYY-MM-DD',
           sea_level_presssure = mslp)
  SILO <- rbind(SILO,file)
}

write_csv(SILO,"data/external_data/SILO_processed.csv")

