# Process DRO data from DRO Research Station to merge with WTF station data

# Load packages
library(tidyverse)
library(lubridate)
library(openxlsx)
library(rio)

########## DRO crane Vaisala WXT520 (original long term, every 30 mins) ##########

# Read in data
wthr_dro_station <- import_list(file="data/dro_data/DRO_Jib_WeatherStation_18-22.xls", 
                                setclass = "tbl", rbind = TRUE)

# Reformat date, rename variables, and adjust units to merge with WTF stations
wthr_dro_station2 <- wthr_dro_station %>%
  mutate_at(c("TIMESTAMP","Ps_WXT520_Avg","Rain_Rim800_Tot",
              "Rain_WXT520_Tot","Rh_WXT520_Avg","Ta_WXT520_Avg"),as.numeric) %>%
  filter(!is.na(TIMESTAMP))

wthr_dro_station3 <- wthr_dro_station2 %>%
  mutate(TIMESTAMP = ymd_hms(as.POSIXct(TIMESTAMP*60*60*24, tz="Etc/GMT+0",origin = "1899-12-30")),
         BP_mbar_Avg = Ps_WXT520_Avg*10,
         RH_Avg = Rh_WXT520_Avg*100,
         # Two rainfall measurements were taken, use WXT520 measurement first as
         # the equipment is most similar to that of WTF stations
         Rain_mm_Tot = ifelse(is.na(Rain_Rim800_Tot),Rain_WXT520_Tot*1.15,Rain_Rim800_Tot)) %>%
  rename(AirTC_Avg = Ta_WXT520_Avg) %>%
  select(TIMESTAMP, BP_mbar_Avg, RH_Avg, AirTC_Avg,Rain_mm_Tot)
write.csv(wthr_dro_station3, "data/dro_data/dro_station.csv")



########## DRO crane Vaisala HMP60 (new long term, every 10 mins) ##########
dro_met_20 <- read.xlsx("data/dro_data/DRO_Crane_Met_2020_cleaned.xlsx", sheet = "10min")
dro_met_21 <- read.xlsx("data/dro_data/DRO_Crane_Met_2021_cleaned.xlsx", sheet = "10min") 
dro_met_22 <- read.xlsx("data/dro_data/DRO_Crane_Met_2022_cleaned.xlsx", sheet = "10min") 
dro_met <- merge(merge(dro_met_20,dro_met_21,all=TRUE),dro_met_22,all=TRUE)

# Summarize data to every 30 mins for merging with WXT520 data
dro_met2 <- dro_met %>%
  mutate(TIMESTAMP = ymd_hms(as.POSIXct(datetime*60*60*24, tz="Etc/GMT+0",origin = "1899-12-30")),
         WS_ms_met = WS_Knots*0.514444) %>%
  rename(AirTC_Avg_met = Air_Temp,
         Rain_mm_Tot_met = Rain,
         RH_Avg_met = RH) %>%
  select(TIMESTAMP,AirTC_Avg_met,Rain_mm_Tot_met,RH_Avg_met,WS_ms_met)

dro_met3 <- dro_met2 %>%
  mutate(TIMESTAMP = floor_date(TIMESTAMP, "30 mins")) %>%
  group_by(TIMESTAMP) %>%
  summarize(AirTC_Avg_met = mean(AirTC_Avg_met,na.rm=TRUE),
            Rain_mm_Tot_met = sum(Rain_mm_Tot_met,na.rm=TRUE),
            RH_Avg_met = mean(RH_Avg_met,na.rm=TRUE),
            WS_ms_met = mean(WS_ms_met,na.rm=TRUE))

write.csv(dro_met3,"data/dro_data/dro_met_processed.csv")


########## DRO soil pit data (Campbell CS506) ##########
library(readxl)

# Read in and process 2019 data
dro_soil_19 <- read_excel("data/dro_data/DRO_soil_pit/DRO_Soil Pit_19.xls") %>%
  mutate_at(c(1:9), as.numeric) %>%
  select(-8,-9) %>%
  filter(!is.na(TIMESTAMP))

dro_soil_19a <- dro_soil_19 %>%
  mutate(TIMESTAMP = ymd_hms(as.POSIXct(as.numeric(TIMESTAMP)*60*60*24, tz="Etc/GMT+0",origin = "1899-12-30")),
         VWC_Avg = rowMeans(dro_soil_19 %>% select(2,3,4))) %>%
  rename(FuelM_Avg = Fuel_CS506_Avg,
         TSoil_Avg = TERN_Ts_TCAV_01_Avg,
         PA_uS_Avg = PAuS_CS506_Avg) %>%
  select(-2,-3,-4)

# Read in and process 2020-22 data
dro_soil_20 <- read_excel("data/dro_data/DRO_soil_pit/DROsoil_Start-2020-01_Stop-2021-01_y2022m06d16h17m05s57.xls",
                          sheet = "Data",col_names = TRUE) %>%
  mutate_at(c(1:47), as.numeric) %>%
  slice(-(1:2)) %>%
  select(TIMESTAMP,TERN_Fuel_CS506_Avg,TERN_Ts_TCAV_01_Avg,TERN_PAuS_CS506_Avg,
         TERN_Sws_616_03_Avg,TERN_Sws_616_06_Avg,TERN_Sws_616_09_Avg)

dro_soil_21 <- read_excel("data/dro_data/DRO_soil_pit/DROsoil_Start-2021-01_Stop-2022-01_y2022m06d16h17m06s05.xls",
                          sheet = "Data",col_names = TRUE) %>%
  mutate_at(c(1:47), as.numeric) %>%
  slice(-(1:2)) %>%
  select(TIMESTAMP,TERN_Fuel_CS506_Avg,TERN_Ts_TCAV_01_Avg,TERN_PAuS_CS506_Avg,
         TERN_Sws_616_03_Avg,TERN_Sws_616_06_Avg,TERN_Sws_616_09_Avg)

dro_soil_22 <- read_excel("data/dro_data/DRO_soil_pit/DROsoil_Start-2022-01_Stop-2023-01_y2022m06d16h17m06s14.xls",
                          sheet = "Data",col_names = TRUE) %>%
  mutate_at(c(1:47), as.numeric) %>%
  slice(-(1:2)) %>%
  select(TIMESTAMP,TERN_Fuel_CS506_Avg,TERN_Ts_TCAV_01_Avg,TERN_PAuS_CS506_Avg,
         TERN_Sws_616_03_Avg,TERN_Sws_616_06_Avg,TERN_Sws_616_09_Avg)

# Merge datasets
dro_soil2 <- merge(merge(dro_soil_20,dro_soil_21,all=TRUE),dro_soil_22,all=TRUE)

# Reformat date, rename variables, and adjust units to merge with WTF stations
dro_soil2a <- dro_soil2 %>%
  mutate(TIMESTAMP = ymd_hms(as.POSIXct(as.numeric(TIMESTAMP)*60*60*24, tz="Etc/GMT+0",origin = "1899-12-30")),
         # 3 VWC measurements were averaged out
         VWC_Avg = rowMeans(dro_soil2 %>% select(5,6,7))) %>%
  rename(FuelM_Avg = TERN_Fuel_CS506_Avg,
         TSoil_Avg = TERN_Ts_TCAV_01_Avg,
         PA_uS_Avg = TERN_PAuS_CS506_Avg) %>%
  select(-TERN_Sws_616_03_Avg,-TERN_Sws_616_06_Avg,-TERN_Sws_616_09_Avg)

dro_soil_all <- merge(dro_soil_19a,dro_soil2a,all=TRUE) %>%
  # Remove outlier points
  filter(FuelM_Avg < 200 & FuelM_Avg > 0) 

write_csv(dro_soil_all,"data/dro_data/dro_soil_processed.csv")

