# Download, clean, and merge WTF weather station data and DRO weather data

# Load packages and set working directory
setwd("weather-flux")
library(googledrive)
library(tidyverse)
library(lubridate)
library(modelr)

########## Download data from Google Drive ##########

# Create local directories to store data
# Creates: data/weather_stations/ and data/processed/weather_stations/,
# data/ibuttons/ and data/processed/ibuttons/,
make_ws_dirs <- function() {
  if(!dir.exists("data/weather_stations/")){
    message(paste("ATTENTION, This function is creating the following directory ", 
                  getwd(),"/data/weather_stations/"))
    dir.create("data/weather_stations/",recursive = TRUE)
  }
  
  if(!dir.exists("data/processed/weather_stations/")){
    message(paste("ATTENTION, This function is creating the following directory ", 
                  getwd(),"/data/processed/weather_stations/"))
    dir.create("data/processed/weather_stations/",recursive = TRUE)
  }
  if(!dir.exists("data/ibuttons/")){
    message(paste0("ATTENTION, This function is creating the following directory ", 
                   getwd(),"/data/ibuttons/"))
    dir.create("data/ibuttons/",recursive = TRUE)
  }
  
  if(!dir.exists("data/processed/ibuttons/")){
    message(paste0("ATTENTION, This function is creating the following directory ", 
                   getwd(),"/data/processed/ibuttons/"))
    dir.create("data/processed/ibuttons/",recursive = TRUE)
  }
}
make_ws_dirs()

# Get files names for weather data from Google Drive
drive_ls("WTFProject/data/weather_stations/Met_data_Current")
ws_files <- drive_ls("WTFProject/data/weather_stations/Met_data_Current", 
                     recursive = TRUE, pattern = ".dat|.csv")

# Download files from drive 
for(x in 1:length(ws_files$id)){
  if(!file.exists(sprintf("data/weather_stations/%s",ws_files$name[x]))){
    drive_download(ws_files$id[x],path = sprintf("data/weather_stations/%s",ws_files$name[x]))
  } else {    message(paste0("File ", ws_files$name[x]," exist"))    }
}

# Get files names for ibutton data from Google Drive
ib_files <- drive_ls("WTFProject/data/i_buttons", recursive = TRUE, pattern= ".txt")

# Download files from drive
dowload_ib_file <- function(x) {
  if(!file.exists(sprintf("data/ibuttons/%s",x))){
    drive_download(x,path = sprintf("data/ibuttons/%s",x))
  } else {    message(paste0("File ", x," exists"))    }
  
}

invisible(sapply(ib_files$name, dowload_ib_file,simplify = TRUE))

detach("package:googledrive", unload = TRUE)



########## Read in downloaded WTF weather station files ##########
weather_files <- list.files(path = "data/weather_stations",pattern = "ten_min",full.names = TRUE)

read_wthr <- function(x) {
  # read in data
  dat <- read_csv(file = x, skip = 4,col_names = FALSE,
                  col_types = cols(X1 = col_datetime(format = ""),
                                   X2 = col_integer(),
                                   X3 = col_double(),
                                   X4 = col_double(),
                                   X5 = col_double(),
                                   X6 = col_double(),
                                   X7 = col_double(),
                                   X8 = col_double(),
                                   X9 = col_double(),
                                   X10 = col_double(),
                                   X11 = col_double(),
                                   X12 = col_double(),
                                   X13 = col_integer(),
                                   X14 = col_double(),
                                   X15 = col_double(),
                                   X16 = col_double(),
                                   X17 = col_double(),
                                   X18 = col_double(),
                                   X19 = col_double(),
                                   X20 = col_double(),
                                   X21 = col_double(),
                                   X22 = col_double(),
                                   X23 = col_double(),
                                   X24 = col_double(),
                                   X25 = col_double(),
                                   X26 = col_double(),
                                   X27 = col_double(),
                                   X28 = col_double()))
  # Get site id from file name
  name <- x
  site <- gsub("(data/weather_stations/wtf_)(.+)(_ten_min.+)",replacement = "\\2",x = name)
  dat_out <- cbind(site, dat)
  return(dat_out)
  
}
wthr_list <- sapply(weather_files, read_wthr, USE.NAMES= TRUE, simplify = FALSE)

# Join all weather stations data into single dataframe
wthr_dat <- dplyr::bind_rows(wthr_list)

# Rename headers
wthr_metadata <- read_csv("data/weather_stations/wthr_metadata.csv",n_max = 2,col_names = TRUE)
colnames(wthr_dat) <- c("site",colnames(wthr_metadata))

# Remove duplicated rows and standardize NAs
wthr_dat <- wthr_dat %>% distinct()
wthr_dat[is.na(wthr_dat)] <- NA

# Change site ids to be consistent with other files
wthr_dat$site[wthr_dat$site=="pnw"] <- "PNW"
wthr_dat$site[wthr_dat$site=="stck"] <- "STCK"
wthr_dat$site[wthr_dat$site=="mtlwrf"] <- "MLRF"
wthr_dat$site[wthr_dat$site=="mtlwsc"] <- "MLES"
wthr_dat$site[wthr_dat$site=="hq_awc"] <- "HQ_AWC"
#write_csv(wthr_dat,"data/processed/weather_stations/wthr_WTF_10min_raw.csv")



########## Visualize and clean WTF station data ##########

library(scattermore)
# Vaisala WXT530 variables:
# Barometric pressure
ggplot(wthr_dat,aes(TIMESTAMP,BP_mbar_Avg)) + 
  geom_scattermore(pointsize=1.5) + 
  facet_wrap(site~.) 
#MLES and MLRF have major issues while other sites are fine

# Remove problematic BP points 
# Mark points outside the seasonal trend as outliers
wthr_ML <- wthr_dat %>%
  filter(site%in%c("MLES","MLRF")) %>%
  mutate(BP_outlier = case_when(BP_mbar_Avg < 890 ~ "Yes",
                                BP_mbar_Avg > 920 ~ "Yes", TRUE ~ "No"))
ggplot(wthr_ML,aes(TIMESTAMP,BP_mbar_Avg,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + geom_hline(yintercept=c(890,920)) + 
  facet_wrap(site~.) 
# MLES and MLRF have major issues while other sites are fine

# Use anomalize to mark remaining outliers
library(anomalize)
wthr_ML_a <- wthr_ML %>%
  filter(BP_outlier == "No") %>%
  filter(!is.na(BP_mbar_Avg)) %>%
  arrange(site,TIMESTAMP) %>%
  group_by(site) %>%
  time_decompose(BP_mbar_Avg, merge = TRUE) %>%
  # Alpha adjusted to avoid marking a low pressure event common across all sites as an outlier
  anomalize(remainder,alpha=0.0275) %>% 
  time_recompose() 
wthr_ML_a %>% plot_anomalies(ncol = 3, alpha_dots = 0.25)

wthr_ML2 <- wthr_ML_a %>%
  select(site,TIMESTAMP,anomaly) %>%
  right_join(wthr_ML,by=c("site","TIMESTAMP")) %>%
  mutate(BP_outlier = ifelse(is.na(anomaly),BP_outlier,anomaly)) %>%
  select(-anomaly) %>%
  distinct()
ggplot(wthr_ML2,aes(TIMESTAMP,BP_mbar_Avg,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + facet_wrap(site~.) 

# Run anomalize again with post 2019-04-01 MLRF data to catch remaining outliers
wthr_MLRF_a <- wthr_ML2 %>%
  filter(site=="MLRF") %>%
  filter(TIMESTAMP > as_datetime("2019-04-01 00:00:00")) %>%
  filter(BP_outlier == "No") %>%
  filter(!is.na(BP_mbar_Avg)) %>%
  arrange(site,TIMESTAMP) %>%
  time_decompose(BP_mbar_Avg, merge = TRUE) %>%
  anomalize(remainder,alpha=0.05) %>% 
  time_recompose() 
wthr_MLRF_a %>% plot_anomalies(ncol = 3, alpha_dots = 0.25)

# Merge back into ML dataset
wthr_ML3 <- wthr_MLRF_a %>%
  select(site,TIMESTAMP,anomaly) %>%
  right_join(wthr_ML2,by=c("site","TIMESTAMP")) %>%
  mutate(BP_outlier = ifelse(is.na(anomaly),BP_outlier,anomaly)) %>%
  select(-anomaly) %>%
  distinct()

# Check points to be removed
ggplot(wthr_ML3,aes(TIMESTAMP,BP_mbar_Avg,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + facet_wrap(site~.) 

# BP error can indicate that the station is down, check other Vaisala variables
ggplot(wthr_ML3,aes(TIMESTAMP,AirTC_Avg,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + facet_wrap(site~.) 
ggplot(wthr_ML3,aes(TIMESTAMP,RH_Avg,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + facet_wrap(site~.) 
ggplot(wthr_ML3,aes(TIMESTAMP,Rain_mm_Tot,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + facet_wrap(site~.) 
ggplot(wthr_ML3,aes(TIMESTAMP,WS_ms_Avg,color=BP_outlier)) + 
  geom_point(size=0.5,alpha=0.5) + facet_wrap(site~.) 
# BP errors look to also be Air temp errors, but not in other variables
# There are two additional point above 40 C that was missed by the BP filter

wthr_ML4 <- wthr_ML3 %>%
  mutate(BP_mbar_Avg = ifelse(BP_outlier=="Yes",NA,BP_mbar_Avg),
         AirTC_Avg = ifelse(BP_outlier=="Yes",NA,AirTC_Avg),
         AirTC_Avg = ifelse(AirTC_Avg > 40,NA,AirTC_Avg)) %>%
  select(-BP_outlier)

# Merge changes back to main dataframe
wthr_dat2 <- wthr_dat %>%
  filter(!site%in%c("MLES","MLRF")) %>%
  rbind(wthr_ML4)

# Check final pressure points
ggplot(wthr_dat2,aes(TIMESTAMP,BP_mbar_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# No obvious outliers observed

# Air Temp
ggplot(wthr_dat2,aes(TIMESTAMP,AirTC_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# No obvious outliers observed

# Relative humidity
ggplot(wthr_dat2,aes(TIMESTAMP,RH_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# 0 values in MLES and MLRF still need to be taken care of

# Wind speed
ggplot(wthr_dat2,aes(TIMESTAMP,WS_ms_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# No obvious outliers observed

# Precipitation
ggplot(wthr_dat2,aes(TIMESTAMP,Rain_mm_Tot)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# Remove one outlier in PNW above 150 mm

# Address remaining outliers
wthr_dat3 <- wthr_dat2 %>%
  mutate(RH_Avg = ifelse(RH_Avg < 1,NA,RH_Avg), # Removes RH=0 points from MLES and MLRF
         Rain_mm_Tot = ifelse(Rain_mm_Tot > 150,NA,Rain_mm_Tot)) # Removes 1 point above 150 in PNW


# CS506 variables:
# PA/Soil dialectric conductivity
ggplot(wthr_dat3,aes(TIMESTAMP,PA_uS_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few outliers that could be easily marked

# FuelM
ggplot(wthr_dat3,aes(TIMESTAMP,FuelM_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few potential outliers, may correspond with PA outliers
# Set negative values to 0

# VWC
ggplot(wthr_dat3,aes(TIMESTAMP,VWC_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few outliers, may correspond with PA outliers

# EC/Soil electric conductivity
ggplot(wthr_dat3,aes(TIMESTAMP,EC_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few potential outliers, may correspond with PA outliers

# Soil temp
ggplot(wthr_dat3,aes(TIMESTAMP,TSoil_Avg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few outliers, may correspond with PA outliers

# Permitivity
ggplot(wthr_dat3,aes(TIMESTAMP,Perm)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few outliers, may correspond with PA outliers

# Permitivity avg
ggplot(wthr_dat3,aes(TIMESTAMP,PerAvg)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# A few outliers, may correspond with PA outliers

# Mark low PA values as outliers
wthr_dat4 <- wthr_dat3 %>%
  mutate(PA_outlier = ifelse(PA_uS_Avg<10,"Yes","No"))

# View marked outliers
ggplot(wthr_dat4,aes(TIMESTAMP,PA_uS_Avg,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
ggplot(wthr_dat4,aes(TIMESTAMP,FuelM_Avg,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
ggplot(wthr_dat4,aes(TIMESTAMP,VWC_Avg,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
ggplot(wthr_dat4,aes(TIMESTAMP,EC_Avg,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
ggplot(wthr_dat4,aes(TIMESTAMP,TSoil_Avg,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
ggplot(wthr_dat4,aes(TIMESTAMP,Perm,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
ggplot(wthr_dat4,aes(TIMESTAMP,PerAvg,color=PA_outlier)) + geom_scattermore(pointsize=1.5) + facet_wrap(site~.)
# PA outliers are consistent across all stick variables

# Remove Campbell stick errors
wthr_dat5 <- wthr_dat4 %>%
  mutate(PA_uS_Avg = ifelse(PA_outlier=="Yes",NA,PA_uS_Avg),
         FuelM_Avg = ifelse(PA_outlier=="Yes",NA,FuelM_Avg),
         VWC_Avg = ifelse(PA_outlier=="Yes",NA,VWC_Avg),
         ECAvg = ifelse(PA_outlier=="Yes",NA,EC_Avg),
         TSoil_Avg = ifelse(PA_outlier=="Yes",NA,TSoil_Avg),
         Perm = ifelse(PA_outlier=="Yes",NA,Perm),
         PerAvg = ifelse(PA_outlier=="Yes",NA,PerAvg)) %>%
  mutate(FuelM_Avg = ifelse(FuelM_Avg<0,0,FuelM_Avg))

# Create 30 min resolution data to merge with DRO data
wthr_dat6 <- wthr_dat5 %>%
  mutate(date = floor_date(TIMESTAMP, "30 mins")) %>%
  group_by(site,date) %>%
  summarize(PA_uS_Avg = mean(PA_uS_Avg, na.rm = TRUE),
            FuelM_Avg=mean(FuelM_Avg, na.rm = TRUE),
            VWC_Avg= mean(VWC_Avg, na.rm = TRUE),
            EC_Avg= mean(EC_Avg, na.rm = TRUE),
            TSoil_Avg = mean(TSoil_Avg, na.rm = TRUE),
            Perm= mean(Perm, na.rm = TRUE),
            PerAvg= mean(PerAvg, na.rm = TRUE),
            AirTC_Avg = mean(AirTC_Avg, na.rm = TRUE),
            AirTC_Max = max(AirTC_Max),
            AirTC_Min = min(AirTC_Min),
            Rain_mm_Tot= sum(Rain_mm_Tot, na.rm = FALSE),
            BP_mbar_Avg = mean(BP_mbar_Avg, na.rm = TRUE),
            BP_mbar_Max = max(BP_mbar_Max),
            BP_mbar_Min = min(BP_mbar_Min),
            RH_Avg = mean(RH_Avg, na.rm = TRUE),
            RH_Max = max(RH_Max),
            RH_Min = min(RH_Min),
            WS_ms_Avg= mean(WS_ms_Avg, na.rm = TRUE),
            WS_ms_Max= max(WS_ms_Max),
            WindDir_DU_WVT= mean(WindDir_DU_WVT, na.rm = TRUE))
#write_csv(wthr_dat6,"data/processed/weather_stations/wthr_WTF_30min_clean.csv")



########## Read in, clean, and merge in DRO station data ########## 

# Original long term DRO crane station (Vaisala WXT520)
dro_crane_long <- read_csv("data/dro_data/dro_station.csv") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  select(-1) %>%
  rename(AirTC_Avg_long = AirTC_Avg,
         RH_Avg_long = RH_Avg,
         Rain_mm_Tot_long = Rain_mm_Tot) 

# View data
ggplot(dro_crane_long,aes(TIMESTAMP,AirTC_Avg_long)) + geom_point(alpha=0.5) # Remove some 0 values
ggplot(dro_crane_long,aes(TIMESTAMP,RH_Avg_long)) + geom_point(alpha=0.5) # Remove low values below 10
ggplot(dro_crane_long,aes(TIMESTAMP,BP_mbar_Avg)) + geom_point(alpha=0.5) # Some outliers

# Mark outliers
dro_crane_long2 <- dro_crane_long %>%
  mutate(BP_outlier = ifelse(BP_mbar_Avg<985,"Yes","No"),
         RH_outlier = ifelse(RH_Avg_long<10,"Yes","No"),
         AirTC_outlier = ifelse(AirTC_Avg_long==0,"Yes","No"))

# Address remaining BP outliers with anomalize
dro_a <- dro_crane_long2 %>%
  filter(BP_outlier=="No") %>%
  filter(!is.na(BP_mbar_Avg)) %>%
  arrange(TIMESTAMP) %>%
  time_decompose(BP_mbar_Avg, merge = TRUE) %>%
  anomalize(remainder,alpha=0.05) %>% 
  time_recompose() 
dro_a %>% plot_anomalies(ncol = 3, alpha_dots = 0.25)
# Although some marked points do not look like outliers, we have good estimates with 
# NASA POWER for gap-filling, so we will err on the side of removing more points

detach("package:anomalize", unload = TRUE)

dro_crane_long3 <- dro_a %>%
  select(TIMESTAMP,anomaly) %>%
  right_join(dro_crane_long2,by="TIMESTAMP") %>%
  mutate(BP_outlier = ifelse(is.na(anomaly),BP_outlier,anomaly)) %>%
  select(-anomaly)

# View outliers to be removed
ggplot(dro_crane_long3,aes(TIMESTAMP,AirTC_Avg_long,color=AirTC_outlier)) + geom_point(alpha=0.5)
ggplot(dro_crane_long3,aes(TIMESTAMP,RH_Avg_long,color=RH_outlier)) + geom_point(alpha=0.5) 
ggplot(dro_crane_long3,aes(TIMESTAMP,BP_mbar_Avg,color=BP_outlier)) + geom_point(alpha=0.5) 

# Remove outliers
dro_crane_long4 <- dro_crane_long3 %>%
  mutate(AirTC_Avg_long = ifelse(AirTC_outlier=="Yes",NA,AirTC_Avg_long),
         RH_Avg_long = ifelse(RH_outlier=="Yes",NA,RH_Avg_long),
         BP_mbar_Avg = ifelse(BP_outlier=="Yes",NA,BP_mbar_Avg))

# Newer DRO crane station, set up in 2020 (Vaisala HMP60)
dro_crane_new <- read_csv("data/dro_data/dro_met_processed.csv") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  select(-1)

dro_crane <- full_join(dro_crane_new,dro_crane_long4,by="TIMESTAMP") 

# Compare rainfall data between two datasets
dro_rain <- dro_crane %>%
  select(TIMESTAMP,Rain_mm_Tot_met,Rain_mm_Tot_long)

dro_rain_r <- dro_rain %>%
  pivot_longer(!TIMESTAMP,names_to = "Station",values_to = "Rain_mm_Tot") %>%
  mutate(Station = gsub("Rain_mm_Tot_","",Station)) 
  
ggplot(dro_rain_r,aes(TIMESTAMP,Rain_mm_Tot,color=Station)) + geom_point(alpha=0.5,size=0.3)
# The rainfall data from the original station went down from 2020-2022
# We will take the rainfall from the original station until 2020
# The rainfall from the newer station will be used as is (late 2020-2022)


# Fill air temp and relative humidity for the new station using data from the original station
# Check linear models
dro_AirTC_lm <- lm(AirTC_Avg_met ~ AirTC_Avg_long, data=dro_crane)
summary(dro_AirTC_lm)
par(mfrow=c(2,2))
plot(dro_AirTC_lm)

dro_RH_lm <- lm(RH_Avg_met ~ RH_Avg_long, data=dro_crane)
summary(dro_RH_lm)
plot(dro_RH_lm)

# Gap-fill dro crane data using linear models to gap fill new station with long-term station data
dro_crane2 <- dro_crane_long4 %>%
  mutate(Rain_mm_Tot_long = ifelse(TIMESTAMP>as.POSIXct("2020-01-01 00:00:00"),NA,Rain_mm_Tot_long)) %>%
  full_join(dro_crane_new,by="TIMESTAMP") %>%
  add_predictions(model=dro_AirTC_lm,var="AirTC_lm") %>%
  add_predictions(model=dro_RH_lm,var="RH_lm") %>%
  mutate(AirTC_Avg = ifelse(is.na(AirTC_Avg_met),AirTC_lm,AirTC_Avg_met),
         RH_Avg = ifelse(is.na(RH_Avg_met),RH_lm,RH_Avg_met),
         Rain_mm_Tot = ifelse(is.na(Rain_mm_Tot_long),Rain_mm_Tot_met,Rain_mm_Tot_long),
         site = "DRO") %>%
  rename(WS_ms_Avg = WS_ms_met) %>%
  select(site, TIMESTAMP, AirTC_Avg, RH_Avg, Rain_mm_Tot, BP_mbar_Avg, WS_ms_Avg)

# Read in and merge dro Campbell stick data
dro_soil_pit <- read_csv("data/dro_data/dro_soil_processed.csv")

dro_data <- left_join(dro_crane2,dro_soil_pit,by="TIMESTAMP") %>%
  rename(date = TIMESTAMP)

# Check for outliers
ggplot(dro_data,aes(date,AirTC_Avg)) + geom_point(alpha=0.5) # No obvious outliers observed
ggplot(dro_data,aes(date,RH_Avg)) + geom_point(alpha=0.5) # No obvious outliers observed
ggplot(dro_data,aes(date,Rain_mm_Tot)) + geom_point(alpha=0.5) # No obvious outliers observed
ggplot(dro_data,aes(date,BP_mbar_Avg)) + geom_point(alpha=0.5) # No obvious outliers observed
ggplot(dro_data,aes(date,WS_ms_Avg)) + geom_point(alpha=0.5) # No obvious outliers observed

# Campbell stick data
ggplot(dro_data,aes(date,PA_uS_Avg)) + geom_point(alpha=0.5) # Remove one point above 100
ggplot(dro_data,aes(date,FuelM_Avg)) + geom_point(alpha=0.5) # No obvious outliers observed
ggplot(dro_data,aes(date,VWC_Avg)) + geom_point(alpha=0.5) # Remove one point below 0
ggplot(dro_data,aes(date,TSoil_Avg)) + geom_point(alpha=0.5) # Remove point above 100

dro_data2 <- dro_data %>%
  mutate(PA_uS_Avg = ifelse(PA_uS_Avg>100,NA,PA_uS_Avg),
         VWC_Avg = ifelse(VWC_Avg<0,NA,VWC_Avg),
         TSoil_Avg = ifelse(TSoil_Avg<10 | TSoil_Avg>100,NA,TSoil_Avg))

# Check stick data again
ggplot(dro_data2,aes(date,PA_uS_Avg)) + geom_point(alpha=0.5) 
ggplot(dro_data2,aes(date,FuelM_Avg)) + geom_point(alpha=0.5)
ggplot(dro_data2,aes(date,VWC_Avg)) + geom_point(alpha=0.5)
ggplot(dro_data2,aes(date,TSoil_Avg)) + geom_point(alpha=0.5)
# The remaining far points in VWC and TSoil will not be removed as there is 
# so little DRO data, it's unclear whether they are outliers
#write_csv(dro_data2,"data/processed/weather_stations/dro_30min_clean.csv")

# Merge dro data with main dataframe
wthr <- bind_rows(wthr_dat6, dro_data2)



########## Read in, clean, and merge ibutton data ##########

# Rename file for consistency
file.rename("data/ibuttons/0201 WTF_MTES_H3.txt" ,"data/ibuttons/0201 WTF_MTES_H_3.txt")

# List ibutton files 
ib_files <- list.files(path = "data/ibuttons",pattern = ".txt",full.names = TRUE)

# Read in files
read_ib <- function(x) {
  #read in data
  dat <- read_csv(file = x,skip = 1)
  #values to numeric (may need to change gsub depending on how R reads the degrees symbol)
  dat$Values  <- as.numeric(gsub("..C",replacement = "",x = dat$Values))
  #take file name
  name <- x
  #make an id out of file name
  ibutton_id <- gsub(pattern = "(data/ibuttons/[[:digit:]]{1,9}\\sWTF_)(.+|AWC_HQ)(_[[:digit:]]{1}|_weather_station)(.txt)",
                     replacement = "\\2",x = name)
  #join id with weather data
  dat_out <- dat %>% mutate(ibutton_id = ibutton_id)
  return(dat_out)
  
}
ib_list <- sapply(ib_files, read_ib, USE.NAMES= TRUE, simplify = FALSE)

# Join data into single data.frame
ib_dat <- dplyr::bind_rows(ib_list)
# Remove duplicated rows
ib_dat <- unique(ib_dat)

# Process ibutton data to merge with main dataframe
ib_dat2 <- ib_dat %>%
  mutate(date = lubridate::ceiling_date(lubridate::dmy_hms(Reading),"30 min")) %>%
  rename(ib_AirTC=Values) %>%
  separate(ibutton_id, into=c("site","ib_plot_id"),sep="_") %>%
  select(-Reading)

# Correct site ids and plot ids
ib_dat2$site[grep("MLRF|MTRF",ib_dat2$site)] <- "MLRF"
ib_dat2$site[grep("MTES",ib_dat2$site)] <- "MLES"
ib_dat2$site[grep("AWC",ib_dat2$site)] <- "HQ_AWC"
ib_dat2$ib_plot_id[grep("HQ",ib_dat2$ib_plot_id)] <- "WS_only"

# Remove bad readings and average data per site
ib_dat3 <- ib_dat2 %>%
  filter(ib_AirTC > 4 & ib_AirTC < 50) %>%
  # Remove weather station ibuttons
  mutate(ib_plot_id = ifelse(is.na(ib_plot_id),"WS",ib_plot_id)) %>% 
  filter(ib_plot_id!="WS") %>%
  select(-ib_plot_id) %>%
  group_by(site,date) %>%
  summarize(ib_AirTC_mean = mean(ib_AirTC, na.rm=TRUE),
            ib_sd = sd(ib_AirTC,na.rm=TRUE))

# View points removed
ggplot(mutate(ib_dat2,outlier=ifelse(ib_AirTC < 4 | ib_AirTC > 50,"Yes","No")),
       aes(date,ib_AirTC,color=outlier)) + geom_point(alpha=0.5,size=0.5) + facet_wrap(site~.)

# View cleaned data
ggplot(ib_dat3, aes(date,ib_AirTC_mean)) + geom_point(alpha=0.5,size=0.5) + 
  facet_wrap(site~.)
#write_csv(ib_dat3,"data/processed/weather_stations/ib_30min_raw.csv")

# Merge with main dataframe
wthr2 <- left_join(wthr, ib_dat3, by=c("site","date"))
#write_csv(wthr2,"data/processed/weather_stations/wthr_ib_clean.csv")



########## Adjust temporal resolution for merging with gap-filling data ##########

# Merge data to 1 hour resolution
wthr3 <- wthr2 %>%
  mutate(date = floor_date(date, "1 hour")) %>%
  group_by(site,date) %>%
  summarize(PA_uS_Avg = mean(PA_uS_Avg, na.rm = TRUE),
            FuelM_Avg=mean(FuelM_Avg, na.rm = TRUE),
            VWC_Avg= mean(VWC_Avg, na.rm = TRUE),
            EC_Avg= mean(EC_Avg, na.rm = TRUE),
            TSoil_Avg = mean(TSoil_Avg, na.rm = TRUE),
            Perm= mean(Perm, na.rm = TRUE),
            PerAvg= mean(PerAvg, na.rm = TRUE),
            AirTC_Avg = mean(AirTC_Avg, na.rm = TRUE),
            AirTC_Max = max(AirTC_Max),
            AirTC_Min = min(AirTC_Min),
            Rain_mm_Tot= sum(Rain_mm_Tot, na.rm = FALSE),
            BP_mbar_Avg = mean(BP_mbar_Avg, na.rm = TRUE),
            BP_mbar_Max = max(BP_mbar_Max),
            BP_mbar_Min = min(BP_mbar_Min),
            RH_Avg = mean(RH_Avg, na.rm = TRUE),
            RH_Max = max(RH_Max),
            RH_Min = min(RH_Min),
            WS_ms_Avg= mean(WS_ms_Avg, na.rm = TRUE),
            WS_ms_Max= max(WS_ms_Max),
            WindDir_DU_WVT= mean(WindDir_DU_WVT, na.rm = TRUE),
            ib_AirTC_Avg = mean(ib_AirTC_mean, na.rm=TRUE))

# Adjust date range from 2018-06-04 to 2022-06-14
ts <- seq.POSIXt(from=as.POSIXlt("2018-06-04 12:00:00",tz="UTC"), 
                 to=as.POSIXlt("2022-06-14 12:00:00",tz="UTC"), 
                 by="1 hour")
ts2 <- as.data.frame(ts) %>%
  rename(date = ts)

sites <- c("DRO","MLRF","MLES","STCK","PNW","HQ_AWC")

wthr4 <- data.frame()
for (x in 1:6) {
  data <- wthr3 %>% filter(site == sites[x])
  seq <- data.frame(ts2) %>%
    mutate(site = sites[x])
  out <- left_join(seq,data,by = c("site","date"))
  wthr4 <- rbind(wthr4,out)
}

# Replace NaN with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
wthr4[is.nan(wthr4)] <- NA

# Save final dataset
write_csv(wthr4,"data/processed/weather_stations/wthr_1hr_clean.csv")



