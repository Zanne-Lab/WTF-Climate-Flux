# Process flux data and calculate flux rates

# Load packages and set working directory
setwd("weather-flux")
library(googledrive)
library(tidyverse)
library(lubridate)
library(broom)
library(modelr)
library(ggforce)

########## Download data and prep files for reading ##########

# Create local directories to store data
# Creates: data/wood_respiration/ and data/processed/wood_respiration/
make_resp_wr_dirs <- function() {
  if(!dir.exists("data/wood_respiration/")){
    message(paste0("ATTENTION, This function is creating the following directory ",
                   getwd(),"/data/wood_respiration/"))
    dir.create("data/wood_respiration/")
  }
  
  if(!dir.exists("data/processed/wood_respiration/")){
    message(paste0("ATTENTION, This function is creating the following directory ",
                   getwd(),"/data/processed/wood_respiration/"))
    dir.create("data/processed/wood_respiration/")
  }
}
make_resp_wr_dirs()

# Get files names for respiration data from Google Drive
resp_files <- drive_ls("WTFProject/data/wood_respiration", recursive = TRUE, pattern= ".csv|.txt")

# Download files from drive 
for(x in 1:length(resp_files$id)){
  if(!file.exists(sprintf("data/wood_respiration/%s",resp_files$name[x]))){
    drive_download(resp_files$id[x],path = sprintf("data/wood_respiration/%s",resp_files$name[x]))
  } else {    message(paste0("File ", resp_files$name[x]," exist"))    }
}
# Run the above script a few times, as drive_ls is unreliable

detach(package:googledrive, unload = TRUE)

# Unzip zipped files
zip_resp <- list.files(path = "data/wood_respiration",pattern = ".txt.zip$",full.names = TRUE, recursive = TRUE)
sapply(zip_resp,unzip,exdir="data/wood_respiration",overwrite=TRUE)

# Rename files so naming conventions are consistent for read in
file.rename("data/wood_respiration/2019122dro642.csv","data/wood_respiration/2019122dro-642.csv")
file.rename("data/wood_respiration/2019618dro548.csv","data/wood_respiration/2019618dro-548.csv")
file.rename("data/wood_respiration/202169MLES299.csv","data/wood_respiration/202169MLES-299.csv")
file.rename("data/wood_respiration/202167%EF%BF%BFDRO-43%EF%BF%BF.csv","data/wood_respiration/202167DRO-43.csv")
file.rename("data/wood_respiration/2020125dro-666%EF%BF%BF.csv","data/wood_respiration/2020125dro-666.csv")
file.rename("data/wood_respiration/2021127rescue-1152.csv","data/wood_respiration/2021127RES-1152.csv")
file.rename("data/wood_respiration/2021127rescueterm-1152.csv","data/wood_respiration/2021127RES-1152-term.csv")
file.rename("data/wood_respiration/2021127rescuewood-1152.csv","data/wood_respiration/2021127RES-1152-wood.csv")



########## Read in files ##########
# Read in IRGA files
irgas_files <- list.files(path = "data/wood_respiration",pattern = "^gga.*.txt$",full.names = TRUE, recursive = TRUE)

read_irgas <- function(x) {
  # Read files
  irgas <- read_csv(x,skip = 1)
  # Remove special characters in columns
  colnames(irgas) <- gsub(pattern = "\\[|\\]",colnames(irgas),replacement = "")
  # Parse rows by seconds, and select one row per second
  irgas_out <- irgas %>%
    mutate(Time = floor_date(dmy_hms(irgas$Time), "second")) %>%
    distinct(Time, .keep_all = TRUE) %>%
    filter(!is.na(Time))
  return(irgas_out)
}

irgas_list <- sapply(irgas_files, read_irgas, USE.NAMES = TRUE, simplify = FALSE)
irgas_dat <- dplyr::bind_rows(irgas_list)


# Read in chamber files
chamber_files <- list.files(path = "data/wood_respiration",pattern = "[0-9]{5,8}.+csv",full.names = TRUE, recursive = TRUE)

read_chamber <- function(x) {
  # Read file name for sample id
  name <- x
  ids <-gsub("data/wood_respiration/[0-9]{5,8}|.csv",replacement = "",x = name)
  # Read file
  chamber <- read_csv(x)
  # Reformat dates
  chamber_out <- chamber %>%
    unite(year, month, day, hour, minute, second,col = Time) %>%
    mutate(Time = floor_date(ymd_hms(Time), "second")) %>%
    distinct(Time, .keep_all = TRUE)
  # Join id and data
  out <- cbind(ids, chamber_out) %>%
    separate(ids, c("resp_site","SampleID"),"(?<=[A-Za-z])-(?=[0-9])")
  return(out)
}

chambers_list <- sapply(chamber_files, read_chamber, USE.NAMES = TRUE, simplify = FALSE)
chamber_dat <- dplyr::bind_rows(chambers_list) %>%
  distinct(Time,.keep_all = TRUE) 

# Join IRGA and chamber data
wood_resp <- left_join(irgas_dat,chamber_dat,by = "Time") %>%
  filter(!is.na(Etime))

# Fix sample ID naming errors and inconsistencies
wood_resp2 <- wood_resp %>%
  filter(!is.na(resp_site) & !is.na(SampleID)) %>%
  filter(!SampleID%in%c("66-","720","723","1046")) # remove first readings
wood_resp2$SampleID[wood_resp2$SampleID=="66-2nd"] <- "66" # rename correct readings
wood_resp2$SampleID[wood_resp2$SampleID=="720-1"] <- "720"
wood_resp2$SampleID[wood_resp2$SampleID=="723-1"] <- "723"
wood_resp2$SampleID[wood_resp2$SampleID=="1046-a"] <- "1046"
unique(wood_resp2$SampleID)
#write_csv(wood_resp2,"data/processed/wood_respiration/wood_resp_raw.csv")



########## Clean raw IRGA data for linear respiration rate fit ##########

# Filter out one minute calibration time and last 10 seconds in case of chamber/IRGA time offset
# Mark samples with multiple reads and take the last of all reads
resp <- wood_resp2 %>%
  filter(Etime >=0 & Etime <= 170) %>%
  mutate(Read = case_when(id <= 240 ~ "first", id > 240 & id < 600 ~ "second",
                          id > 600 & id < 1000 ~ "third", TRUE ~ "fourth"))

resp2 <- resp %>% filter(Read=="second")
resp3 <- resp %>% filter(Read=="third") # only samples 201 and 585 has a fourth read
resp4 <- resp %>% filter(Read=="fourth") # only sample 585 has a fourth read

resp_c <- resp2 %>%
  filter(!SampleID%in%c("585","201")) %>%
  rbind(filter(resp3,SampleID=="201")) %>%
  rbind(resp4)

resp_c2 <- resp %>%
  filter(!SampleID%in%c(unique(resp_c$SampleID))) %>%
  rbind(resp_c) 

# Visualize IRGA time series for CO2 measurements
seq <- 1:18
pdf("data/processed/wood_respiration/CO2_IRGA_raw.pdf",width=11,height=8.5)
for(x in 1:length(seq)){
  plot <- ggplot(resp_c2,aes(Etime,CO2d_ppm)) + geom_point(size=0.7) + 
    facet_wrap_paginate(SampleID~.,nrow=8,ncol=8,scales="free",page=seq[x])
  print(plot)
}
dev.off()

# Visualize IRGA time series for CH4 measurements
pdf("data/processed/wood_respiration/CH4_IRGA_raw.pdf",width=11,height=8.5)
for(x in 1:length(seq)){
  plot <- ggplot(resp_c2,aes(Etime,CH4d_ppm)) + geom_point(size=0.7) + 
    facet_wrap_paginate(SampleID~.,nrow=8,ncol=8,scales="free",page=seq[x])
  print(plot)
}
dev.off()
# Some samples still show two reads and some samples show a jump in ppm, 
# indicating the chamber seal may have been broken

# Address samples which reads indicate a broken chamber seal
resp_c3 <- resp_c2 %>%
  mutate(remove_read = ifelse(SampleID=="232"&Etime>=90,"Yes",
                              ifelse(SampleID=="632"&Etime<=100,"Yes",
                                     ifelse(SampleID=="713"&Etime<=50,"Yes",
                                            ifelse(SampleID=="889"&Etime>=125,"Yes",
                                                   ifelse(SampleID=="971"&Etime>=125,"Yes","No")))))) %>%
  # Remove extra reads for 265 and 433 due to incorrect site id naming
  mutate(site_error = ifelse(resp_site=="bro"&SampleID=="433" | 
                               resp_site=="pw"&SampleID=="265","Yes","No")) %>%
  filter(site_error=="No") %>%
  filter(remove_read == "No")



########## Calculate CO2 and CH4 flux rates ##########

# CO2 flux
CO2intercept <- resp_c3 %>%
  group_by(SampleID) %>%
  group_modify(~broom::tidy(lm(CO2d_ppm~Etime, data=.x))) %>%
  bind_rows() %>%
  filter(term=="Etime") %>%
  select(SampleID,estimate,p.value) %>%
  rename(deltaCO2=estimate,
         CO2.p = p.value)

CO2r <- resp_c3 %>%
  group_by(SampleID) %>%
  group_modify(~broom::glance(lm(CO2d_ppm~Etime, data=.x))) %>%
  bind_rows() %>%
  select(SampleID,adj.r.squared) %>%
  rename(adj_r_CO2 = adj.r.squared)

CO2 <- left_join(CO2intercept,CO2r, by="SampleID") 

# CH4 flux
CH4intercept <- resp_c3 %>%
  group_by(SampleID) %>%
  group_modify(~broom::tidy(lm(CH4d_ppm~Etime, data=.x))) %>%
  bind_rows() %>%
  filter(term=="Etime") %>%
  select(SampleID,estimate,p.value) %>%
  rename(deltaCH4=estimate,
         CH4.p = p.value)

CH4r <- resp_c3 %>%
  group_by(SampleID) %>%
  group_modify(~broom::glance(lm(CH4d_ppm~Etime, data=.x))) %>%
  bind_rows() %>%
  select(SampleID,adj.r.squared) %>%
  rename(adj_r_CH4 = adj.r.squared)

CH4 <- left_join(CH4intercept,CH4r, by="SampleID")



########## Calculate mean chamber temperature across measurement ##########
Tcham <- resp %>%
  group_by(SampleID) %>%
  summarize(mean_Tcham = mean(Tcham, na.rm = TRUE))



########## Get wood block weights and other metadata ##########
# Processed weights from Law et al. 2023 (includes natives, pine gradient, drought, rescues)
weights_all <- read_csv("data/processed/wood_weights_all.csv") 

# FMC will be calculated later, so harvest_dry_wt was recalculated for pines for consistency
weights_all_c <- weights_all %>%
  filter(Species.Code=="PIRA") %>%
  mutate(Sawdust_FW = ifelse(Sawdust_FW==0,NA,Sawdust_FW)) %>%
  mutate(harvest_dry_wt = ifelse(is.na(Post_drill_FW),DW_Wood,FW_Wood*dry.wet)) %>%
  mutate(mass.loss = init_dry_wt - harvest_dry_wt) %>%
  rbind(filter(weights_all,Species.Code!="PIRA"))

# Calculate proportion of sample that was wood
# FW_Wood will be gap-filled with Field_fresh_wt
# Field_fresh_wt was filled with a linear model from individual FW components
weights_all2 <- weights_all_c %>%
  rowwise() %>%
  mutate(FW_tot = sum(c_across(c("FW_Wood","FW_Excess","FW_Carton","FW_Soil")),na.rm = TRUE),
         DW_tot = sum(c_across(c("harvest_dry_wt","DW_Excess","DW_Carton","DW_Soil")),na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(FW_tot = ifelse(FW_tot==0,NA,FW_tot),
         Collar = ifelse(is.na(Collar),5,Collar),
         wood_por = harvest_dry_wt/DW_tot)

# Gap fill Field_fresh_wt based on linear relationship between it and FW_Wood_tot
Field_lm <- lm(Field_fresh_wt ~ FW_tot,data=weights_all2)
summary(Field_lm)
plot(Field_lm)

# Missing volumes for first harvest are the same as block original dimensions: 9x9x5cm = 0.405L
# For respiration calculations, calculate Field_dry_wt from DW and Post drill weight for blocks that are less than 50% wood
# For blocks that are less than 50% wood, DW_tot will be used to include excess/soil/carton weight
weights_all3 <- weights_all2 %>%
  add_predictions(model=Field_lm,var="Field_FW_lm") %>%
  mutate(fresh_volume = case_when(harvest==1 ~ 0.405, TRUE ~ fresh_volume),
         Field_fresh_wt = ifelse(is.na(Field_fresh_wt),Field_FW_lm,Field_fresh_wt),
         FW_Wood = ifelse(is.na(FW_Wood),Field_fresh_wt,FW_Wood),
         resp_dry_wt = ifelse(wood_por>0.5,harvest_dry_wt,DW_tot)) %>%
  select(-Field_FW_lm)



########## Get pressure data from climate dataset ##########

# Read in climate dataset and select relevant columns
wthr_BP <- read_csv("data/processed/weather_stations/wthr_1hr_FMC.csv") %>%
  select(site, date, BP_mbar_Avg) %>% 
  mutate(date = ymd_hms(date)) %>%
  arrange(site,date) %>%
  rename(site_wthr = site)

# Add correct site IDs to resp data
# For droughts and rescues, DRO pressure data will be used
# Create 1 hr intervals with resp data to merge with pressure measurements
resp_1hr <- resp_c3 %>%
  mutate(date = ymd_hms(floor_date(Time, unit="1 hour"))) %>%
  # separate SampleID for merging with weights sheet
  separate(SampleID, c("SampleID","extra_id"),"-") %>%
  mutate(SampleID = as.numeric(SampleID)) %>%
  left_join(weights_all3,by="SampleID") %>%
  unite(SampleID,extra_id, col="SampleID",sep="-",na.rm=TRUE) %>%
  filter(!is.na(site)) %>%
  mutate(site_wthr = case_when(site=="STCK" ~ "STCK", site=="MLES" ~ "MLES",
                               site=="MLRF" ~ "MLRF", site=="PNW" ~ "PNW",
                               TRUE ~ "DRO")) %>%
  select(date,site_wthr,SampleID)

Pressure <- left_join(resp_1hr, wthr_BP, by=c("site_wthr","date")) %>%
  group_by(SampleID) %>%
  summarize(mean_Pressure = mean(BP_mbar_Avg,na.rm=TRUE)) %>%
  ungroup() 



########## Join datasets for unit conversion calculation ##########

rates <- CO2 %>%
  left_join(CH4, by="SampleID") %>%
  left_join(Tcham, by="SampleID") %>%
  left_join(Pressure, by="SampleID")

# Prep sample ids to merge with wood weights file
rates2 <- rates %>% 
  separate(SampleID, c("SampleID","extra_id"),"-") %>%
  mutate(SampleID = as.numeric(SampleID)) %>%
  filter(!is.na(SampleID)) %>%
  ungroup() 

resp_all <- weights_all3 %>%
  right_join(rates2, by="SampleID", multiple="all") %>%
  unite(SampleID,extra_id, col="SampleID",sep="-",na.rm=TRUE) %>%
  #Remove respiration measurements for samples with no associated metadata
  filter(!is.na(site))



########## Calculate flux rates based on formula by Dossa et al. 2015 ##########

# Temp conversion to kelvin: T = Tc + Ti; Ti = 273.15
# Pi (standard pressure) = 1013.25 mbar 
# Vi (standard volume) = 22.4 L
# collar = 5 or 9 cm, offset = 1 cm, diameter collar = 4.5 cm, volume chamber = 4076.1 cm^3
# Vc (volume of chamber) = (((collar-1)*4.5^2*pi) + 4076.1)/1000 
# MCO2 (molar mass CO2) = 44.01
# MCH4 (molar mass CH4) = 16.04

CO2_resp<-function(deltaCO2,Tc,Vs,Ws,P,collar){ 
  Pi=1013.25
  Vc=(((collar-1)*4.5^2*pi) + 4076.1)/1000 
  Vi=22.4
  Ti=273.15
  MCO2=44.01
  resp <- deltaCO2*MCO2*(P/Pi)*((Vc-Vs)/Vi)*(Ti/(Ti+Tc))*(1/Ws)
  return(resp)
}

CH4_resp<-function(deltaCH4,Tc,Vs,Ws,P,collar){ 
  Pi=1013.25
  Vc=(((collar-1)*4.5^2*pi) + 4076.1)/1000 
  Vi=22.4
  Ti=273.15
  MCH4=16.04
  resp <- deltaCH4*MCH4*(P/Pi)*((Vc-Vs)/Vi)*(Ti/(Ti+Tc))*(1/Ws)
  return(resp)
}

# Calculate flux and fuel moisture content
resp_out <- resp_all %>%
  mutate(CO2_resp_rate=CO2_resp(deltaCO2=deltaCO2,Tc=mean_Tcham,Vs=fresh_volume,
                                Ws=resp_dry_wt,P=mean_Pressure,collar=Collar),
         CH4_resp_rate=CH4_resp(deltaCH4=deltaCH4,Tc=mean_Tcham,Vs=fresh_volume,
                                Ws=resp_dry_wt,P=mean_Pressure,collar=Collar))

# Save data
write_csv(resp_out,"data/processed/wood_respiration/wood_respiration_rates.csv")



########## Additional processing of pine and natives for climate-flux model ##########

# Select pine blocks from pine gradient experiment and natives
resp_CF <- resp_out %>%
  filter(site%in%c("DRO","MLRF","MLES","STCK","PNW")) %>%
  filter(!SampleID%in%c("91-15","91-25","91-35","374-block","288-25","251-25","251-35"))

# Mark blocks that are less than 50% wood and have insignificant linear models
resp_CF_o <- resp_CF %>% 
  mutate(CO2_resp_outlier = ifelse(wood_por<=0.5,"low wood %",
                               ifelse(CO2.p>.05,"nonsignificant fit","No")),
         CH4_resp_outlier = ifelse(wood_por<=0.5,"low wood %",
                                   ifelse(CH4.p>.05,"nonsignificant fit","No")))

# Merge with linear fit data and check graphs
resp_CF_o2 <- resp_c3 %>%
  left_join(resp_CF_o,by="SampleID") %>%
  filter(!is.na(Species.Code)) %>%
  select(site,SampleID,Species.Code,Etime,CO2d_ppm,CH4d_ppm,CO2_resp_outlier,CH4_resp_outlier)
resp_pine_o2 <- filter(resp_CF_o2,Species.Code=="PIRA")
resp_native_o2 <- filter(resp_CF_o2,Species.Code!="PIRA")

# Save IRGA data plots for pine CO2
seq2 <- 1:6
pdf("data/processed/wood_respiration/pine_CO2_IRGA.pdf",width=11,height=8.5)
for(x in 1:length(seq2)){
  plot <- ggplot(resp_pine_o2,aes(Etime,CO2d_ppm,color=CO2_resp_outlier)) + geom_point(size=0.7) + 
    facet_wrap_paginate(SampleID~.,nrow=8,ncol=8,scales="free",page=seq2[x])
  print(plot)
}
dev.off()

# Save IRGA data plots for pine CH4
pdf("data/processed/wood_respiration/pine_CH4_IRGA.pdf",width=11,height=8.5)
for(x in 1:length(seq2)){
  plot <- ggplot(resp_pine_o2,aes(Etime,CH4d_ppm,color=CH4_resp_outlier)) + geom_point(size=0.7) + 
    facet_wrap_paginate(SampleID~.,nrow=8,ncol=8,scales="free",page=seq2[x])
  print(plot)
}
dev.off()

# Save IRGA data plots for native CO2
seq3 <- 1:10
pdf("data/processed/wood_respiration/native_CO2_IRGA.pdf",width=11,height=8.5)
for(x in 1:length(seq3)){
  plot <- ggplot(resp_native_o2,aes(Etime,CO2d_ppm,color=CO2_resp_outlier)) + geom_point(size=0.7) + 
    facet_wrap_paginate(SampleID~.,nrow=8,ncol=8,scales="free",page=seq3[x])
  print(plot)
}
dev.off()

# Save IRGA data plots for native CH4
pdf("data/processed/wood_respiration/native_CH4_IRGA.pdf",width=11,height=8.5)
for(x in 1:length(seq3)){
  plot <- ggplot(resp_native_o2,aes(Etime,CH4d_ppm,color=CH4_resp_outlier)) + geom_point(size=0.7) + 
    facet_wrap_paginate(SampleID~.,nrow=8,ncol=8,scales="free",page=seq3[x])
  print(plot)
}
dev.off()

# Calculate FMC
resp_CF_o3 <- resp_CF_o %>%
  mutate(FMC = ((FW_Wood-harvest_dry_wt)/harvest_dry_wt)*100)
# Negative FMC values are likely 0. They will be removed from analysis.

# Save final files
resp_pine_CO2 <- resp_CF_o3 %>%
  filter(Species.Code=="PIRA") %>%
  filter(CO2_resp_outlier == "No") %>%
  filter(FMC > 0) %>%
  filter(!is.na(CO2_resp_rate)) %>%
  write_csv("data/processed/wood_respiration/pine_CO2_clean.csv")
  
resp_pine_CH4 <- resp_CF_o3 %>%
  filter(Species.Code=="PIRA") %>%
  filter(CH4_resp_outlier == "No") %>%
  filter(FMC > 0) %>%
  filter(!is.na(CH4_resp_rate)) %>%
  write_csv("data/processed/wood_respiration/pine_CH4_clean.csv")

resp_native_CO2 <- resp_CF_o3 %>%
  filter(Species.Code!="PIRA") %>%
  filter(CO2_resp_outlier == "No") %>%
  filter(FMC > 0) %>%
  filter(!is.na(CO2_resp_rate)) %>%
  write_csv("data/processed/wood_respiration/native_CO2_clean.csv")

resp_native_CH4 <- resp_CF_o3 %>%
  filter(Species.Code!="PIRA") %>%
  filter(CH4_resp_outlier == "No") %>%
  filter(FMC > 0) %>%
  filter(!is.na(CH4_resp_rate)) %>%
  write_csv("data/processed/wood_respiration/native_CH4_clean.csv")


