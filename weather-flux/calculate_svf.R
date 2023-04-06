# Calculate sky view factor

# Load packages and set working directory
setwd("weather-flux")
library(dplyr)
library(jpeg)
library(png)

# Read in files
svf_files <- list.files(path = "data/svf_pics",pattern = ".jpeg|.jpg",full.names = TRUE)

# Parameters
xth <- 0.6 # Threshold value in making binary images
r_ratio<-1 # RGB ratio
g_ratio<-1
b_ratio<-50

svf_out <- data.frame()
for (i in 1:length(svf_files)){
  # Extract file name
  fname <- gsub(pattern = "data/svf_pics/",svf_files[i],replacement = "") 
  fname0 <- gsub(pattern = ".jpeg|.jpg",fname,replacement = "") 
  # Read in file and get dimensions
  x <- readJPEG(svf_files[i])
  xdim <- nrow(x)
  ydim <- ncol(x)
  # Make equal-area binary image
  x_area <- (x[,,1]*r_ratio+x[,,2]*g_ratio+x[,,3]*b_ratio)/(r_ratio+g_ratio+b_ratio)
  x_area[x_area>=xth]<-1
  x_area[x_area<xth]<-0
  # Save binary equal-area images
  writePNG(x_area,paste0("data/processed/svf_pics/",fname0,"_area_bi.png"))
  # Calculate sky view factor from equal-area image
  svf <- sum(x_area)/(xdim*ydim)
  # Merge with dataframe
  temp <- data.frame(fname0,svf)
  svf_out <- rbind(svf_out,temp)
}

svf_avg <- svf_out %>%
  separate(fname0,c("site","location","number"),"_") %>%
  group_by(site,location) %>%
  summarise(mean_svf = mean(svf),
            sd_svf = sd(svf)) %>%
  rename(svf = mean_svf)

svf_final <- svf_avg %>%
  pivot_wider(names_from = location, values_from = c(svf,sd_svf)) %>%
  write_csv("data/processed/svf_pics/svf_calculations.csv")

svf_pine <- svf_final %>%
  select(site,svf_ws,svf_pine,sd_svf_pine) %>%
  write_csv("data/processed/svf_pics/svf_pine.csv")

