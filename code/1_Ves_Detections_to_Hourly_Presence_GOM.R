# ---------------------------------------------------------
# This script is based on the one used to summarize vessel presence
# in the Van Parijs et al. (2023) Baseline paper
#  
# For each site included in the study, the Raven selection tables are
# loaded and concatenated, then summarized in a single data frame
# that summarizes hourly vessel presence and counts
# ---------------------------------------------------------


library(broom)
library(cli)
library(dplyr)
library(crayon)
library(dbplyr)
library(dtplyr)
library(forcats)
library(ggplot2)
library(googledrive)
library(googlesheets4)
library(hms)
library(httr)
library(jsonlite)
library(lubridate)
library(magrittr)
library(modelr)
library(pillar)
library(purrr)
library(readr)
library(readxl)
library(reprex)
library(rlang)
library(rstudioapi)
library(rvest)
library(stringr)
library(tibble)
library(tidyr)
library(xml2) 

library(tidyverse)

setwd("//nefscdata/PassiveAcoustics/DATA_ANALYSIS/PUBLICATIONS/MANUSCRIPTS/Holdman-et-al_TBD_GOM-baseline/Code/GOM_Vessel_Analysis_Scripts")
#setwd("C:/Users/rebecca.vanhoeck/Documents/GOM_vessels")

#### NEFSC GOM Data ####

#### source helper file w/functions (must be in "scripts" folder within wd)
source("GOM_ves_funs.R")

#### Load in OG dataset with all detections 
LUBEC_dets <- Compile_ves_detections(site_id = "LUBEC") 
MDR_dets <- Compile_ves_detections(site_id = "MDR")  
MONHEGAN_dets <- Compile_ves_detections(site_id = "MONHEGAN")
PORTLAND_dets <- Compile_ves_detections(site_id = "PORTLAND") 
YORK_dets <- Compile_ves_detections(site_id = "YORK") 
SB03_dets <- Compile_ves_detections(site_id = "SB03")
USTR01_dets <- Compile_ves_detections(site_id = "USTR01") 
USTR03_dets <- Compile_ves_detections(site_id = "USTR03") 
USTR11_dets <- Compile_ves_detections(site_id = "USTR11")


#### convert detections to hourly presence 

# manually set the first and last day of recording for the entire dataset used at that site
start_dep_date <- as_date("2020-05-14")
end_dep_date <- as_date("2023-06-17")
LUBEC_hp <- dets_to_hp(det_table = LUBEC_dets,  site_id = "LUBEC") 

start_dep_date <- as_date("2020-10-28")
end_dep_date <- as_date("2021-11-13")
MDR_hp <- dets_to_hp(det_table = MDR_dets,  site_id = "MDR")

start_dep_date <- as_date("2020-03-09")
end_dep_date <- as_date("2022-12-21")
MONHEGAN_hp <- dets_to_hp(det_table = MONHEGAN_dets,  site_id = "MONHEGAN")

start_dep_date <- as_date("2020-06-18")
end_dep_date <- as_date("2023-10-18")
PORTLAND_hp <- dets_to_hp(det_table = PORTLAND_dets,  site_id = "PORTLAND")

start_dep_date <- as_date("2020-09-12")
end_dep_date <- as_date("2023-10-11")
YORK_hp <- dets_to_hp(det_table = YORK_dets,  site_id = "YORK")

start_dep_date <- as_date("2020-04-07")
end_dep_date <- as_date("2023-12-08")
SB03_hp <- dets_to_hp(det_table = SB03_dets,  site_id = "SB03")

start_dep_date <- as_date("2022-05-20")
end_dep_date <- as_date("2023-09-12")
USTR01_hp <- dets_to_hp(det_table = USTR01_dets,  site_id = "USTR01")

start_dep_date <- as_date("2022-05-19")
end_dep_date <- as_date("2023-09-12")
USTR03_hp <- dets_to_hp(det_table = USTR03_dets,  site_id = "USTR03")

start_dep_date <- as_date("2022-05-19")
end_dep_date <- as_date("2023-09-12")
USTR11_hp <- dets_to_hp(det_table = USTR11_dets,  site_id = "USTR11")

#### coerce each deployment into a larger list
all_hp <- list(LUBEC_hp, MDR_hp, MONHEGAN_hp, PORTLAND_hp, YORK_hp, SB03_hp, USTR01_hp, USTR03_hp, USTR11_hp)

#### merge all deps into single data frame
allsites_hp <- do.call("rbind", all_hp)

# add column for hourly presence (not count)
hp_all = allsites_hp
hp_all$Ves_pres = case_when(hp_all$Ves_counts >= 1 ~ 1, 
                            hp_all$Ves_counts ==0 ~ 0)

#### Remove Data Gaps between deployments for NEFSC data
gaps_all = read.csv("data_inputs/GOM_baseline_datagaps.csv", header = TRUE)

#### filter gaps to only deployments included
gaps = gaps_all %>%
  filter(!is.na(gaps_all$DataInput))
# convert to date format
gaps$Gap_start = as.Date(gaps$Gap_start, format = "%m/%d/%Y")
gaps$Gap_end = as.Date(gaps$Gap_end, format = "%m/%d/%Y")
gaps$Gap_start = as.POSIXct(gaps$Gap_start, format = "%Y-%m-%d")
gaps$Gap_end = as.POSIXct(gaps$Gap_end, format = "%Y-%m-%d")


# Make a dataframe that contains a date sequence to cover all data gaps between deployments
allGaps1 <- gaps %>% 
  filter(!is.na(Gap_start)) %>% 
  group_by(Site, DeploymentCode) %>% 
  reframe(gap_date = seq(from=Gap_start, to=Gap_end, by='days')) %>% 
  ungroup() %>% 
  mutate(gap_date = format(gap_date, format='%Y-%m-%d')) %>% 
  distinct()

allGaps1$gap_date = as.Date(allGaps1$gap_date, format = '%Y-%m-%d')

# Join the data gap dataframe with the hourly presence data frame to identify gaps to be removed
hp_all <- left_join(
  hp_all,
  allGaps1,
  by=c('SITE'='Site', 'start_date_ISO'='gap_date'),
  relationship = 'many-to-one')
hp_all$off_effort <- !is.na(hp_all$DeploymentCode)
#table(hp_all$Ves_counts, hp_all$off_effort)

# Set all hours of data gaps/off_effort to NA
hp_all$Ves_pres[hp_all$off_effort] <- NA
hp_all$Ves_counts[hp_all$off_effort] <- NA
# remove this column because we dont need it 
hp_all$DeploymentCode <- NULL


#### Save Hourly Presence table into data_outputs folder

write.csv(hp_all, paste0("data_outputs/", "All_GOM_Vessel_Hourly_Presence_effortCorrected.csv"))


#### End NEFSC Data 





#### AEON GOM DATA ####

# Load data and metadata

AEON1a = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jan22_AEON1_AMAR511.1.16000_ch0__20220107_20230210__detection_levels.csv", header = TRUE)
AEON1b = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Feb23_AEON1-NEC_AMAR700.1.16000_ch0__20230213_20240220__detection_levels.csv", header = TRUE)

AEON2a = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jan22_AEON2_AMAR507.1.16000_ch0__20220106_20230210__detection_levels.csv", header = TRUE)
AEON2b = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Feb23_AEON2-ECS_AMAR705.1.16000_ch0__20230213_20240221__detection_levels.csv", header = TRUE)

AEON3a = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jul21_AEON3_AMAR495.1.16000_ch0__20210723_20220112__detection_levels.csv", header = TRUE)
AEON3b = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jan22_AEON3_AMAR509.1.16000_ch0__20220112_20230214__detection_levels.csv", header = TRUE)
AEON3c = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Feb23_AEON3-GEB_AMAR679.1.16000_ch0__20230213_20240222__detection_levels.csv", header = TRUE)

AEON4a = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Feb21_AEON4_AMAR509.2.16000_ch1__20210214_20210722__detection_levels.csv", header = TRUE)
AEON4b = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jul21_AEON4_AMAR679.1.16000_ch0__20210723_20220113__detection_levels.csv", header = TRUE)
AEON4c = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jan22_AEON4_AMAR504.1.16000_ch0__20220111_20221212__detection_levels.csv", header = TRUE)

AEON5a = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Feb21_AEON5_AMAR501.1.512000_ch0__20210215_20210720__detection_levels.csv", header = TRUE)
AEON5b = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jul21_AEON5_AMAR378.1.16000_ch0__20210723_20220108__detection_levels.csv", header = TRUE)
AEON5c = read.csv("data_inputs/AEON_data/AEON_data_for_NEFSC/Jan22_AEON5_AMAR510.1.16000_ch0__20220108_20220117__detection_levels.csv", header = TRUE)


# Compile single sheet with vessel columns

AEON1 = bind_rows(AEON1a, AEON1b)
AEON1 = AEON1 %>%
  select(Time, shipping.detection.flag, boating.detection.flag) %>%
  mutate(Site = "AEON1_NEC")
AEON1$Datetime = as.POSIXct(AEON1$Time, format = "%d-%b-%Y %H:%M")

AEON2 = bind_rows(AEON2a, AEON2b)
AEON2 = AEON2 %>%
  select(Time, shipping.detection.flag, boating.detection.flag) %>%
  mutate(Site = "AEON2_ECS")
AEON2$Datetime = as.POSIXct(AEON2$Time, format = "%d-%b-%Y %H:%M")

AEON3 = bind_rows(AEON3a, AEON3b, AEON3c)
AEON3 = AEON3 %>%
  select(Time, shipping.detection.flag, boating.detection.flag) %>%
  mutate(Site = "AEON3_GEB")
AEON3$Datetime = as.POSIXct(AEON3$Time, format = "%d-%b-%Y %H:%M")

AEON4 = bind_rows(AEON4a, AEON4b, AEON4c)
AEON4 = AEON4 %>%
  select(Time, shipping.detection.flag, boating.detection.flag) %>%
  mutate(Site = "AEON4_JOB")
AEON4$Datetime = as.POSIXct(AEON4$Time, format = "%d-%b-%Y %H:%M")

AEON5 = bind_rows(AEON5a, AEON5b, AEON5c)
AEON5 = AEON5 %>%
  select(Time, shipping.detection.flag, boating.detection.flag) %>%
  mutate(Site = "AEON5_WIB")
AEON5$Datetime = as.POSIXct(AEON5$Time, format = "%d-%b-%Y %H:%M")


AEONall = bind_rows(AEON1,AEON2,AEON3, AEON4, AEON5)
AEONall = AEONall %>%
  mutate(Date = date(Datetime), Hour = hour(Datetime))

# a few dates are reformatting correctly, can't find an obvious difference or error
# moving forward without them
dateCheck = AEONall[is.na(AEONall$Date),]


# Summarize hourly presence at each site

# vessel is considered present if ship or boating, or both have a detection 
AEONall$ves_pres = case_when(AEONall$shipping.detection.flag > 0 | AEONall$boating.detection.flag > 0 ~ 1, 
                             AEONall$shipping.detection.flag == 0 & AEONall$boating.detection.flag == 0 ~ 0)

AEON_hp = AEONall %>%
  group_by(Site, Date, Hour) %>%
  summarize(min_pres = sum(ves_pres), min_effort = n())
AEON_hp$ves_pres = case_when(AEON_hp$min_pres > 0 ~ 1, 
                             AEON_hp$min_pres ==0 ~ 0)


# Remove days that have incomplete effort
AEON_effort = AEON_hp %>%
  group_by(Site, Date) %>%
  summarize(hr_effort = n()) %>%
  filter(hr_effort < 24)

# Join the data gap dataframe with the hourly presence data frame to identify gaps to be removed
AEON_hp <- left_join(
  AEON_hp,
  AEON_effort,
  by=c('Site'='Site', 'Date'='Date'),
  relationship = 'many-to-one')
AEON_hp$off_effort <- !is.na(AEON_hp$hr_effort)

# Set all hours of data gaps/off_effort to NA
AEON_hp$min_pres[AEON_hp$off_effort] <- NA
AEON_hp$min_effort[AEON_hp$off_effort] <- NA
AEON_hp$ves_pres[AEON_hp$off_effort] <- NA
# remove this column because we dont need it 
AEON_hp$hr_effort <- NULL

# Write csv file of hourly presence

write.csv(AEON_hp, "data_outputs/All_AEON_vessel_hourly_presence.csv")







