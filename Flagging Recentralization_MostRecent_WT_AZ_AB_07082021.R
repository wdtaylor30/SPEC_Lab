############################################
# Identifying Recentralization
# Dataset: IDC_analysis_master_GM_20210608.rds
# This Dataset is the most recently merged dataset according to Ashley
# at the time this particular script was created, which is July 8, 2021.
# Recentralization: 1 to 0
# Updated By: Will Taylor, Annie Zhou, Apurvi Bhartia, July 8, 2021
# Created By: Jasmine Chu, May 30, 2020
# Last edited: July 8, 2021
##############################################

rm(list=ls())

library(ggplot2)
library(dplyr)
library(scales)

# load the most recently merged dataset, 'merged'
setwd("/Volumes/GoogleDrive/My Drive/Federalism/Federalism article/data/")
merged = readRDS("IDC_analysis_master_GM_20210608.rds")

idc_controls_r <- merged %>%
  subset(select = c(country, gwno, year, subed_IDC, subpolice_IDC, subtax_IDC))

# filter data to only have values 0 and 1
idc_controls_r <- filter(idc_controls_r, subed_IDC >=0) %>%
  dplyr::filter(subpolice_IDC >=0) %>%
  dplyr::filter(subtax_IDC >=0)

# lag variables by 1
subnat_r <- idc_controls_r %>%
  group_by(country) %>%
  mutate(subed_lag = dplyr::lag(subed_IDC),
         subpolice_lag = dplyr::lag(subpolice_IDC),
         subtax_lag = dplyr::lag(subtax_IDC)) %>%
  filter(year > 1975)

#################################
# 1. subed lag
subed_changes_r <- subnat_r %>%
  select(country, year, subed_IDC, subed_lag)

# recentralization for subed variable; lag is 1, variable is 0
subed_recentral_r <- subed_changes_r %>%
  filter(subed_IDC == 0 & subed_lag == 1)

subed_r_count_r <- nrow(subed_recentral_r) #6 cases of recentralization

#################################

# 2. subtax lag
subtax_changes_r <- subnat_r %>%
  select(country, year, subtax_IDC, subtax_lag)

# recentralization for subtax variable; lag is 1, variable is 0
subtax_recentral_r <- subtax_changes_r %>%
  filter(subtax_IDC == 0 & subtax_lag == 1)

subtax_r_count_r <- nrow(subtax_recentral_r) #6 cases of recentralization

#################################
# 3. subpolice lag
subpo_changes_r <- subnat_r %>%
  select(country, year, subpolice_IDC, subpolice_lag)

# recentralization for subpolice variable; lag is 1, variable is 0
subpo_recentral_r <- subpo_changes_r %>%
  filter(subpolice_IDC == 0 & subpolice_lag == 1)

subpo_r_count_r <- nrow(subpo_recentral_r) #5 cases of recentralization

#################################

# change variable names to indicate as recent (_r)
subed_recentral_r <- subed_recentral_r %>%
  rename(subed_IDC_r = subed_IDC, subed_lag_r = subed_lag)

subpo_recentral_r <- subpo_recentral_r %>%
  rename(subpolice_IDC_r = subpolice_IDC, subpolice_lag_r = subpolice_lag)

subtax_recentral_r <- subtax_recentral_r %>%
  rename(subtax_IDC_r = subtax_IDC, subtax_lag_r = subtax_lag)

# merging originally identified recentralization cases with the ones from the recentrly merged dataset
# as of 07.08.2021 the most recent version of the dataset is already merged
# thus, commented out the following three lines
# subed_comp <- merge(subed_recentral, subed_recentral_r, all = TRUE)
# subpo_comp <- merge(subpo_recentral, subpo_recentral_r, all = TRUE)
# subtax_comp <- merge(subtax_recentral, subtax_recentral_r, all = TRUE)

# setwd("/Users/SarahLim/Desktop/USC/EC/SPEC LAB/Senior/Variations in Federalism")
save(subed_recentral_r, file="Flagging_Recentralization_Comparison_subed_AZ_WT_AB_07082021.RDATA")
save(subpo_recentral_r, file="Flagging_Recentralization_Comparison_subpo_AZ_WT_AB_07082021.RDATA")
save(subtax_recentral_r, file="Flagging_Recentralization_Comparison_subtax_AZ_WT_AB_07082021.RDATA")
