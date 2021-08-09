# ---- Header ----
# CivicPulse Demographic Table
# Generating a table of unweighted and weighted counts and percentages of
# demographic distribution.
# To be placed in the appendix of the Summer 2021 Final Report.
# Created by: William Taylor
# Last edited: 7.19.21

# TODO: rounding, beautification, adding categories
# (region, perhaps voteshare or other interesting restricted data demographics)

# ---- 0.) Setup ----
rm(list = ls())
library(dplyr)
library(tidyr)
library(weights) # to calculate weighted percentages
library(kableExtra) # export table to png

# load data
# using the most recent prepped dataset (as of 7.18.21)
setwd("/Volumes/GoogleDrive/My Drive/SPEC LEWIS Registry Workproduct/CivicPulse/")
cp = readRDS("data/CivicPulse-PreppedData.RDS")

# ---- 1.) Craete Table as Dataframe ----
# we'll create a dataframe from scratch to hold the counts for each demographic
# option, according to the CP Reference Guide
gov_types = c("County", "Municipality", "Township")

ages = c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65",
         "66-70", "71-75")

educ = c("High school graduate", "Technical/trade school",
         "Some college", "College graduate", "Some graduate school", "Graduate degree")

# TODO: with the restricted data we might be able to make this more granular
# ask Maya
race = c("Non-Hispanic White", "Other")

gender = c("Male", "Female")

# TODO: come back to this
# add region info to cp dataset
# region = c("South", "West", "Northeast", "North Central")
# cp = cp %>% rename(state_abb = State_abb)

# not the most computationally efficient way...
# for (i in 1:length(cp)) {
#  cp$region[cp$state_abb == state.abb] = state.region
# }

dem_table = data.frame(row.names = c(gov_types, ages, educ, race, gender))

# TODO: add urban/rural, college_prop

# since counts and percentages must be filled in piecewise,
# initialize columns with zeros to start
dem_table$Unweighted_Count = 0
dem_table$Unweighted_Percent = 0
dem_table$Weighted_Percent = 0


# ---- 2.) Add Counts to Table ----
# have to manually index rows

# ---- 2a.) Gov. Type ----
dem_table$Unweighted_Count[1] = sum(cp$Gov_type == "County")
dem_table$Unweighted_Count[2] = sum(cp$Gov_type == "Municipality")
dem_table$Unweighted_Count[3] = sum(cp$Gov_type == "Township")

# ---- 2b.) Age ----
dem_table$Unweighted_Count[4] = sum(cp$Age == "31-35", na.rm = T)
dem_table$Unweighted_Count[5] = sum(cp$Age == "36-40", na.rm = T)
dem_table$Unweighted_Count[6] = sum(cp$Age == "41-45", na.rm = T)
dem_table$Unweighted_Count[7] = sum(cp$Age == "46-50", na.rm = T)
dem_table$Unweighted_Count[8] = sum(cp$Age == "51-55", na.rm = T)
dem_table$Unweighted_Count[9] = sum(cp$Age == "56-60", na.rm = T)
dem_table$Unweighted_Count[10] = sum(cp$Age == "61-65", na.rm = T)
dem_table$Unweighted_Count[11] = sum(cp$Age == "66-70", na.rm = T)
dem_table$Unweighted_Count[12] = sum(cp$Age == "71-75", na.rm = T)

# ---- 2c.) Education ----
dem_table$Unweighted_Count[13] = sum(cp$Education== "High school graduate", na.rm = T)
dem_table$Unweighted_Count[14] = sum(cp$Education == "Technical/trade school", na.rm = T)
dem_table$Unweighted_Count[15] = sum(cp$Education == "Some college", na.rm = T)
dem_table$Unweighted_Count[16] = sum(cp$Education == "College graduate", na.rm = T)
dem_table$Unweighted_Count[17] = sum(cp$Education == "Some graduate school", na.rm = T)
dem_table$Unweighted_Count[18] = sum(cp$Education == "Graduate degree", na.rm = T)

# ---- 2d.) Race ----
dem_table$Unweighted_Count[19] = sum(cp$NonHispanic_white == 1, na.rm = T)
dem_table$Unweighted_Count[20] = sum(cp$NonHispanic_white == 0, na.rm = T)

# ---- 2e.) Gender
dem_table$Unweighted_Count[21] = sum(cp$Gender == "Male", na.rm = T)
dem_table$Unweighted_Count[22] = sum(cp$Gender == "Female", na.rm = T)

# ---- 3.) Unweighted Percentages ----
# again, have to index one by one

# ---- 3a.) Gov. Type ----
dem_table$Unweighted_Percent[1] = sum(cp$Gov_type == "County") / nrow(cp)
dem_table$Unweighted_Percent[2] = sum(cp$Gov_type == "Municipality") / nrow(cp)
dem_table$Unweighted_Percent[3] = sum(cp$Gov_type == "Township") / nrow(cp)

# ---- 3b.) Age ----
dem_table$Unweighted_Percent[4] = sum(cp$Age == "31-35", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[5] = sum(cp$Age == "36-40", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[6] = sum(cp$Age == "41-45", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[7] = sum(cp$Age == "46-50", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[8] = sum(cp$Age == "51-55", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[9] = sum(cp$Age == "56-60", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[10] = sum(cp$Age == "61-65", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[11] = sum(cp$Age == "66-70", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[12] = sum(cp$Age == "71-75", na.rm = T) / nrow(cp)

# ---- 3c.) Education ----
dem_table$Unweighted_Percent[13] = sum(cp$Education== "High school graduate", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[14] = sum(cp$Education == "Technical/trade school", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[15] = sum(cp$Education == "Some college", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[16] = sum(cp$Education == "College graduate", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[17] = sum(cp$Education == "Some graduate school", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[18] = sum(cp$Education == "Graduate degree", na.rm = T) / nrow(cp)

# ---- 3d.) Race ----
dem_table$Unweighted_Percent[19] = sum(cp$NonHispanic_white == 1, na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[20] = sum(cp$NonHispanic_white == 0, na.rm = T) / nrow(cp)

# ---- 3e.) Gender
dem_table$Unweighted_Percent[21] = sum(cp$Gender == "Male", na.rm = T) / nrow(cp)
dem_table$Unweighted_Percent[22] = sum(cp$Gender == "Female", na.rm = T) / nrow(cp)

# ---- 4.) Weighted Percentages ----
# using Weight_1, which was calculated using dataset with of respondents who
# finished entire survey (see CivicPulse Reference Guide for details)

# using the wpct function from the weights package
# wpct returns a vector of percentages, so I have to index the return value
# to extract the percentage for each category

# ---- 4a.) Gov. Type ----
dem_table$Weighted_Percent[1] = wpct(cp$Gov_type, cp$Weight_1)[1]
dem_table$Weighted_Percent[2] = wpct(cp$Gov_type, cp$Weight_1)[2]
dem_table$Weighted_Percent[3] = wpct(cp$Gov_type, cp$Weight_1)[3]

# ---- 4b.) Age ----
dem_table$Weighted_Percent[4] = wpct(cp$Age, cp$Weight_1)[1]
dem_table$Weighted_Percent[5] = wpct(cp$Age, cp$Weight_1)[2]
dem_table$Weighted_Percent[6] = wpct(cp$Age, cp$Weight_1)[3]
dem_table$Weighted_Percent[7] = wpct(cp$Age, cp$Weight_1)[4]
dem_table$Weighted_Percent[8] = wpct(cp$Age, cp$Weight_1)[5]
dem_table$Weighted_Percent[9] = wpct(cp$Age, cp$Weight_1)[6]
dem_table$Weighted_Percent[10] = wpct(cp$Age, cp$Weight_1)[7]
dem_table$Weighted_Percent[11] = wpct(cp$Age, cp$Weight_1)[8]
dem_table$Weighted_Percent[12] = wpct(cp$Age, cp$Weight_1)[9]

# ---- 4c.) Education ----
dem_table$Weighted_Percent[13] = wpct(cp$Education, cp$Weight_1)[1]
dem_table$Weighted_Percent[14] = wpct(cp$Education, cp$Weight_1)[2]
dem_table$Weighted_Percent[15] = wpct(cp$Education, cp$Weight_1)[3]
dem_table$Weighted_Percent[16] = wpct(cp$Education, cp$Weight_1)[4]
dem_table$Weighted_Percent[17] = wpct(cp$Education, cp$Weight_1)[5]
dem_table$Weighted_Percent[18] = wpct(cp$Education, cp$Weight_1)[6]

# ---- 4d.) Race ----
dem_table$Weighted_Percent[19] = wpct(cp$Race, cp$Weight_1)[1]
dem_table$Weighted_Percent[20] = wpct(cp$Race, cp$Weight_1)[2]

# ---- 4e.) Gender ----
# female is the base level, so indices on wpct call are inverted intentionally
dem_table$Weighted_Percent[21] = wpct(cp$Gender, cp$Weight_1)[2]
dem_table$Weighted_Percent[22] = wpct(cp$Gender, cp$Weight_1)[1]

# ---- 5.) Save Table ----
# save to png
# requires phantomjs
# if you don't have it, you should get a warning asking you to run
# `webshot::install_phantomjs()`
dem_table %>% kbl() %>%
  kable_material("hover") %>%
  save_kable(file = "tables/CivicPulse_Demographic_Table_WT_071821.png",
             self_contained = T)


# ---- 6.) Mini Tables ----
# saving age and education dataframes for use in final report script
age_cleaned = data.frame(Age = ages, count = dem_table$Unweighted_Count[4:12],
                         percentage = dem_table$Unweighted_Percent[4:12])

educ_cleaned = data.frame(Education = educ, count = dem_table$Unweighted_Count[13:18],
                          percentage = dem_table$Unweighted_Percent[13:18])

# save dataframes as RDS objects in shared tables folder
saveRDS(age_cleaned, "tables/age_cleaned.RDS")
saveRDS(educ_cleaned, "tables/educ_cleaned.RDS")
