###################################################################
# Beyond Conquest France-Mali Citation
# Retroactive data analysis to support claims made on
# page 20 of the Beyond Conquest draft.
# This script specifically looks at French Exports from 2014-2020.
# Created by: William Taylor
# Last Edited: 06.24.2021
###################################################################

# clear environment
rm(list = ls())

# load libraries
library(readxl) # importing excel files
library(tidyverse) # tidyr for cleaning, dplyr for manipulation
library(ggplot2) # plotting

# set working directory for this project and load data
setwd("/Volumes/GoogleDrive/My Drive/PROJECT Beyond Conquest/Typology Paper/Data on France Exports to Mali (Citations) /")
exports = read_excel("Data/Exports_and_Imports_by_Areas_and_Co.xlsx")

# R read this literally as an Excel file, without formatting headers properly
# drop first few columns
exports = exports %>%
  drop_na()

# rename Exports column for ease of use
exports = exports %>%
  rename(Country = "Exports, FOB to Partner Countries")

# filter by only France's observations
exports_france = exports %>%
  filter(Country == "France")

# pivot to country-month
exports_france = exports_france %>%
  pivot_longer(...2:...77, names_to = "Months_after_Jan_2014")

# since R auto-named columns starting at 2, count of months should be n - 1
for (i in 1:nrow(exports_france)) {
  exports_france$Months_after_Jan_2014[i] = i - 1
}

# change months and exports to numeric
exports_france$Months_after_Jan_2014 = as.numeric(exports_france$Months_after_Jan_2014)
exports_france$value = as.numeric(exports_france$value)

# plot to see if we see an increase in the first place
# TODO: are they looking for net exports?
# or positive rate of change in exports (slope)?
ggplot(data = exports_france, aes(x = Months_after_Jan_2014, y = value,
                               group = 1)) +
  labs(title = "French Exports to Partner Countries by Month, 2014-2020",
       subtitle = "Source: International Monetary Fund") +
  xlab("Months after January 2014") +
  ylab("Exports, Millions of USD") +
  geom_line()

# save figure
# ggsave("Figures/French_Exports_2014_2020_WT_062321.png", width = 8, height = 8,
#        dpi = "retina")
