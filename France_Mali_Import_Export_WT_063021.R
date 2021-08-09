###########################################################################
# Continuing to look for trends in France-Mali Imports and Exports.
# This time around, using UN Comtrade data from 2012-2013, 2015, 2017-2019.
# Created by: William Taylor
# Last edited: 7.1.21
###########################################################################

# we're looking at the wrong thing! TODO: look for trade surplus!
# Do France's exports to Mali outweigh it's imports from Mali?
# that answers one of our questions (reference your memo)

# clear environment
rm(list = ls())

# imports
library(dplyr) # manipulation
library(tidyr) # reshaping
library(ggplot2) # plotting
library(ggthemes) # themes
library(Hmisc) # labels

# prettier plots globally
theme_set(theme_bw())

# set working directory to typology project
setwd("/Volumes/GoogleDrive/My Drive/PROJECT Beyond Conquest/Typology Paper/Data on France Exports to Mali (Citations) /")

# load data
# 6 different .csv files, so we'll have to merge them
com_12 = read.csv("Data/Reporter-France: Partner- Mali/comtrade data set 2012 (1).csv")
com_13 = read.csv("Data/Reporter-France: Partner- Mali/comtrade data set 2013.csv")
com_15 = read.csv("Data/Reporter-France: Partner- Mali/comtrade data set 2015.csv")
com_17 = read.csv("Data/Reporter-France: Partner- Mali/comtrade data set 2017.csv")
com_18 = read.csv("Data/Reporter-France: Partner- Mali/comtrade data set 2018.csv")
com_19 = read.csv("Data/Reporter-France: Partner- Mali/comtrade data set 2019.csv")

# ---- Part 1: Manipulate Data ----
# we want to make this into one country-year dataframe...
# merge doesn't allow more than two df's at a time, so this is overly verbose
com_12_13 = full_join(com_12, com_13)
com_12_13_15 = full_join(com_12_13, com_15)
com_12_13_15_17 = full_join(com_12_13_15, com_17)
com_12_13_15_17_18 = full_join(com_12_13_15_17, com_18)
com_full = full_join(com_12_13_15_17_18, com_19)

# for readability, change Trade Value to hundreds, label as "Millions"
com_full$Trade.Value..US.. = com_full$Trade.Value..US.. / 1000000
com_full = com_full %>% rename("Trade.Value" = Trade.Value..US..)
label(com_full$Trade.Value) = "Millions of USD"

# drop all columns that other than Year, Trade.Flow, and Trade.Value
com_full = com_full %>% select(c(Year, Trade.Flow, Trade.Value))

# the easiest way is two separate dataframes based on import-export
# remember, France is the Reporter, Mali is the Partner
france_import = com_full %>%
  subset(com_full$Trade.Flow == "Import") %>%
  arrange(Year)

france_export = com_full %>%
  subset(com_full$Trade.Flow == "Export") %>%
  arrange(Year)

# ---- Part 2: Plot ----
# plot imports
export_line = ggplot(france_export, aes(x = Year, y = Trade.Value)) +
  labs(title = "French Exports to Mali, 2012-2019",
       subtitle = "Source: UN Comtrade Data") +
  ylab("Trade Value (USD)") +
  geom_line()

import_line = ggplot(france_import, aes(x = Year, y = Trade.Value)) +
  labs(title = "French Imports from Mali, 2012-2019",
       subtitle = "Source: UN Comtrade Data") +
  ylab("Trade Value (USD, Hundreds of Millions)") +
  geom_line()

# balance of trade: this is likely the best assertion to prove the author's claim
# looking for French trade surplus, exports > imports over time
# looks like that's the case, unequivocally
balance_area = ggplot(com_full, aes(x = Year, y = Trade.Value,
                                    fill = Trade.Flow)) +
  labs(title = "France Experienced a Trade Surplus in its\nRelationship with Mali",
       subtitle = "Source: UN Comtrade Data, 2012-2019", fill = "Direction") +
  ylim(0, 500) +
  ylab("Trade Value (Millions of USD)") +
  geom_area(alpha = .6) +
  scale_fill_manual(values = c("red", "darkgreen"))

balance_area

# ---- Part 3: Saving ----
# ggsave("Figures/French_Exports_to_Mali_2012-2019_WT_063021.png", export_line,
#       width = 8, height = 8, dpi = "retina")
# ggsave("Figures/French_Imports_to_Mali_2012-2019_WT_063021.png", import_line,
#       width = 8, height = 8, dpi = "retina")
# ggsave("Figures/Balance_of_Trade_2012-2019_WT_070121.png", balance_area,
#       width = 8, height = 8, dpi = "retina")

