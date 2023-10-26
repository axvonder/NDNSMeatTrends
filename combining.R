library(dplyr)
library(tidyverse)
#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets (raw data from NDNS; access instructions in publication)
mod <- read.csv('final.csv')
yr1_4 <- read.csv('ndns_rp_yr1-4a_indiv_uk.csv')
yr5_6 <- read.csv('ndns_rp_yr5-6a_indiv.csv')
yr7_8 <- read.csv('ndns_rp_yr7-8a_indiv.csv')
yr9_11 <- read.csv('ndns_rp_yr9-11a_indiv_20211020.csv')
#keep desired variables
yr1_4 <- yr1_4[ , which(names(yr1_4) %in% c("seriali", "serialh", "area", "astrata5", "wti_UKY1234", "eqvinc"))]
yr5_6 <- yr5_6[ , which(names(yr5_6) %in% c("seriali", "serialh", "area", "astrata5", "wti_Y56", "eqvinc"))]
yr7_8 <- yr7_8[ , which(names(yr7_8) %in% c("seriali", "serialh", "area", "astrata5", "wti_Y78", "eqvinc"))]
yr9_11 <- yr9_11[ , which(names(yr9_11) %in% c("seriali", "serialh", "Area", "astrata5", "wti_Y911", "eqv3"))]
#turn yrs 1-7 income data into categorical (tertiles)
yr1_4 <- yr1_4 %>%
  mutate(eqvinc = ntile(ifelse(eqvinc == -1, NA, eqvinc), 3))
yr5_6 <- yr5_6 %>%
  mutate(eqvinc = ntile(ifelse(eqvinc == -1, NA, eqvinc), 3))
yr7_8 <- yr7_8 %>%
  mutate(eqvinc = ntile(ifelse(eqvinc == -1, NA, eqvinc), 3))
#change yr 9-11 -1 values to NAs to match the other datasets
yr9_11 <- yr9_11 %>%
  mutate(eqv3 = na_if(eqv3, -1))
#harmonize variable names
yr1_4 <- yr1_4 %>%
  rename(wti = wti_UKY1234) %>%
  rename(eqv = eqvinc)
yr5_6 <- yr5_6 %>%
  rename(wti = wti_Y56) %>%
  rename(eqv = eqvinc)
yr7_8 <- yr7_8 %>%
  rename(wti = wti_Y78) %>%
  rename(eqv = eqvinc)
yr9_11 <- yr9_11 %>%
  rename(wti = wti_Y911) %>%
  rename(eqv = eqv3) %>%
  rename(area = Area)
#merge datasets
allyr <- full_join(yr1_4,
                   full_join(yr5_6,
                             full_join(yr7_8, yr9_11)))
#combine with mod dataset
omega <- merge(mod, allyr, by = "seriali")
#write to copy
write.csv(omega,"/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data/omega.csv", row.names = F)
