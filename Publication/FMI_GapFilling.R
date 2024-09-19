#Kayla Blincow
#7/13/2021


#The purpose of this script is to expand the OHI gapfilling to inlcude reporters
#and partners not included in the OHI data

#code adapted from Ocean Health Index. Year. ohiprep version: Preparation of data for global scenarios of the Ocean Health Index, [date downloaded]. National Center for Ecological Analysis and Synthesis, University of California, Santa Barbara. Available at: https://github.com/OHI-Science/ohiprep/releases

#clear my workspace 
rm(list = ls())

#load my packages
library(tidyverse)
library(WDI)
library(here)
library(devtools)
devtools::install_github("ohi-science/ohicore@dev")
library(ohicore)
source('https://raw.githubusercontent.com/OHI-Science/ohiprep_v2019/gh-pages/workflow/R/common.R')


#load my data

#GTA data
GTA <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)
GTA <- filter(GTA, Direction == "Import")

#create a list of unique trade reporters and partners from this dataset
traders <- c(unique(GTA$Reporter), unique(GTA$Partner)) %>% unique() %>% 
  as.data.frame()
colnames(traders) <- "Country"

#Current FMI data
FMI <- read.csv("DataExports/FMI/fmi_res.csv", header = T)

#combine with GTA traders list
d <- full_join(FMI, traders) %>% distinct()

#OHI region ID matching
regionid <- read.csv("DataExports/FMI/OHIRegnID.csv", header = T)

d2 <- left_join(d, regionid, by = c("Country" = "region_name")) %>% 
  select(-region_id)
d2$rgn_id <- as.integer(d2$rgn_id)

#load model variable data
spi <- read.csv("DataExports/FMI/spi_res.csv", header = T)

spi <- filter(spi, year == 2018)

d3 <- left_join(d2, spi)

UNgeorgn()
georegions <- UNgeorgn %>%
  select(rgn_id, rgn_name = rgn_label, r0_label, r1_label, r2_label)

d4 <- left_join(d3, georegions) %>% 
  rename(spi = resilience_score)

#figure out what's up with the NA values...
d4[is.na(d4$rgn_id),]

#write.csv(d4, "DataExports/FMI/FullFMIGapFill.csv")

#filling in the gaps of the SPI and FMI and documenting in supplementary material in the csv

d <- read.csv("DataExports/FMI/FullFMIGapFill.csv", header = T)

#need to use the OHI equation to fill out FMI for countries that don't have it

mod_r2 <- lm(FMI ~ r2_label + spi, data=d)
## have to do the predict in a more complicated fashion because some r2 categories have no data, this returns an NA for these
d$fmi_pred_r2 <- 
  sapply(1:nrow(d), 
         function(i) 
           tryCatch(predict(mod_r2, d[i,]), 
                    error=function(e) NA))

# get predictions for the regions not represented by r2 regions:
mod_r1 <- lm(FMI ~ r1_label + spi, data=d)
## have to do the predict in a more complicated fashion because some r2 categories have no data, this returns an NA for these
d$fmi_pred_r1 <- 
  sapply(1:nrow(d), 
         function(i) 
           tryCatch(predict(mod_r1, d[i,]), 
                    error=function(e) NA))

# final data and gapfilling recordkeeping

fmi_gf_all <- d %>%
  dplyr::mutate(gapfilled = ifelse(is.na(FMI), "1", 0)) %>%
  dplyr::mutate(method = ifelse(is.na(FMI) & !is.na(fmi_pred_r2), 
                                "SPI + UN_geopolitical region r2", NA)) %>%
  dplyr::mutate(method = ifelse(is.na(FMI) & is.na(fmi_pred_r2), 
                                "SPI + UN_geopolitical region r1" , method)) %>%
  dplyr::mutate(fmi2 = ifelse(is.na(FMI), fmi_pred_r2, FMI)) %>% 
  dplyr::mutate(fmi2 = ifelse(is.na(fmi2), fmi_pred_r1, fmi2)) %>%
  dplyr::mutate(FMI = fmi2) %>%
  dplyr::select(-fmi2)

write.csv(fmi_gf_all, "ConsumptionAngle/FMI_GTAMatch.csv")
