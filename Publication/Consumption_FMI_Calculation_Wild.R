#Kayla Blincow
#9/15/2024

#The purpose of this script is to combine the Import FMI information with the 
#FAO data and calculate the consumption FMI with the wild capture data

#Goal: 
#1. Original Proportional Estimates (already calculated)
#2. Gephart Paper ratio for all countries
#3. Guillen Paper for all countries


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)

#load data
FAO <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)
#convert FAO year to actual year
FAO$Year <- sub(".", "", FAO$Year) %>% as.numeric()

#50/50 assumption
#impFMI <- read.csv("ConsumptionAngle/Import_FMI.csv", header = T)

#proportional assumption
impFMI <- read.csv("ConsumptionAngle/Import_FMI_prop_Wild.csv", header = T)


#remove X columns
FAO <- select(FAO, !X)
impFMI <- select(impFMI, !X)


#combine data
d <- left_join(FAO, impFMI, by = c("Country" = "Reporter",
                                   "ProductType" = "FAOGroup",
                                   "Year" = "Year"))



#remove countries that have NA Import FMIs (i.e. countries that didn't report to GTA dataset)
NA_countries <-  unique(d[is.na(d$FMI_Imp1),]$Country)

d <- filter(d, !Country %in% NA_countries)
d <- filter(d, FoodSupply != 0)

#calculate consumption FMI (Proportional)
d$FMI_Cons1 <- (d$p_prod*d$FMI) + (d$p_imp*d$FMI_Imp1)

#calculate Consumption FMI (Guillen et al. 2010)
d$p_prod2 <- 0.74
d$p_imp2 <- 0.26

d$FMI_Cons2 <- (d$p_prod2*d$FMI) + (d$p_imp2*d$FMI_Imp1)

#calculate Consumption FMI (Gephart Scale) (not this... this doesn't make sense)
d$p_prod3 <- (d$p_prod*-.294661) + d$p_prod
#OLD d$p_imp3 <- (d$p_imp*0.316012) + d$p_imp  
d$p_imp3 <- 1 - d$p_prod3

#just change proportion to 0.365 for everyone? YES! this..
d$p_prod3a <- 0.365
d$p_imp3a <- 0.665


d$FMI_Cons3 <- (d$p_prod3*d$FMI) + (d$p_imp3*d$FMI_Imp1)
d$FMI_Cons3a <- (d$p_prod3a*d$FMI) + (d$p_imp3a*d$FMI_Imp1) #this


#aggregate across product type and year by country
C_country <- d %>% group_by(Country) %>% 
  summarize(totalC = sum(FoodSupply),
            totalI = sum(Imports))

d2 <- left_join(d, C_country) %>% 
  mutate(propC = FoodSupply/totalC,
         FMIc_sc1 = FMI_Cons1*propC,
         FMIc_sc2 = FMI_Cons2*propC,
         FMIc_sc3 = FMI_Cons3*propC,
         FMIc_sc3a = FMI_Cons3a*propC,
         propI = Imports/totalI,
         FMIi_sc = FMI_Imp1 * propI
  ) %>% 
  group_by(Country) %>% 
  summarize(FMIc1 = sum(FMIc_sc1),
            FMIc2 = sum(FMIc_sc2),
            FMIc3 = sum(FMIc_sc3),
            FMIc3_flp = sum(FMIc_sc3a),
            FMIi = sum(FMIi_sc),
            FMIp = first(FMI),
            Exports = sum(Exports),
            Imports = sum(Imports),
            Production = sum(Production),
            FoodSupply = sum(FoodSupply))


#save final dataset for visualization/analysis
#50/50 assumption
#write.csv(d2, "ConsumptionAngle/Final_FAOFMI.csv")
#proportional assumption
write.csv(d2, "ConsumptionAngle/Final_FAOFMI_prop_Wild.csv")
