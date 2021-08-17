#Kayla Blincow
#7/7/2021

#The purpose of this script is to calculate import FMI values accounting for re-exports
#UPDATE (7/16/2021):
#explore how countries FMIc changes if we 
#adjust the ratio of production to imports in consumption.

#Goal: 
#1. Original Proportional Estimates (already calculated)
#2. No re-exports
#3. Scalar adjustment to meet P:I ratio in Gephart paper


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(learnr) #for positive reinforcement

#load my data
GTA <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)
FAO <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)
FMI <- read.csv("ConsumptionAngle/FMI_GTAMatch.csv", header = T)

#can skip to line 117

#combine GTA data based on FAO product groupings and widen so have a column
#for exports and imports
GTA <- GTA %>% 
  pivot_wider(names_from = Direction,
              values_from = c(Value, livewgt)) %>% 
  group_by(Reporter, Partner, Year, FAOGroup) %>% 
  summarize(Import_Value = sum(Value_Import, na.rm = T),
            Import_Quantity = sum(livewgt_Import, na.rm = T),
            Export_Value = sum(Value_Export, na.rm = T),
            Export_Quantity = sum(livewgt_Export, na.rm = T)) %>% 
  filter(Partner != "") %>% #Panama apparently trades with no one? removing this data
  filter(Import_Quantity > 0) #don't care if you aren't importing from the partner


#convert FAO year to actual year
FAO$Year <- sub(".", "", FAO$Year) %>% as.numeric()


#Simplify FMI data
FMIs <- FMI %>% 
  select(Country, FMI)

#use GTA data to calculate Import FMI accounting for re-exports
GTA <- left_join(GTA, FMIs, by = c("Partner" = "Country")) %>% 
  rename(FMI_part = FMI)

#first need to find out the proportion of production associated with each partner/product
prod_pr <- dplyr::select(FAO, Country, ProductType, Year, p_prod) %>% distinct()

GTA <- left_join(GTA, prod_pr, by = c("Partner" = "Country", 
                                "FAOGroup" = "ProductType",
                                "Year" = "Year")) %>% 
  rename(pprod_Part = p_prod) 


#For partners that have NA proportion of production:
#if it's a partner/product group that's representated in FAO data but w/ year mismatch
#<- mean(p_prod) for that country/product group
FAO2 <- FAO %>% group_by(Country, ProductType) %>% 
  summarize(p_prod_m = mean(p_prod))

prod_pr2 <- dplyr::select(FAO2, Country, ProductType, p_prod_m) %>% distinct()

GTA <- left_join(GTA, prod_pr2, by = c("Partner" = "Country",
                                       "FAOGroup" = "ProductType")) %>% 
  rename(pprod_Part2 = p_prod_m)

GTA[is.na(GTA$pprod_Part) & !is.na(GTA$pprod_Part2),]$pprod_Part <- 
  GTA[is.na(GTA$pprod_Part) & !is.na(GTA$pprod_Part2),]$pprod_Part2 

#if it's a partner that's represented in FAO data but w/ product type mismatch
#<- mean(p_rpod) for that coarser product group
#i.e. mean(pelagic fish, demersal fish) for marine fish nei
FAO$GroupCoarse <- NA
FAO[FAO$ProductType == "Demersal fish" | FAO$ProductType == "Pelagic fish" |
      FAO$ProductType == "Marine fish nei" |
      FAO$ProductType == "Freshwater & diadromous fish",]$GroupCoarse <- "Fish"
FAO[FAO$ProductType == "Aquatic animals nei" | FAO$ProductType == "Cephalopods" | 
      FAO$ProductType == "Crustaceans" | 
      FAO$ProductType == "Molluscs excl. cephalopods",]$GroupCoarse <- "Invert"  

GTA$GroupCoarse <- "word"
GTA[GTA$FAOGroup == "Demersal fish" | GTA$FAOGroup == "Pelagic fish" |
      GTA$FAOGroup == "Marine fish nei" |
      GTA$FAOGroup == "Freshwater & diadromous fish",]$GroupCoarse <- "Fish"
GTA[GTA$FAOGroup == "Aquatic animals nei" | GTA$FAOGroup == "Cephalopods" | 
      GTA$FAOGroup == "Crustaceans" | 
      GTA$FAOGroup == "Molluscs excl. cephalopods",]$GroupCoarse <- "Invert"  



FAO3 <- FAO %>% group_by(Country, GroupCoarse) %>% 
  summarize(p_prod_m2 = mean(p_prod))


GTA <- left_join(GTA, FAO3, by = c("Partner" = "Country",
                                       "GroupCoarse" = "GroupCoarse")) %>% 
  rename(pprod_Part3 = p_prod_m2)

GTA[is.na(GTA$pprod_Part) & !is.na(GTA$pprod_Part3),]$pprod_Part <- 
  GTA[is.na(GTA$pprod_Part) & !is.na(GTA$pprod_Part3),]$pprod_Part3


#the rest are weird groups of countries or special cases from single countries
#exporting to a csv to manually input production estimates, because it's easier
#assigning proportion of production based on regional mean for each unaccounted
#for partner (see supplement)
#write.csv(GTA, "ConsumptionAngle/IntermediateFMIiCalc.csv")

GTA2 <- read.csv("ConsumptionAngle/IntermediateFMIiCalc.csv", header = T)

#excluding data where the reporter and partner are the same
GTA2 <- filter(GTA2, Reporter != Partner)


#calculate total imports for each Reporter, FAOGroup, Year
totals <- GTA2 %>% group_by(Reporter, FAOGroup, Year) %>% 
  summarize(TotalI = sum(Import_Quantity))

#calculate the proportion of imports for that product/year that are associated with a given partner
GTA2 <- left_join(GTA2, totals) %>% 
  mutate(propI = Import_Quantity/TotalI)


#calculate global weighted average FMI based on production
FMIg <- FAO %>% group_by(ProductType) %>% 
  mutate(TotalP = sum(Production)) %>% 
  group_by(Country, ProductType) %>% 
  summarize(CountryP = sum(Production),
            TotalP = first(TotalP),
            FMI = first(FMI)) %>% 
  ungroup() %>% 
  mutate(FMIP = (CountryP/TotalP)*FMI)


FMIglbl <- FMIg %>% group_by(ProductType) %>% 
  summarize(FMIglbl = sum(FMIP))

#calculate FMI for imports based on the proportion of a partners exports associated
#with their production, and the proportion of total imports associated with a 
#given partner, and the FMI of that partner plus the porportion of partner's exports
#associated with their imports, and the proprotion of total imports associated with 
#a given partner, and the global FMI average
GTA3 <- left_join(GTA2, FMIglbl, by = c("FAOGroup" = "ProductType"))

GTA3$FMIi <- (0.5*GTA3$propI*GTA3$FMI_part) + 
     (0.5*GTA3$propI*GTA3$FMIglbl)

#OLD GTA3$FMIi <- (GTA3$pprod_Part*GTA3$propI*GTA3$FMI_part) + 
#   ((1-GTA3$pprod_Part)*GTA3$propI*GTA3$FMIglbl)


#compile them all
FMIi <- GTA3 %>% group_by(Reporter, Year, FAOGroup) %>% 
  summarize(FMI_Imp1 = sum(FMIi, na.rm = T))

write.csv(FMIi, "ConsumptionAngle/Import_FMI.csv")

