#Kayla Blincow
#7/7/2021

#The purpose of this script is to calculate import FMI values accounting for re-exports
#UPDATE (8/16/2021):
#Originally decided for a 50:50 ratio of imports v production for trade partners
#removes the need for all the p_prod code/assumptions
#UPDATE (11/12/2021):
#Going back to the proportional method of imports v production for trade partners


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(learnr) #for positive reinforcement

#load my data
GTA <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)
FAO <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)
FMI <- read.csv("ConsumptionAngle/FMI_GTAMatch.csv", header = T)


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

GTA1 <- filter(GTA, !is.na(pprod_Part))

#For partners that have NA proportion of production:
#if it's a partner/product group that's represented in FAO data but w/ year mismatch
#<- mean(p_prod) for that country/product group
FAO2 <- FAO %>% group_by(Country, ProductType) %>% 
  summarize(p_prod_m = mean(p_prod))

prod_pr2 <- dplyr::select(FAO2, Country, ProductType, p_prod_m) %>% distinct()

GTA2i <- filter(GTA, is.na(pprod_Part))

GTA2i <- left_join(GTA2i, prod_pr2, by = c("Partner" = "Country",
                                       "FAOGroup" = "ProductType")) %>% 
  select(-pprod_Part) %>% 
  rename(pprod_Part = p_prod_m)

GTA2 <- filter(GTA2i, !is.na(pprod_Part))

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

GTA3i <- filter(GTA2i, is.na(pprod_Part))

GTA3i$GroupCoarse <- "word"
GTA3i[GTA3i$FAOGroup == "Demersal fish" | GTA3i$FAOGroup == "Pelagic fish" |
      GTA3i$FAOGroup == "Marine fish nei" |
      GTA3i$FAOGroup == "Freshwater & diadromous fish",]$GroupCoarse <- "Fish"
GTA3i[GTA3i$FAOGroup == "Aquatic animals nei" | GTA3i$FAOGroup == "Cephalopods" | 
      GTA3i$FAOGroup == "Crustaceans" | 
      GTA3i$FAOGroup == "Molluscs excl. cephalopods",]$GroupCoarse <- "Invert"  



FAO3 <- FAO %>% group_by(Country, GroupCoarse) %>% 
  summarize(p_prod_m2 = mean(p_prod))


GTA3i <- left_join(GTA3i, FAO3, by = c("Partner" = "Country",
                                       "GroupCoarse" = "GroupCoarse")) %>% 
  select(-pprod_Part, -GroupCoarse) %>% 
  rename(pprod_Part = p_prod_m2)

GTA3 <- filter(GTA3i, !is.na(pprod_Part))

#the rest are weird groups of countries or special cases from single countries
#assigning proportion of production based on regional mean for each unaccounted
#for partner (see supplement)
regions <- read.csv("ConsumptionAngle/CountryRegions.csv")
FAOr <- left_join(FAO, regions, by = c("Country" = "Partner"))
EU <- data.frame(Country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
        "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
        "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
        "Spain", "Sweden"),
        EU = rep("EU", 27))
FAOr <- left_join(FAOr, EU)
FAOr[is.na(FAOr$EU),]$EU <- "Non-EU"

GTA4i <- filter(GTA3i, is.na(pprod_Part))
GTA4i$GroupCoarse <- "word"
GTA4i[GTA4i$FAOGroup == "Demersal fish" | GTA4i$FAOGroup == "Pelagic fish" |
        GTA4i$FAOGroup == "Marine fish nei" |
        GTA4i$FAOGroup == "Freshwater & diadromous fish",]$GroupCoarse <- "Fish"
GTA4i[GTA4i$FAOGroup == "Aquatic animals nei" | GTA4i$FAOGroup == "Cephalopods" | 
        GTA4i$FAOGroup == "Crustaceans" | 
        GTA4i$FAOGroup == "Molluscs excl. cephalopods",]$GroupCoarse <- "Invert"  

#calculate p_prod based on supplementary table
GTA4i[GTA4i$Partner == "Vatican City State",]$pprod_Part <- 0
GTA4i[GTA4i$Partner == "Afghanistan",]$pprod_Part <- 0
GTA4i[GTA4i$Partner == "Monaco",]$pprod_Part <- 0
GTA4i[GTA4i$Partner == "Nauru",]$pprod_Part <- 0
GTA4i[GTA4i$Partner == "Pitcairn",]$pprod_Part <- 0
GTA4i[GTA4i$Partner == "Tokelau",]$pprod_Part <- 0
GTA4i[GTA4i$Partner == "Midway Islands",]$pprod_Part <- mean(FAO[FAO$Country == "American Samoa" |
                                                                   FAO$Country == "Northern Mariana Islands & Guam" &
                                                                   FAO$GroupCoarse == "Fish",]$p_prod)
GTA4i[GTA4i$Partner == "U.S. Minor Outlying Is." & GTA4i$GroupCoarse == "Fish",]$pprod_Part <- mean(FAO[FAO$Country == "American Samoa" |
                                                                   FAO$Country == "Northern Mariana Islands & Guam" &
                                                                   FAO$GroupCoarse == "Fish",]$p_prod)
GTA4i[GTA4i$Partner == "U.S. Minor Outlying Is." & GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAO[FAO$Country == "American Samoa" |
             FAO$Country == "Northern Mariana Islands & Guam" &
             FAO$GroupCoarse == "Invert",]$p_prod)

GTA4i[GTA4i$Partner == "Netherlands Antilles",]$pprod_Part <- mean(FAO[FAO$Country == "Aruba" |
                                                                         FAO$Country == "Curacao" &
                                                                         FAO$GroupCoarse == "Fish",]$p_prod)
GTA4i[GTA4i$Partner == "Oth. Africa, N.E.S." & GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$r1_label == "Africa"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[GTA4i$Partner == "Oth. Africa, N.E.S." & GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$r1_label == "Africa"  &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)


GTA4i[GTA4i$Partner == "Other Asia, N.E.S." & GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$r1_label == "Asia"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Austral. Oceania" | GTA4i$Partner == "Christmas Island" |
        GTA4i$Partner == "Cocos Islands" | GTA4i$Partner == "Norfolk Island" |
        GTA4i$Partner == "Other Australian Terr.") & GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$r1_label == "Oceania"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Austral. Oceania") & GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$r1_label == "Oceania"  &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[GTA4i$Partner == "Returned Bahrain Goods" & GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Bahrain"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[GTA4i$Partner == "Returned Bahrain Goods" & GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Bahrain"  &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[GTA4i$Partner == "Duty Free (Cartagena)" & GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Colombia"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "FTZ-Colon" | GTA4i$Partner == "FTZ-Ecuador") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Ecuador"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "FTZ-Colon" | GTA4i$Partner == "FTZ-Ecuador") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Ecuador"  &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Other Europe, N.E.S.") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$r1_label == "Europe"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "French Oceania") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$r2_label == "Polynesia"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "San Marino") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Italy"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Central/South Amer., NES" | GTA4i$Partner == "Other Latin America, N.E.S.") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[(FAOr$r2_label == "Central America" | FAOr$r2_label == "Latin America")
            & FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Central/South Amer., NES" | GTA4i$Partner == "Other Latin America, N.E.S.") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$r2_label == "Central America" | FAOr$r2_label == "Latin America")
            & FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)


GTA4i[(GTA4i$Partner == "St. Brandon & Outer Islands") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Mauritius"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Svalbard & Jan Mayen") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Norway"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Svalbard & Jan Mayen") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Norway"  &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)


GTA4i[(GTA4i$Partner == "Other South American Countries") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$r2_label == "South America"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Gibraltar") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Spain"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Andorra" | GTA4i$Partner == "Liechtenstein") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Switzerland"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Andorra" | GTA4i$Partner == "Liechtenstein") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Switzerland"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Andorra" | GTA4i$Partner == "Liechtenstein") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Switzerland"  &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "FTZ-Chabahar") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "Turkey"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "FTZ-Jebel Ali") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$Country == "United Arab Emirates"  &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Other North America, N.E.S.") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "United States" | FAOr$Country == "Mexico" | 
              FAOr$Country == "Canada") &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Other North America, N.E.S.") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "United States" | FAOr$Country == "Mexico" | 
               FAOr$Country == "Canada") &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Oth. West Europe") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$r2_label == "Western Europe") &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Oth. West Europe") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[(FAOr$r2_label == "Western Europe") &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Other Middle & Near Eastern") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$r2_label == "Western Asia") &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Western Sahara") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "Morocco") &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Bouvet Island") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "Norway") &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)
GTA4i[(GTA4i$Partner == "Canary Islands" | GTA4i$Partner == "Gibraltar") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "Spain") &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Kosovo") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "Montenegro") &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Antarctica") & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "Argentina" | 
               FAOr$Country == "Australia" |
               FAOr$Country == "Chile" |
               FAOr$Country == "France" |
               FAOr$Country == "New Zealand" |
               FAOr$Country == "Norway" |
               FAOr$Country == "United Kingdom" ) &
              FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Antarctica") & 
        GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[(FAOr$Country == "Argentina" | 
               FAOr$Country == "Australia" |
               FAOr$Country == "Chile" |
               FAOr$Country == "France" |
               FAOr$Country == "New Zealand" |
               FAOr$Country == "Norway" |
               FAOr$Country == "United Kingdom" ) &
              FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)


GTA4i[(GTA4i$Partner == "Commercial or Military Secret" | 
         GTA4i$Partner == "Duty Free" |
         GTA4i$Partner == "Free Trade Zones" |
         GTA4i$Partner == "High Seas" |
         GTA4i$Partner == "International Waters" |
         GTA4i$Partner == "North Korea" |
         GTA4i$Partner == "Not Determined" |
         GTA4i$Partner == "Other Countries, NES" |
         GTA4i$Partner == "Products Outside Ter. Waters" |
         GTA4i$Partner == "Storage Deposits" |
         GTA4i$Partner == "Unidentified Country" |
         GTA4i$Partner == "Unknown Countries" 
         ) & 
        GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  median(FAOr[FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Commercial or Military Secret" | 
         GTA4i$Partner == "Duty Free" |
         GTA4i$Partner == "Free Trade Zones" |
         GTA4i$Partner == "International Waters" |
         GTA4i$Partner == "North Korea" |
         GTA4i$Partner == "Not Determined" |
         GTA4i$Partner == "Other Countries, NES" |
         GTA4i$Partner == "Products Outside Ter. Waters" |
         GTA4i$Partner == "Storage Deposits" |
         GTA4i$Partner == "Unidentified Country" |
         GTA4i$Partner == "Unknown Countries" 
) & 
  GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  median(FAOr[FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "EU Suppression" | 
         GTA4i$Partner == "European Union" |
         GTA4i$Partner == "Intra EU Stores & Provisions" |
         GTA4i$Partner == "Not Determin Intra EU Trade" 
) & 
  GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$EU == "EU" & FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "EU Suppression" | 
         GTA4i$Partner == "European Union" |
         GTA4i$Partner == "Not Determin Intra EU Trade" 
) & 
  GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$EU == "EU" & FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Non-EU Suppression" | 
         GTA4i$Partner == "Not Determin Extra EU Trade" 
) & 
  GTA4i$GroupCoarse == "Invert",]$pprod_Part <- 
  mean(FAOr[FAOr$EU == "Non-EU" & FAOr$GroupCoarse == "Invert",]$p_prod, na.rm = T)

GTA4i[(GTA4i$Partner == "Non-EU Suppression" | 
         GTA4i$Partner == "Not Determin Extra EU Trade" 
) & 
  GTA4i$GroupCoarse == "Fish",]$pprod_Part <- 
  mean(FAOr[FAOr$EU == "Non-EU" & FAOr$GroupCoarse == "Fish",]$p_prod, na.rm = T)


GTA4 <- select(GTA4i, -GroupCoarse)
          
#COMBINE ALL THE GTA FILES INTO ONE COMPLETE GTA FILE WITH PPROD_PART
GTAc <- rbind(GTA1, GTA2, GTA3, GTA4)



#excluding data where the reporter and partner are the same
GTAc <- filter(GTAc, Reporter != Partner)


#calculate total imports for each Reporter, FAOGroup, Year
totals <- GTAc %>% group_by(Reporter, FAOGroup, Year) %>% 
  summarize(TotalI = sum(Import_Quantity))

#calculate the proportion of imports for that product/year that are associated with a given partner
GTAc <- left_join(GTAc, totals) %>% 
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
#given partner, and the FMI of that partner plus the proportion of partner's exports
#associated with their imports, and the global FMI average
GTAf <- left_join(GTAc, FMIglbl, by = c("FAOGroup" = "ProductType"))

#50:50 Business.. (OLD)
# GTA3$FMIi <- (0.5*GTA3$propI*GTA3$FMI_part) + 
#      (0.5*GTA3$propI*GTA3$FMIglbl)

#Proportional Business.. (Current)
GTAf$FMIi <- (GTAf$pprod_Part*GTAf$propI*GTAf$FMI_part) + 
   ((1-GTAf$pprod_Part)*GTAf$propI*GTAf$FMIglbl)


#compile them all
FMIi <- GTAf %>% group_by(Reporter, Year, FAOGroup) %>% 
  summarize(FMI_Imp1 = sum(FMIi, na.rm = T))

write.csv(FMIi, "ConsumptionAngle/Import_FMI_prop.csv")

