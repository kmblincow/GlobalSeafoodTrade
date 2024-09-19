#Kayla Blincow
#5/30/2021

#UPDATE (9/15/2024): Updating to create another data file that removes products
#likely associated with aquaculture.

#The purpose of this script is to work with the GTA data to:
# 1. filter out non-food use products
# 2. convert quantities to live weights and 
# 3. Assign each product to the appropriate FAO product group


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)

#load the data
d12 <- read.csv("DataExports/2012_all.csv")
d13 <- read.csv("DataExports/2013_all.csv")
d14 <- read.csv("DataExports/2014_all.csv")
d15 <- read.csv("DataExports/2015_all.csv")
d16 <- read.csv("DataExports/2016_all.csv")
d17 <- read.csv("DataExports/2017_all.csv")

#function to do initial data cleaning
cln_dat <- function(dt){
  #hmmm looks like column names don't match the fields they are a part of..
  #manually specify column names based on online report
  names(dt) <- c("Direction", "Reporter", "Partner", "Product", 
                 "Description", "HS2", "HS4", "HS6", "Currency", "Unit",
                 "Value", "Quantity", "Trade_Period", "Year", "Trade_Month",
                 "Trade_Quarter", "Trade_Half")
  
  dt$HS6 <- as.character(dt$HS6)
  
  
  #only going to look at products reported by weight 
  #KG (kilograms), KN (net kilograms), LB (pounds), T (metric tons)
  #let's convert everything to tonnes
  d1 <- dt[dt$Unit== "KG" | dt$Unit== "KN" | dt$Unit== "LB" | dt$Unit== "T",]
  
  
  d1$QuantT <- NA
  d1[d1$Unit == "LB",]$QuantT <- d1[d1$Unit == "LB",]$Quantity*0.000453592
  d1[d1$Unit == "KG" | d1$Unit == "KN",]$QuantT <- 
    d1[d1$Unit == "KG" | d1$Unit == "KN",]$Quantity*.001
  d1[d1$Unit == "T", ]$QuantT <- d1[d1$Unit == "T",]$Quantity
  
  #remove duplicate reporters
  d1 <- d1 %>% filter(Reporter != "Austria (Customs)" & 
                        Reporter != "China with HK ReExports" &
                        Reporter != "Cote d Ivoire (General)" &
                        Reporter != "Georgia (HS 10)" & 
                        Reporter != "Germany (Customs)" &
                        Reporter != "Ireland (Customs)" &
                        Reporter != "Italy Istat" &
                        Reporter != "Poland (Customs)" &
                        Reporter != "Spain (Customs)" &
                        Reporter != "Thailand (HS 10)" &
                        Reporter != "Turkey (HS 8)" &
                        Reporter != "United Kingdom HMRC" &
                        Reporter != "United States (CIF)" &
                        Reporter != "United States (Consumption/Domestic)" &
                        Reporter != "United States (Duty Paid)" &
                        Reporter != "Romania (Customs)" &
                        Reporter != "France (Customs)" &
                        Reporter != "EU (Exclude UK)" &
                        Reporter != "EU28 (External Trade)" &
                        Reporter != "Mozambique" & #mozambique has reporting issues
                        Partner != "World" #need to remove this summary row
  )
  
  #Standardize GTA partner names to match other datasets...
  d1$Partner[d1$Partner == "Faroe Islands"] <- "Faeroe Islands"
  d1$Partner[d1$Partner == "Korea, South"] <- "South Korea"
  d1$Partner[d1$Partner == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
  d1$Partner[d1$Partner == "Puerto Rico (U.S.)"] <- "Puerto Rico"
  d1$Partner[d1$Partner == "Guam"] <- "Northern Mariana Islands & Guam"
  d1$Partner[d1$Partner == "Micronesia (Federated States of)"] <- "Micronesia"
  d1$Partner[d1$Partner == "St. Helena"] <- "Saint Helena"
  d1$Partner[d1$Partner == "Virgin Islands (British)"] <- "British Virgin Islands"
  d1$Partner[d1$Partner == "Virgin Islands (U.S.)"] <- "US Virgin Islands"
  d1$Partner[d1$Partner == "Korea, North"] <- "North Korea"
  d1$Partner[d1$Partner == "St. Pierre & Miquelon"] <- "Saint Pierre & Miquelon"
  d1$Partner[d1$Partner == "Samoa (Western)"] <- "Samoa"
  d1$Partner[d1$Partner == "St. Kitts & Nevis"] <- "Saint Kitts & Nevis"
  d1$Partner[d1$Partner == "St. Lucia"] <- "Saint Lucia"
  d1$Partner[d1$Partner == "Guinea-Bissau"] <- "Guinea Bissau"
  d1$Partner[d1$Partner == "Serbia and Montenegro"] <- "Montenegro"
  d1$Partner[d1$Partner == "Northern Mariana Islands"] <- "Northern Mariana Islands & Guam"
  d1$Partner[d1$Partner == "Wallis & Futuna Islands"] <- "Wallis & Futuna"
  d1$Partner[d1$Partner == "Malaysia, Sarawak"] <- "Malaysia"
  d1$Partner[d1$Partner == "Isle of Man (U.K.)"] <- "United Kingdom"
  d1$Partner[d1$Partner == "France, Metropolitan"] <- "France"
  d1$Partner[d1$Partner == "Hawaii (U.S.)"] <- "United States"
  d1$Partner[d1$Partner == "Malaysia, Sabah"] <- "Malaysia"
  d1$Partner[d1$Partner == "Okinawa (Japan)"] <- "Japan"
  d1$Partner[d1$Partner == "Abkhazia"] <- "Georgia"
  d1$Partner[d1$Partner == "South Ossetia"] <- "Georgia"
  d1$Partner[d1$Partner == "Heligoland"] <- "Germany"
  d1$Partner[d1$Partner == "Niue & Tokelau"] <- "Nieu"
  d1$Partner[d1$Partner == "Trieste"] <- "Italy"
  d1$Partner[d1$Partner == "Ceuta"] <- "Spain"
  d1$Partner[d1$Partner == "Melilla"] <- "Spain"
  d1$Partner[d1$Partner == "Turk. Rep. of N. Cyprus"] <- "Cyprus"
  d1$Partner[d1$Partner == "Cocos (Keeling) Islands"] <- "Cocos Islands"
  d1$Partner[d1$Partner == "Belgium-Luxembourg"] <- "Belgium"
  d1$Partner[d1$Partner == "Brunei Darussalam"] <- "Brunei"
  d1$Partner[d1$Partner == "St. Vincent & the Grenadines"] <- "Saint Vincent & the Grenadines"
  d1$Partner[d1$Partner == "Tahiti"] <- "French Polynesia"
  
  

  
  d1 %>% 
    group_by(Direction, Reporter, Partner, Product, Description, HS2, HS4, HS6,
             Currency, Year) %>%
    summarize(Quantity = sum(QuantT),
              Value = sum(Value),
              Unit = "Tonnes")
    
}


#do the thing
dt12 <- cln_dat(d12)
dt13 <- cln_dat(d13)
dt14 <- cln_dat(d14)
dt15 <- cln_dat(d15)
dt16 <- cln_dat(d16)
dt17 <- cln_dat(d17)

#combine those things
dt <- rbind(dt12, dt13, dt14, dt15, dt16, dt17)



#removed the big old data files
rm(d12, d13, d14, d15, d16, d17)



####Filter to remove non-human consumption products (things should be "16")####
#ornamentals should already be taken care of...

#Meals
filter(dt, HS2 == 16, str_detect(Description, "Meal"), 
       !str_detect(Description, "Prepared Meals" )) %>% 
  distinct(Description)
#looks like we are good to go on that front

#Oils
filter(dt, HS2 == 16, str_detect(Description, "Oil"), 
       !str_detect(Description, "Olive Oil | Vegetable Oil")) %>% 
  distinct(Description)
#fish oil has  different product code

#Feed
filter(dt, HS2 == 16, str_detect(Description, "Pellet|pellet")) %>% 
  distinct(Description)


#Bait
filter(dt, HS2 == 16, str_detect(Description, "Bait|bait")) %>% 
  distinct(Description)


#Capsules
dt <- filter(dt, !str_detect(Description, "Capsule|capsule"))

  
#Extracts & Juices
dt <- filter(dt, !HS4 == 1603) 

#Products only at Chapter Level or Confidential Products
dt <- filter(dt, !HS6 == "03MMM0" &
               !HS6 == "03SSS9" &
               !HS6 == "03SSS0")

#create csv with list of unique HS6 codes which is how I'm going to assign 
#live weight conversion factors
#write.table(unique(dt$HS6), "ConsumptionAngle/HS6Codes.txt")

#assigned live weigth conversion factors using FAO's Annex I.1: Indicative
#Factors for Converting Product Weight to Live Weight For a Selection of Major
#Fishery Commodities

CF <- read.csv("ConsumptionAngle/HS6Codes.csv", header = T)
CF <- CF[,c(1,3,5,6)]

#combine data with live weight conversions
dt2 <- left_join(dt, CF)

#remove products that aren't appropriate (namely non-fish product HS6 code)
dt2 <- filter(dt2, ConversionFactor != "Remove")

#convert CF to numeric
dt2$ConversionFactor <- as.numeric(dt2$ConversionFactor)

####Live Weight Conversions####
dt2$livewgt <- dt2$Quantity*dt2$ConversionFactor


#calculate proportion of total imports not from direct FAO CFs
check <- dt2 %>% filter(Direction == "Import") %>% 
  ungroup() %>% 
  mutate(totalI = sum(livewgt)) %>% 
  group_by(Derivation) %>% 
  summarize(prop = sum(livewgt)/first(totalI))

####exporting to csv####
write.csv(dt2, "ConsumptionAngle/GTA_FAOmatch.csv")


