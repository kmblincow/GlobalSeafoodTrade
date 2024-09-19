#Kayla Blincow
#5/29/2021

#The purpose of this script is to wrangle the FAO balance sheet data so I can
#easily match it to the GTA data


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)

#read in the data
#FAO
d <- read.csv("ConsumptionAngle/FAOBalanceSheetData.csv", header = T)

#Deal with inconsistencies in country names between datasets
#Countries with different names
#Antigua and Barbuda
d$Country[d$Country == "Antigua and Barbuda"] <- "Antigua & Barbuda"
#Belgium-Luxembourg
d$Country[d$Country == "Belgium-Luxembourg"] <- "Belgium"
#Netherlands Antilles
d$Country[d$Country == "Bonaire/S.Eustatius/Saba"] <- "BES Islands" 
#"British Indian Ocean Ter"
d$Country[d$Country == "British Indian Ocean Ter"] <- "British Indian Ocean Terr."
#Brunei
d$Country[d$Country == "Brunei Darussalam"] <- "Brunei"
#Cape Verde
d$Country[d$Country == "Cabo Verde"] <- "Cape Verde"
#DRC
d$Country[d$Country == "Congo, Dem. Rep. of the"] <- "Democratic Republic of the Congo"
#Curacao
d$Country[d$Country == "Curaçao"] <- "Curacao"
#Czech Republic
d$Country[d$Country == "Czechoslovakia"] <- "Czech Republic"
#Swaziland
d$Country[d$Country == "Eswatini"] <- "Swaziland"
#Ethiopia
d$Country[d$Country == "Ethiopia PDR"] <- "Ethiopia"
#Falkalands
d$Country[d$Country ==  "Falkland Is.(Malvinas)"] <- "Falkland Islands"
#Faeroe Islands
d$Country[d$Country == "Faroe Islands"] <- "Faeroe Islands"
#French Southern Territories
d$Country[d$Country == "French Southern Terr"] <- "French Southern Terr."
#Guam
d$Country[d$Country == "Guam" ] <- "Northern Mariana Islands & Guam"
#Guinea Bissau
d$Country[d$Country == "Guinea-Bissau"] <- "Guinea Bissau"
#Laos
d$Country[d$Country ==  "Lao People's Dem. Rep."] <- "Laos"
#Micronesia
d$Country[d$Country == "Micronesia, Fed.States of"] <- "Micronesia"
#Moldova
d$Country[d$Country == "Moldova, Republic of"] <- "Moldova"
#Mariana Islands
d$Country[d$Country == "Northern Mariana Is."] <- "Northern Mariana Islands & Guam"
#Pitcairns
d$Country[d$Country == "Pitcairn Islands"] <- "Pitcairn" 
#Saint Martin
d$Country[d$Country ==  "Saint-Martin"] <- "Northern Saint-Martin"
#St. Barts
d$Country[d$Country == "Saint Barthélemy"] <- "St. Barthelemy"
#St. Kitts and NEvis
d$Country[d$Country == "Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
#Saint Vincent and Grenadines
d$Country[d$Country == "Saint Vincent/Grenadines"] <- "Saint Vincent & the Grenadines"
#Sao Tome and Principe
d$Country[d$Country == "Sao Tome and Principe"] <- "Sao Tome & Principe" 
#St Pierre and MIquelon
d$Country[d$Country == "St. Pierre and Miquelon"] <- "Saint Pierre & Miquelon"
#sudan
d$Country[d$Country ==  "Sudan (former)"] <- "Sudan"
#Syria
d$Country[d$Country == "Syrian Arab Republic"] <- "Syria"
#Taiwan
d$Country[d$Country == "Taiwan Province of China"] <- "Taiwan"
#Tanzania
d$Country[d$Country ==  "Tanzania, United Rep. of"] <- "Tanzania" 
#East Timor
d$Country[d$Country == "Timor-Leste"] <- "East Timor"
#Trinidad/Tobago
d$Country[d$Country == "Trinidad and Tobago"] <- "Trinidad & Tobago"
#Turks and Caicos
d$Country[d$Country ==  "Turks and Caicos Is."] <- "Turks & Caicos Islands"
#Vietnam
d$Country[d$Country == "Viet Nam"] <- "Vietnam"
#Wallis and Futuna
d$Country[d$Country == "Wallis and Futuna Is."] <- "Wallis and Futuna"
#Yugoslavia
d$Country[d$Country == "Yugoslavia SFR"] <- "Yugoslavia"

#Bolivia
d$Country[d$Country == "Bolivia (Plurinat.State)"] <- "Bolivia"
#Bosnia & Herzegovina
d$Country[d$Country == "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
#China (Hong Kong)
d$Country[d$Country == "China, Hong Kong SAR"] <- "Hong Kong" 
#China (Macao)
d$Country[d$Country == "China, Macao SAR"] <- "Macau" 
#Cote D'Ivoire
d$Country[d$Country == "Côte d'Ivoire"] <- "Cote d Ivoire"
#Czech Republic
d$Country[d$Country == "Czechia"] <- "Czech Republic"
#Iran
d$Country[d$Country == "Iran (Islamic Rep. of)"] <- "Iran"
#Macedonia
d$Country[d$Country == "North Macedonia"] <- "Macedonia"
#Russia
d$Country[d$Country == "Russian Federation"] <- "Russia"
#Serbia
d$Country[d$Country == "Serbia and Montenegro"] <- "Serbia"
#South Korea
d$Country[d$Country == "Korea, Republic of"] <- "South Korea"
#Venezuela
d$Country[d$Country == "Venezuela, Boliv Rep of"] <- "Venezuela"
#USA
d$Country[d$Country == "United States of America"] <- "United States"
#Reunion
d$Country[d$Country == "Réunion"] <- "Reunion"

#UK = Isle of Man
d$Country[d$Country == "Isle of Man"] <- "United Kingdom"



#only look at 2012-2017, and pivot so I have Elements as columns, years as a single
#column and values under elements
d2 <- d %>% 
  select(Country:Element,X2012:X2017) %>% 
  group_by(Country, FAOSTATgroup, Element) %>% 
  summarize(X2012 = sum(X2012),
            X2013 = sum(X2013),
            X2014 = sum(X2014),
            X2015 = sum(X2015),
            X2016 = sum(X2016),
            X2017 = sum(X2017)) %>% 
  pivot_longer(cols = X2012:X2017, names_to = "Year", values_to = "Value") %>% 
  pivot_wider(names_from = Element, values_from = Value) %>% 
  as.data.frame()

#rename columns to make my life easier
names(d2) <- c("Country", "ProductType", "Year", "Exports", "Imports", "NonFoodUse",
               "Production", "Variation", "FoodSupply")


#create a column "class" which classifies each country's PIE relations for each product
#This isn't super necessary with the proportional assumption, but still a useful
#benchmark, so keeping it in
d2$class <- NA

#find instances where exports, imports, and production are all zero
#(assign these to do not include in calculation)
d2[d2$Exports == 0 & d2$Production == 0 & d2$Imports == 0,]$class <- "NotIncluded"


#find instances where exports > 1, production > 1, and imports roughly = 0 
#(assign these to all exports are produced here)
d2[is.na(d2$class) & 
     ((1 >= d2$Exports/d2$Production &  d2$Exports/d2$Production >= 0.9) | 
        (1 >= d2$Production/d2$Exports & d2$Production/d2$Exports >= 0.9)) & 
     (d2$Imports/d2$Exports < 0.1 & d2$Imports/d2$Production < 0.1),]$class <- "no_reexp"


#find instances where exports > 1, production = 0 , and imports > 1
#(assign these to all exports are re-exports/processed)
d2[is.na(d2$class) & 
     ((1 >= d2$Exports/d2$Imports &  d2$Exports/d2$Imports >= 0.9) | 
        (1 >= d2$Imports/d2$Exports & d2$Imports/d2$Exports >= 0.9)) & 
     (d2$Production/d2$Exports < 0.1 & d2$Production/d2$Imports < 0.1),]$class <- "all_reexp"



#find instances where exports = 0 or roughly equal to 0
#(assign these to people straight consuming imports)
d2[d2$Exports < 0.1*(d2$Production + d2$Imports) & is.na(d2$class),]$class <- "no_reexp"



#everything else is proportional processor...
d2[is.na(d2$class),]$class <- "proportional"


#calculate proportion of inputs associated with production vs. imports
d2$p_prod <- d2$Production/(d2$Production + d2$Imports)
d2$p_imp <- d2$Imports/(d2$Production + d2$Imports)

#add FMI data
FMI <- read.csv("ConsumptionAngle/FMI_GTAMatch.csv", header = T)

d3 <- left_join(d2, FMI, by = "Country")


#remove rows without FMI values and NotIncluded classifications
d3 <- filter(d3, class != "NotIncluded" & !is.na(FMI)) %>% #removes FAO countries that don't report GTA data
  select(Country, ProductType, Year, Exports, Imports, NonFoodUse, Production, 
         Variation, FoodSupply, class, p_prod, p_imp, FMI)


write.csv(d3, "ConsumptionAngle/FAO_FMI_class.csv")
