#Kayla Blincow
#5/3/2021

#UPDATE (7/7/2021): This code is now obsolete, accomplished this task in a fresh
#code file 

#The purpose of this script is to reformat the FAO balance sheet data so that I
#can calculate consumption by country. 

#Consumption = (Imports + Production) - Exports

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)

#read in the data
#FAO
d <- read.csv("ConsumptionAngle/FAOBalanceSheetData.csv", header = T)

#FMI data
FMI <- read.csv("DataExports/FMI/fmi_res.csv", header = T)

#GTA trade data
#read in the trade data
d12 <- read.csv("DataExports/2012_all.csv")
d13 <- read.csv("DataExports/2013_all.csv")
d14 <- read.csv("DataExports/2014_all.csv")
d15 <- read.csv("DataExports/2015_all.csv")
d16 <- read.csv("DataExports/2016_all.csv")
d17 <- read.csv("DataExports/2017_all.csv")

#make a function to tidy the trade data up and get it ready to rock
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
  #let's convert everything to T
  d1 <- dt[dt$Unit== "KG" | dt$Unit== "KN" | dt$Unit== "LB" | dt$Unit== "T",]
  
  
  d1$QuantT <- NA
  d1[d1$Unit == "LB",]$QuantT <- d1[d1$Unit == "LB",]$Quantity*0.000453592
  d1[d1$Unit == "KG" | d1$Unit == "KN",]$QuantT <- 
    d1[d1$Unit == "KG" | d1$Unit == "KN",]$Quantity*.001
  d1[d1$Unit == "T", ]$QuantT <- d1[d1$Unit == "T",]$Quantity
  
  #aggregate acorss trade partners, years, and direction
  d1 <- d1 %>% 
    group_by(Reporter, Partner, Year, Direction) %>% 
    summarize(Tons = sum(QuantT))
  
  d1
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

#check the thing
summary(dt)

#need to widen the data so I have a column for exports and column for imports
dt <- dt %>% pivot_wider(names_from = Direction, 
                         values_from = c(Tons),
                         values_fill = 0) %>% 
  group_by(Reporter, Partner) %>% 
  summarize(Import = sum(Import),
            Export = sum(Export))

#remove duplicate reporters
dt <- dt %>% filter(Reporter != "Austria (Customs)" & 
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



#Deal with inconsistencies in country names between datasets
#Countries with different names
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

#EU Customs territories
#France = Martinique, Reunion, French Guiana, Mayotte, Guadeloupe
d$Country[d$Country== "French Guiana" |
            d$Country == "Guadeloupe" |
            d$Country == "Martinique" |
            d$Country == "Mayotte" |
            d$Country == "Réunion"] <- "France"
#UK = Isle of Man
d$Country[d$Country == "Isle of Man"] <- "United Kingdom"
#United States = Puerto Rico
d$Country[d$Country == "Puerto Rico"] <- "United States"


#Now for the mess that is the GTA partners...
dt$Partner[dt$Partner == "Faroe Islands"] <- "Faeroe Islands"
dt$Partner[dt$Partner == "Korea, South"] <- "South Korea"
dt$Partner[dt$Partner == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
dt$Partner[dt$Partner == "Puerto Rico (U.S.)"] <- "Puerto Rico"
dt$Partner[dt$Partner == "Guam"] <- "Northern Mariana Islands & Guam"
dt$Partner[dt$Partner == "Micronesia (Federated States of)"] <- "Micronesia"
dt$Partner[dt$Partner == "St. Helena"] <- "Saint Helena"
dt$Partner[dt$Partner == "Virgin Islands (British)"] <- "British Virgin Islands"
dt$Partner[dt$Partner == "Virgin Islands (U.S.)"] <- "US Virgin Islands"
dt$Partner[dt$Partner == "Korea, North"] <- "North Korea"
dt$Partner[dt$Partner == "St. Pierre & Miquelon"] <- "Saint Pierre & Miquelon"
dt$Partner[dt$Partner == "Samoa (Western)"] <- "Samoa"
dt$Partner[dt$Partner == "St. Kitts & Nevis"] <- "Saint Kitts & Nevis"
dt$Partner[dt$Partner == "St. Lucia"] <- "Saint Lucia"
dt$Partner[dt$Partner == "Guinea-Bissau"] <- "Guinea Bissau"
dt$Partner[dt$Partner == "Serbia and Montenegro"] <- "Montenegro"
dt$Partner[dt$Partner == "Northern Mariana Islands"] <- "Northern Mariana Islands & Guam"
dt$Partner[dt$Partner == "Wallis & Futuna Islands"] <- "Wallis & Futuna"
dt$Partner[dt$Partner == "Malaysia, Sarawak"] <- "Malaysia"
dt$Partner[dt$Partner == "Isle of Man (U.K.)"] <- "United Kingdom"
dt$Partner[dt$Partner == "France, Metropolitan"] <- "France"
dt$Partner[dt$Partner == "Hawaii (U.S.)"] <- "United States"
dt$Partner[dt$Partner == "Malaysia, Sabah"] <- "Malaysia"
dt$Partner[dt$Partner == "Okinawa (Japan)"] <- "Japan"
dt$Partner[dt$Partner == "Abkhazia"] <- "Georgia"
dt$Partner[dt$Partner == "South Ossetia"] <- "Georgia"
dt$Partner[dt$Partner == "Heligoland"] <- "Germany"
dt$Partner[dt$Partner == "Niue & Tokelau"] <- "Nieu"
dt$Partner[dt$Partner == "Trieste"] <- "Italy"
dt$Partner[dt$Partner == "Ceuta"] <- "Spain"
dt$Partner[dt$Partner == "Melilla"] <- "Spain"
dt$Partner[dt$Partner == "Turk. Rep. of N. Cyprus"] <- "Cyprus"
dt$Partner[dt$Partner == "Cocos (Keeling) Islands"] <- "Cocos Islands"
dt$Partner[dt$Partner == "Belgium-Luxembourg"] <- "Belgium"

#re-sum based on these Partner names
dt <- dt %>% 
  group_by(Reporter, Partner) %>% 
  summarize(Import = sum(Import),
            Export = sum(Export))

#filter FAO data based on available reporters from GTA data
d <- filter(d, Country %in% dt$Reporter)

#manipulate FAO data
d <- d %>% 
  group_by(Country, FAOSTATgroup, Element) %>% 
  summarize(x1995 = sum(X1995),
            x1996 = sum(X1996),
            x1997 = sum(X1997),
            x1998 = sum(X1998),
            x1999 = sum(X1999),
            x2000 = sum(X2000),
            x2001 = sum(X2001),
            x2002 = sum(X2002),
            x2003 = sum(X2003),
            x2004 = sum(X2004),
            x2005 = sum(X2005),
            x2006 = sum(X2006),
            x2007 = sum(X2007),
            x2008 = sum(X2008),
            x2009 = sum(X2009),
            x2010 = sum(X2010),
            x2011 = sum(X2011),
            x2012 = sum(X2012),
            x2013 = sum(X2013),
            x2014 = sum(X2014),
            x2015 = sum(X2015),
            x2016 = sum(X2016),
            x2017 = sum(X2017))

####2012-2017 Combined####

#manipulate FAO data so it is in a reasonable format
d2 <- select(d, c(1:3, 21:26)) %>% #remove columns for years we don't want
  as.data.frame() %>% 
  mutate(Value = rowSums(.[4:9])) %>% #sum across remaining years
  select(c(1:3, 10)) %>% #select only columns we want
  pivot_wider(names_from = Element, values_from = Value)%>% #make each flow a column
  as.data.frame()

#add FMI data
d2 <- left_join(d2, FMI)

#need to get relative proportions of import/export partners for each reporter
totals <- dt %>% group_by(Reporter) %>% 
  summarize(TotalI = sum(Import),
            TotalE = sum(Export))

dt <- left_join(dt, totals) %>%
  mutate(propI = Import/TotalI,
         propE = Export/TotalE) %>% 
  left_join(., FMI, by = c("Partner" = "Country"))
#so there's a fair amount of wonky partners without FMI values
#going to first see who those partners are, then I'm going to focus on the ones
#that make up large proportions of exports
summary(dt[is.na(dt$FMI),]) #looks like most of them make up really small proportions

unique(dt[is.na(dt$FMI),]$Partner)
#oooo! some of these are actually in the FMI data, they just have different names (of course... eyeroll) *change made above with dt renaming


dt[dt$propI == max(dt[is.na(dt$FMI),]$propI),]$Partner #International Waters?
dt[dt$propE == max(dt[is.na(dt$FMI),]$propE),] #Macedonia..

#going to roll with the data as is, and see where that takes us.

#do the calculations (FMI*ProportionImports) which will be used in consumption calc 
#from FAO data
ImpVal <- dt %>% 
  select(Reporter, Partner, propI, FMI) %>% 
  mutate(pFMIsc = propI * FMI) %>% 
  group_by(Reporter) %>% 
  summarize(FMIscI = sum(pFMIsc, na.rm = T)) #this na.rm = T might cause issues

#join that with my FAO dataset (and filter so we only look at countries with 
#trade data)
d2 <- left_join(d2, ImpVal, by = c("Country" = "Reporter")) %>% 
  filter(!is.na(FMIscI))

#all species groups combined
d3 <- d2 %>% 
  group_by(Country) %>% 
  summarize(FoodExp = sum(`Food exports`),
            FoodImp = sum(`Food imports`),
            NonFoodUse = sum(`Non-food uses`),
            Prod = sum(Production),
            StockAdj = sum(`Stock variations`),
            TotFoodSupply = sum(`Total food supply`),
            FMI = first(FMI),
            FMIscI = first(FMIscI)
            ) %>% 
  as.data.frame()
  
#caculate total supply by imports and production, then find proportion from each
d3$propI <- d3$FoodImp/(d3$FoodImp + d3$Prod)
d3$propP <- d3$Prod/(d3$FoodImp + d3$Prod)
d3$FMIscE <- (d3$propI*d3$FMIscI) + (d3$propP*d3$FMI)

d3$Csust <- (d3$Prod*d3$FMI + d3$FoodImp*d3$FMIscI) - (d3$FoodExp*d3$FMIscE) - d3$NonFoodUse + d3$StockAdj
#well this is hard to interpret...

#what if I look at the difference between total food supply scaled by FMI to the
#Csust?
d3$diff <- (d3$TotFoodSupply*d3$FMI) - d3$Csust

#either way I want to save that dataframe
write.csv(d3, "ConsumptionAngle/ConsMetricData.csv")

#also want to save dt for info on proportions of imports
write.csv(dt, "ConsumptionAngle/TradeInfo.csv")

#look at difference betweeen food supply and csust? Csust would penalize your 
#supply based on sustainability, so lower sustainability would have higher diffs
d3$Cdiff <- d3$TotFoodSupply - d3$Csust

#scale per capita??? i don't think so...

#what if remove China?
test <- filter(d3, Country != "China")
plot(test$Cdiff, test$FMI)
plot(test$Csust, test$FMI)
