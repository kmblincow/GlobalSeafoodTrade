#Kayla Blincow
#5/27/21

#Comparing FAO and GTA data

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)

#load data
FAO <- read.csv("ConsumptionAngle/Final_FAOFMI_prop.csv", header = T)
GTA <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)

#Plot total imports for each country in each dataset
dGTA <- GTA %>% dplyr::group_by(Reporter, Direction) %>% 
  dplyr::summarize(Quantity = sum(livewgt)) %>% 
  filter(Direction == "Import")
dFAO <- FAO %>%
  dplyr::group_by(Country) %>% 
  dplyr::summarize(Imports = sum(Imports))

d <- left_join(dFAO, dGTA, by = c("Country" = "Reporter")) %>% 
  filter(Country != "Mozambique" & Country != "Ecuador")

p1 <- ggplot(d, aes(x = Imports, y = Quantity)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "FAO Imports (2012-2017)", y = "GTA Imports (2012-2017)", 
       title = "Comparison of Total Import Data by Country") +
  theme_classic()


png(filename="ConsumptionAngle/Figures/FAOvGTA.png", 
    units="in", 
    width=5, 
    height=5, 
    res=400)

p1

dev.off()
