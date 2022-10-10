#Kayla Blincow
#5/6/2021

#The purpose of this script is to explore the FAO/FMI consumption data

#NOTE: We should probs just remove Ecuador from this analysis

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(viridis)

#load ze data
d <- read.csv("ConsumptionAngle/ConsMetricData.csv")
dt <- read.csv("ConsumptionAngle/TradeInfo.csv")
mets <- read.csv("CountryMetrics_Full.csv")

#wrangle the metric data to match the timeline of the FMI data
mets <- filter(mets, Year != 2018 & Year != 2019) %>% 
  group_by(Reporter) %>% 
  summarize(Region = first(Region),
            IncomeGrp = first(IncomeGrp),
            EPI = first(EPI),
            HDI = mean(HDI, na.rm = T),
            GDP = mean(GDP, na.rm = T),
            EEZ = first(EEZ),
            POP = mean(POP, na.rm = T),
            IEF = mean(IEF, na.rm = T),
            CCO = mean(CCO, na.rm = T), 
            GOE = mean(GOE, na.rm = T),
            ROL = mean(ROL, na.rm = T),
            RQU = mean(RQU, na.rm = T),
            SAV = mean(SAV, na.rm = T))


#do some data finagling
d2 <- select(d, Country, TotFoodSupply, FMI, FMIscI, Csust) %>% 
  filter(Country != "Ecuador") %>% 
  left_join(mets, by = c("Country" = "Reporter")) %>% 
  mutate(Cpc = TotFoodSupply/POP,
         diff = FMI - FMIscI) %>% 
  select(Country, IncomeGrp, FMI, FMIscI, Cpc, diff, Csust) %>% 
  filter(!is.na(FMI), !is.na(Cpc)) %>% 
  pivot_longer(cols = FMI:FMIscI,
               names_to = "FMItype",
               values_to = "FMIvalue") %>% 
  arrange(diff)

d2$Country <- factor(d2$Country, levels = unique(d2$Country))
d2$IncomeGrp <- factor(d2$IncomeGrp, levels = c("High", "UpperMid", "LowerMid"))


#plot csust, cuz they are going to want to see that
p <- ggplot(d2) +
  geom_point(aes(x = Country, y = Csust)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Consumption Sustainability")


png(filename="ConsumptionAngle/ConsMetric.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=400)

p

dev.off()


#Plot FMI/FMIscI vs. Country with size scaled to Consumption/Capita
p1 <- ggplot(d2) +
  geom_point(aes(x = Country, y = FMIvalue,
                 color = FMItype, shape = IncomeGrp, size = Cpc)) +
  scale_color_manual(values = c("#000000", "#0072B2"), 
                     labels = c("Production FMI", "Import FMI")) +
  labs(color = "FMI", shape = "Income Group", 
       size = "Consumption per Capita", y = "FMI",
       title = "Imports vs. Production FMI ordered by difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

png(filename="ConsumptionAngle/FMIdiff.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=400)

p1

dev.off()

#next figure
#relative proportions of FMI from imports for each country
dt <- dt %>% 
  filter(Reporter %in% d2$Country,
         Reporter != "Ecuador") %>% 
  group_by(Reporter) %>% 
  arrange(!is.na(FMI), FMI, .by_group = T)

dt$Reporter <- factor(dt$Reporter, levels = unique(d2$Country))

p2 <- ggplot(dt) +
  geom_bar(aes(x = Reporter, y = propI, fill = FMI), 
           position = "fill", stat = "identity") +
  scale_fill_gradient(na.value = "grey50", low = "#DC3220", high = "#005AB5") +
  labs(y = "Proportion of Imports", x = "Country", fill = "Import FMI") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

png(filename="ConsumptionAngle/RelPropImpFMI.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=400)

p2

dev.off()


#look regionally..
reg <- left_join(dt, mets, by = c("Reporter" = "Reporter"))

regp <- function(region){
  ggplot(filter(reg, Region == region)) +
    geom_bar(aes(x = Reporter, y = propI, fill = FMI), 
             position = "fill", stat = "identity") +
    scale_fill_gradient(na.value = "grey50", low = "#DC3220", high = "#005AB5") +
    labs(y = "Proportion of Imports", x = "Country", fill = "Import FMI") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}


p <- list(NA)

for(i in 1:length(unique(reg$Region))){
  p[[i]] <- regp(unique(reg$Region)[i])
}

lapply(1:7, function(i)
  ggsave(filename=paste0("ConsumptionAngle/RegionPlots",i,".png"), 
         plot=p[[i]]))


#let's do it without relative proprotions, just straight magnitude (or proportions compared to highest imports?)
p3 <- ggplot(dt) +
  geom_bar(aes(x = Reporter, y = Import, fill = FMI), 
           stat = "identity") +
  scale_fill_gradient(na.value = "grey50", low = "#DC3220", high = "#005AB5") +
  labs(y = "Imports (Tons)", x = "Country", fill = "Import FMI") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


png(filename="ConsumptionAngle/MagImpFMI.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=400)

p3

dev.off()

#production vs consumption raio!

d$Country <- factor(d$Country, levels = unique(d2$Country))

ggplot(filter(d, !is.na(Country))) +
  geom_point(aes(x = Country, y = Prod/TotFoodSupply,
                 size = TotFoodSupply, color = Prod)) +
  scale_y_log10() +
  geom_hline(yintercept = 1) +
  labs(x = "Country", y = "Production/Consumption", 
       size = "Consumption", color = "Production") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(filter(d, !is.na(Country))) +
  geom_point(aes(x = FMI-FMIscI, y = Prod/TotFoodSupply,
                 size = TotFoodSupply, color = Prod)) +
  scale_y_log10() +
  geom_hline(yintercept = 1) +
  labs(x = "HMI Production - HMI Imports", y = "Production/Consumption", 
       size = "Consumption", color = "Production") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

png(filename="ConsumptionAngle/ProdConsRatio.png", 
    units="in", 
    width=10, 
    height=8, 
    pointsize=15, 
    res=400)

p4

dev.off()



#make a map!

#US centric
#load lat/long info for countries
cntry <- read.csv("CountryLatsLongs.csv")

#match country names
cntry$name[cntry$name=="Brunei"] <- "Brunei Darussalam"
cntry$name[cntry$name=="Saint Vincent and the Grenadines"] <- "St. Vincent & the Grenadines"
cntry$name[cntry$name=="Myanmar [Burma]"] <- "Myanmar"
cntry$name[cntry$name=="Côte d'Ivoire"] <- "Cote d Ivoire"
cntry$name[cntry$name=="Trinidad and Tobago"] <- "Trinidad & Tobago"
cntry$name[cntry$name=="Antigua and Barbuda"] <- "Antigua & Barbuda"
cntry$name[cntry$name=="Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
cntry$name[cntry$name=="Turks and Caicos Islands"] <- "Turks & Caicos Islands"
cntry$name[cntry$name=="Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
cntry$name[cntry$name=="Réunion"] <- "Reunion"
cntry$name[cntry$name=="Falkland Islands [Islas Malvinas]"] <- "Falkland Islands"
cntry$name[cntry$name=="Saint Pierre and Miquelon"] <- "Saint Pierre & Miquelon"
cntry$name[cntry$name=="Cocos [Keeling] Islands"] <- "Cocos Islands"
cntry$name[cntry$name=="Faroe Islands"] <- "Faeroe Islands"


#filter for larger exports or else plot will be crazy messy
edge_list1 <- select(dt, Reporter, Partner, Import, FMI) %>% 
  filter(Reporter == "United States", 
         Import > 0)

#join cntry with our list of reporting countries
cntrydata <- as.data.frame(unique(c(unique(edge_list1$Reporter), 
                                    unique(edge_list1$Partner))))
names(cntrydata) <- "name"
geodata <- left_join(cntry, cntrydata) %>% 
  filter(!is.na(latitude))
#reoder so I can supply it to make my graph object
geodata <- geodata[, c(4, 2, 3)] 

#only use trade data with countries that match our geodata
edge_list <- filter(edge_list1, Reporter %in% geodata$name, 
                    Partner %in% geodata$name) %>% 
  as.data.frame()
edge_list <- edge_list[, c(2, 1, 3, 4)]
names(edge_list) <- c("from", "to", "weight", "FMI")

#make graph object
g <- graph_from_data_frame(edge_list, directed = T, vertices = geodata)

#prep for plotting
edges_for_plot <- edge_list %>%
  inner_join(geodata %>% select(name, longitude, latitude), 
             by = c('from' = 'name')) %>%
  rename(x = longitude, y = latitude) %>%
  inner_join(geodata %>% select(name, longitude, latitude), 
             by = c('to' = 'name')) %>%
  rename(xend = longitude, yend = latitude)
assert_that(nrow(edges_for_plot) == nrow(edge_list))


edges_for_plot <- edges_for_plot[edges_for_plot$from!=edges_for_plot$to,]
edge_list <- edge_list[edge_list$from != edge_list$to,]
assert_that(nrow(edges_for_plot) == nrow(edge_list))

#get a base map
#common theme
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm')) 

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "gray70", color = "gray30",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-55, 80))


points <- left_join(edge_list, geodata, by = c("from"="name"))
#plot in gpplot
map <- ggplot(geodata) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = FMI, size = weight, alpha = weight),
             data = edges_for_plot, curvature = 0.33) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(data = points, aes(x = longitude, y = latitude, fill = FMI, 
                                size = weight), # draw nodes
             shape = 21, color = 'black', stroke = 0.5, alpha = 1) +
  scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
  scale_alpha(guide = FALSE, range = c(0.4, 1)) +
  scale_color_gradient(na.value = "grey50", low = "#DC3220", high = "#005AB5") +
  scale_fill_gradient(na.value = "grey50", low = "#DC3220", high = "#005AB5") +
  labs(title = "USA Imports") +
  mapcoords + maptheme

png(filename="ConsumptionAngle/USImportsMap.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=400)

map

dev.off()



#tunnel in to look at USA only. (thru time? plus HSSI?)


