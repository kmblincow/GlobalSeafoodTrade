#Kayla Blincow
#7/18/2021

#The purpose of this script is to generate the US import map figure
#UPDATE: 3/6/2024 - Center the map on the US & only include top 25 import partners


#NOTE: THIS REQUIRES THE RECENTER FUNCTION in fnctn_Recenter.R file


#UPDATE: Only map top 25 import partners

#clear my workspace
rm(list = ls())


#load packages
library(tidyverse)
library(plyr)
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(viridis)
library(sf)
library(maps)

####LOAD RECENTER FUNCTION####
source("fnctn_Recenter.R")


#load my data
dt <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)
d <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)
FMI <- read.csv("DataExports/FMI/fmi_res.csv", header = T)

#only pull imports from 
dt <- dplyr::filter(dt, Direction == "Import") %>% #imports only
  left_join(FMI, by = c("Partner" = "Country")) %>% #add FMI data
  group_by(Reporter, Partner) %>% #group by bilateral relationship
  dplyr::summarize(Import = sum(livewgt), #sum all imports from each relationship
                   FMI = first(FMI)) #only need single FMI value for each reporter


#plot origin of top 25 US imports
US <- dplyr::filter(dt, Reporter == "United States") %>% 
  mutate(totalI = sum(Import),
         propI = Import/totalI) %>% 
  arrange(desc(Import)) 

US$Top <- "Other"
US[1:25,]$Top <- US[1:25,]$Partner

US2 <- US %>% group_by(Top) %>% 
  dplyr::summarize(propI = sum(propI),
                   FMI = mean(FMI)) %>%
  filter(Top != "Other") %>% 
  arrange(desc(propI))

US2$Top <- factor(US2$Top, levels = unique(US2$Top))

sum(US2$propI) #represents 94.68% of US imports


p2 <- ggplot(US2, aes(x = Top, y = propI)) +
  geom_col(aes(fill = FMI)) +
  scale_fill_gradient(name = "Production\nSustainability",
                      na.value = "grey50", low = "#DC3220", high = "#005AB5",
                      limits = range(FMI$FMI)) +
  labs(x = "Top 25 USA Import Partners", 
       y = "Proportion of Total USA Imports\n(2012 - 2017)") +
  annotate("text", x = 22, y = 0.24, 
           label = ("USA Production Sustainability = 0.932")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none") 




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


#create edge list just for imports to US
edge_list_og <- dplyr::select(dt, Reporter, Partner, Import, FMI) %>% 
  filter(Reporter == "United States", 
         Import > 0) #can use this to filter for different sized imports

#filter to only include top 25 trade partners
edge_list1 <- left_join(US2, edge_list_og, by = c("Top" = "Partner", "FMI" = "FMI")) %>% 
  dplyr::rename(Partner = Top)


#join county lats and longs with our list of reporting countries
cntrydata <- as.data.frame(unique(c(unique(edge_list1$Reporter), 
                                    unique(edge_list1$Partner))))
names(cntrydata) <- "name"
geodata <- left_join(cntry, cntrydata) %>% 
  filter(!is.na(latitude))
#reoder so I can supply it to make my graph object
geodata <- geodata[, c(4, 2, 3)] 
colnames(geodata) <- c("name", "lat", "long")

#recenter data so US in the middle
geodata2 <- Recenter(data = geodata, idfield = "name")
geodata2 <- geodata2[, 1:3]

#only use trade data with countries that match our geodata
edge_list <- dplyr::filter(edge_list1, Reporter %in% geodata2$name, 
                    Partner %in% geodata2$name) %>% 
  as.data.frame()
edge_list <- edge_list[, c(1, 4, 5, 3)]
names(edge_list) <- c("from", "to", "weight", "FMI")

#make graph object
g <- graph_from_data_frame(edge_list, directed = T, vertices = geodata2)

#prep for plotting
edges_for_plot <- edge_list %>%
  inner_join(geodata2 %>% select(name, long, lat), 
             by = c('from' = 'name')) %>%
  dplyr::rename(x = long, y = lat) %>%
  inner_join(geodata2 %>% select(name, long, lat), 
             by = c('to' = 'name')) %>%
  dplyr::rename(xend = long, yend = lat)
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
  #theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm')) +
  theme(plot.title = element_text(hjust = 0.5))

mapd <- map_data('world') %>% filter(region != "USA")
mapd_US <- map_data('world') %>% filter(region == "USA")

#recenter on US
mapd2 <- Recenter(data = mapd, idfield = "group")
mapd_US2 <- Recenter(data = mapd_US, idfield = "group")


#prep mapping geoms
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = mapd2, color = "gray30", fill = "gray70",
                               linewidth = 0.15) 

US_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                          data = mapd_US2, color = "gray30", fill = "#0e5ab4",
                          size = 0.15, alpha = 0.9) 

mapcoords <- coord_fixed(xlim = c(80, 440), ylim = c(-55, 80))


points <- left_join(edge_list, geodata2, by = c("from"="name"))


#plot in gpplot
US_map <- ggplot(geodata2) + country_shapes + US_shapes +
  scale_fill_manual(values = c("#005AB5", "gray30")) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = FMI, size = weight, alpha = weight),
             data = edges_for_plot, curvature = 0.33) +
  scale_size_continuous(guide = "none", range = c(0.25, 2)) + # scale for edge widths
  geom_point(data = points, aes(x = long, y = lat, fill = FMI,
                                size = weight), # draw nodes
             shape = 21, color = 'black', stroke = 0.5, alpha = 1) +
  scale_size_continuous(guide = "none", range = c(1, 6)) +    # scale for node size
  scale_alpha(guide = "none", range = c(0.4, 1)) +
  scale_color_gradient(name = "Production\nSustainability",
                       na.value = "grey50", low = "#DC3220", high = "#005AB5",
                       limits = range(FMI$FMI)) +
  scale_fill_gradient(name = "Production\nSustainability",
                      na.value = "grey50", low = "#DC3220", high = "#005AB5",
                      limits = range(FMI$FMI)) +
  labs(title = "(a) Imports to the United States of America") +
  mapcoords + 
  maptheme


library(patchwork)
boom <- US_map / p2 + plot_layout(widths = c(4,1), heights = (c(2,1)))

png(filename="ConsumptionAngle/Figures/USImportsMap5_USCentered_Top25.png", 
    units="in", 
    width=15, 
    height=10, 
    pointsize=15, 
    res=400)

boom

dev.off()

