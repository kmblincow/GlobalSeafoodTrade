#Kayla Blincow
#7/18/2021

#The purpose of this script is to generate the China/US import/export map figures for the trade MS

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

#load my data
dt <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)
d <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)
FMI <- read.csv("DataExports/FMI/fmi_res.csv", header = T)

#only pull imports
dt <- filter(dt, Direction == "Import") %>% #imports only
  left_join(FMI, by = c("Partner" = "Country")) %>% #add FMI data
  group_by(Reporter, Partner) %>% #group by bilateral relationship
  summarize(Import = sum(livewgt), #sum all imports from each relationship
            FMI = first(FMI)) #only need single FMI value for each reporter



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
edge_list1 <- select(dt, Reporter, Partner, Import, FMI) %>% 
  filter(Reporter == "United States", 
         Import > 0) #can use this to filter for different sized imports

#join county lats and longs with our list of reporting countries
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
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm')) +
  theme(plot.title = element_text(hjust = 0.5))

mapd <- map_data('world') %>% filter(region != "USA")
mapd_US <- map_data('world') %>% filter(region == "USA")

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = mapd, color = "gray30", fill = "gray70",
                               size = 0.15) 

US_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                          data = mapd_US, color = "gray30", fill = "#0e5ab4",
                          size = 0.15, alpha = 0.9) 

mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-55, 80))


points <- left_join(edge_list, geodata, by = c("from"="name"))


#plot in gpplot
US_map <- ggplot(geodata) + country_shapes + US_shapes +
  scale_fill_manual(values = c("#005AB5", "gray30")) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = FMI, size = weight, alpha = weight),
             data = edges_for_plot, curvature = 0.33) +
  scale_size_continuous(guide = "none", range = c(0.25, 2)) + # scale for edge widths
  geom_point(data = points, aes(x = longitude, y = latitude, fill = FMI, 
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
  labs(title = "(A) Imports to the United States") +
  mapcoords + maptheme

#plot origin of top 25 US imports
US <- filter(dt, Reporter == "United States") %>% 
  mutate(totalI = sum(Import),
         propI = Import/totalI) %>% 
  arrange(desc(Import)) 

US$Top <- "Other"
US[1:25,]$Top <- US[1:25,]$Partner

US2 <- US %>% group_by(Top) %>% 
  summarize(propI = sum(propI),
            FMI = mean(FMI)) %>%
  filter(Top != "Other") %>% 
  arrange(desc(propI))

US2$Top <- factor(US2$Top, levels = unique(US2$Top))

p2 <- ggplot(US2, aes(x = Top, y = propI)) +
  geom_col(aes(fill = FMI)) +
  scale_fill_gradient(name = "Production\nSustainability",
                      na.value = "grey50", low = "#DC3220", high = "#005AB5",
                      limits = range(FMI$FMI)) +
  labs(x = "Top 25 US Import Partners", 
       y = "Proportion of Total US Imports\n(2012 - 2017)") +
  annotate("text", x = 22, y = 0.24, 
           label = ("US Production Sustainability = 0.932")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none") 

library(patchwork)
boom <- US_map / p2 + plot_layout(widths = c(4,1), heights = (c(2,1)))

png(filename="ConsumptionAngle/Figures/USImportsMap2.png", 
    units="in", 
    width=15, 
    height=10, 
    pointsize=15, 
    res=400)

boom

dev.off()




#do same thing for China Exports

#load my data
dt <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)

#only pull exports
dt <- filter(dt, Direction == "Export") %>% 
  left_join(FMI, by = c("Partner" = "Country")) %>% 
  group_by(Reporter, Partner) %>% 
  summarize(Export = sum(livewgt),
            FMI = first(FMI))



#make a map!


#China centric

#filter for larger exports or else plot will be crazy messy
edge_list1 <- select(dt, Reporter, Partner, Export, FMI) %>% 
  filter(Reporter == "China", 
         Export > 0)

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
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))  +
  theme(plot.title = element_text(hjust = 0.5))

mapd <- map_data('world') %>% filter(region != "China")
mapd_Ch <- map_data('world') %>% filter(region == "China")

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = mapd, color = "gray30", fill = "gray70",
                               size = 0.15) 

Ch_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                          data = mapd_Ch, color = "gray30", fill = "#bc444e",
                          size = 0.15, alpha = 0.9) 

mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-55, 80))


points <- left_join(edge_list, geodata, by = c("from"="name"))


#plot in gpplot
Ch_map <- ggplot(geodata) + country_shapes + Ch_shapes +
  scale_fill_manual(values = c("#005AB5", "gray30")) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = FMI, size = weight, alpha = weight),
             data = edges_for_plot, curvature = 0.33) +
  scale_size_continuous(guide = "none", range = c(0.25, 2)) + # scale for edge widths
  geom_point(data = points, aes(x = longitude, y = latitude, fill = FMI, 
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
  labs(title = "(B) Exports from China") +
  mapcoords + maptheme

#plot destination of China exports
Ch <- filter(dt, Reporter == "China") %>% 
  mutate(totalE = sum(Export),
         propE = Export/totalE) %>% 
  arrange(desc(Export)) 

Ch$Top <- "Other"
Ch[1:25,]$Top <- Ch[1:25,]$Partner

Ch2 <- Ch %>% group_by(Top) %>% 
  summarize(propE = sum(propE),
            FMI = mean(FMI)) %>%
  filter(Top != "Other") %>% 
  arrange(desc(propE))

Ch2$Top <- factor(Ch2$Top, levels = unique(Ch2$Top))

p3 <- ggplot(Ch2, aes(x = Top, y = propE)) +
  geom_col(aes(fill = FMI)) +
  scale_fill_gradient(name = "Production\nSustainability",
                      na.value = "grey50", low = "#DC3220", high = "#005AB5",
                      limits = range(FMI$FMI)) +
  labs(x = "Top 25 China Export Partners", 
       y = "Proportion of Total China Exports\n(2012 - 2017)") +
  annotate("text", x = 22, y = 0.23, 
           label = ("China Production Sustainability = 0.370")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none") 

library(patchwork)
boom2 <- Ch_map / p3 + plot_layout(widths = c(4,1), heights = (c(2,1)))

png(filename="ConsumptionAngle/Figures/ChExportsMap2.png", 
    units="in", 
    width=15, 
    height=10, 
    pointsize=15, 
    res=400)

boom2

dev.off()
