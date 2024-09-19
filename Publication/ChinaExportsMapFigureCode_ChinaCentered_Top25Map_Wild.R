#Kayla Blincow
#9/15/2024

#The purpose of this script is to generate the China export map figure with Wild Capture Data


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

#load recenter function
source("fnctn_Recenter.R")

#load my data
dt <- read.csv("ConsumptionAngle/GTA_FAOmatch_Wild.csv", header = T)
d <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)
FMI <- read.csv("DataExports/FMI/fmi_res.csv", header = T)

#only pull exports
dt <- filter(dt, Direction == "Export") %>% 
  left_join(FMI, by = c("Partner" = "Country")) %>% 
  group_by(Reporter, Partner) %>% 
  dplyr::summarize(Export = sum(livewgt),
                   FMI = first(FMI))


#plot destination of China exports
Ch <- dplyr::filter(dt, Reporter == "China") %>% 
  mutate(totalE = sum(Export),
         propE = Export/totalE) %>% 
  arrange(desc(Export)) 

Ch$Top <- "Other"
Ch[1:25,]$Top <- Ch[1:25,]$Partner

Ch2 <- Ch %>% group_by(Top) %>% 
  dplyr::summarize(propE = sum(propE),
                   FMI = mean(FMI)) %>%
  filter(Top != "Other") %>% 
  arrange(desc(propE))

Ch2$Top <- factor(Ch2$Top, levels = unique(Ch2$Top))

sum(Ch2$propE) #accounts for 89.89% of total exports

levels(Ch2$Top)[3] <- "USA"

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



#make a map!
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
cntry$name[cntry$name=="United States"] <- "USA"

#create edge list just for China Exports
#China centric

#filter for larger exports or else plot will be crazy messy
dt[dt$Partner == "United States",]$Partner <- "USA"
edge_list_og <- select(dt, Reporter, Partner, Export, FMI) %>% 
  filter(Reporter == "China", 
         Export > 0)

#filter to only include top 25 trade partners
edge_list1 <- left_join(Ch2, edge_list_og, by = c("Top" = "Partner", "FMI" = "FMI")) %>% 
  dplyr::rename(Partner = Top)


#join cntry with our list of reporting countries
cntrydata <- as.data.frame(unique(c(unique(edge_list1$Reporter), 
                                    unique(edge_list1$Partner))))
names(cntrydata) <- "name"
geodata <- left_join(cntry, cntrydata) %>% 
  filter(!is.na(latitude))
#reoder so I can supply it to make my graph object
geodata <- geodata[, c(4, 2, 3)] 
colnames(geodata) <- c("name", "lat", "long")

#recenter data so China in the middle
geodata2 <- Recenter(data = geodata, center = 99, idfield = "name") #######
geodata2 <- geodata2[, 1:3]


#only use trade data with countries that match our geodata
edge_list <- filter(edge_list1, Reporter %in% geodata2$name, 
                    Partner %in% geodata2$name) %>% 
  as.data.frame()
edge_list <- edge_list[, c(4, 1, 5, 3)]
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
  #theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))  +
  theme(plot.title = element_text(hjust = 0.5))

#get base map layer
mapd <- map_data('world') %>% filter(region != "China")
mapd_Ch <- map_data('world') %>% filter(region == "China")

#recenter on US
mapd2 <- Recenter(data = mapd, center = 99, idfield = "group")
mapd_Ch2 <- Recenter(data = mapd_Ch, center = 99, idfield = "group")


country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = mapd2, color = "gray30", fill = "gray70",
                               size = 0.15) 

Ch_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                          data = mapd_Ch2, color = "gray30", fill = "#bc444e",
                          size = 0.15, alpha = 0.9) 

mapcoords <- coord_fixed(xlim = c(-81, 279), ylim = c(-55, 80))


points <- left_join(edge_list, geodata2, by = c("to"="name"))

#make export arrow data
arrows <- data.frame(arrow = c(1:2),
                     y = rep(35.86166, 2),
                     x = rep(104.1954, 2),
                     yend = c(56.130366, 9.081999),
                     xend = c(127.766922, 85))


#plot in gpplot
Ch_map <- ggplot(geodata) + country_shapes + Ch_shapes +
  # geom_curve(data = arrows, aes(x = x , y = y, 
  #                                    xend = xend, yend = yend),
  #            curvature = 0,
  #            arrow = arrow(length = unit(0.25, "centimeters"), 
  #                          type = "open"),
  #            linewidth = 1) +
  # 
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
  labs(title = "(b) Exports from China") +
  mapcoords + 
  maptheme


library(patchwork)
boom2 <- Ch_map / p3 + plot_layout(widths = c(4,1), heights = (c(2,1)))

png(filename="ConsumptionAngle/Figures/ChExportsMap3_ChinaCentered_Top25Map_Wild.png", 
    units="in", 
    width=15, 
    height=10, 
    pointsize=15, 
    res=400)

boom2

dev.off()
