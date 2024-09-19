#Kayla Blincow
#9//15/2024


#The purpose of this script is to address the third question in the trade analysis using Wild Capture Data

#Q3: Are there specific trade partners that contribut more to FMIp and FMIc disparity?

#UPDATE (11/12/2021)
#reverting back to the proporational calculation of FMIi
#new files will have "_prop" at the end, while original file names will preserve the
#original dissertation 50/50 assumption


#clear my workspace
rm(list = ls())


#load my packages
library(tidyverse)
library(learnr) #for positive reinforcement

#load my data
GTA <- read.csv("ConsumptionAngle/GTA_FAOmatch_Wild.csv", header = T)


GTA_exp <- GTA %>% filter(Direction == "Export") %>% 
  group_by(Reporter) %>% 
  summarize(Exports = sum(livewgt, na.rm = T),
            Partners = n_distinct(Partner))


GTA_imp <- GTA %>% filter(Direction == "Import") %>% 
  group_by(Reporter) %>% 
  summarize(Imports = sum(livewgt, na.rm = T),
            Partners = n_distinct(Partner))

#load FMI data
#d <- read.csv("ConsumptionAngle/Final_FAOFMI.csv", header = T)
d <- read.csv("ConsumptionAngle/Final_FAOFMI_prop_Wild.csv", header = T)
d <- d[,-1]

#Ecuador has some weirdnesses so we are removing it 
d <- filter(d, Country != "Ecuador")


#plot countries based on amount of exports and number of trade partners
d3 <- left_join(d, GTA_exp, by = c("Country" = "Reporter")) %>% 
  mutate(ExportsTot = sum(Exports.x),
         ExportProp = Exports.x/ExportsTot,
         diff = FMIp - mean(c(FMIc1, FMIc2, FMIc3))) %>% 
  arrange(FMIp)

d3$Country <- factor(d3$Country, levels = unique(d3$Country))

p <- ggplot(d3) +
  geom_point(aes(x = FMIp, y = ExportProp, 
                 color = Partners, size = Partners)) +
  labs(x = expression("Production Sustainability ("*FMI[P]*")"), 
       y = "Proportion of Global Exports", 
       size = "# of Trade Partners",
       color = "# of Trade Partners") +
  scale_color_continuous(limits=c(2, 217), breaks=seq(50, 200, by=50)) +
  guides(color= guide_legend(), size=guide_legend()) +
  scale_size_continuous(limits=c(2, 217), breaks=seq(50, 200, by=50)) +
  annotate("text", x = 0.35, y = 0.055, label = "Thailand") +
  annotate("text", x = 0.4, y = 0.201, label = "China") +
  annotate("text", x = 0.81, y = 0.054, label = "Russia") +
  annotate("text", x = 0.835, y = 0.074, label = "Norway") +
  annotate("text", x = 0.96, y = 0.065, label = "United States") +
  annotate("text", x = 0.22, y = 0.201, label = "a)") +
  theme_bw() +
  theme(legend.position = "none")


#plot countries based on amount of exports and number of trade partners
d4 <- left_join(d, GTA_imp, by = c("Country" = "Reporter")) %>% 
  mutate(ImportsTot = sum(Imports.x),
         ImportProp = Imports.x/ImportsTot,
         diff = FMIp - mean(c(FMIc1, FMIc2, FMIc3))) %>% 
  arrange(FMIp)

d4$Country <- factor(d4$Country, levels = unique(d4$Country))

slice_max(d4, ImportProp, n = 5)


p2 <- ggplot(d4) +
  geom_point(aes(x = FMIp, y = ImportProp, 
                 color = Partners, size = Partners)) +
  labs(x = expression("Production Sustainability ("*FMI[P]*")"), 
       y = "Proportion of Global Imports", 
       size = "# of Trade Partners",
       color = "# of Trade Partners") +
  scale_color_continuous(limits=c(2, 217), breaks=seq(50, 200, by=50)) +
  guides(color= guide_legend(), size=guide_legend()) +
  scale_size_continuous(limits=c(2, 217), breaks=seq(50, 200, by=50)) +
  annotate("text", x = 0.715, y = 0.089, label = "Japan") +
  annotate("text", x = 0.4, y = 0.0912, label = "China") +
  annotate("text", x = 0.72, y = 0.051, label = "Spain") +
  annotate("text", x = 0.63, y = 0.0462, label = "France") +
  annotate("text", x = 0.875, y = 0.125, label = "United States") +
  annotate("text", x = 0.22, y = 0.125, label = "b)") +
  theme_bw() +
  theme(legend.position = "bottom")

library(patchwork)
p/p2


#figure is same regardless of import FMI calculation
png(#filename="ConsumptionAngle/Figures/4_GlobalImp_Exp.png", 
  filename="ConsumptionAngle/Figures/4_GlobalImp_Exp_prop_Wild.png", 
  units="in", 
  width=10, 
  height=10, 
  pointsize=15, 
  res=400)

p/p2

dev.off()








#Region information
reg <- read.csv("ConsumptionAngle/FAOMetrics.csv", header = T)

d <- left_join(d, reg)

#let's first explore this question by plotting it
d2 <- left_join(d, GTA, by = c("Country" = "Reporter")) %>% 
  select(Country, FMI, FMI_Cons, Imports.x, Partner, propI, Region, IncomeLevel) %>% 
  mutate(diffPC = FMI - FMI_Cons) %>% 
  arrange(diffPC)

ggplot(d2, aes(x = Country, y = propI, fill = Partner)) +
  geom_bar(stat = "identity")
#welll that doesn't work... 

#maybe just look at trade partners with higher proportions?
range(d2$propI)

d2a <- filter(d2, propI > 0.5)
ggplot(d2a, aes(x = Country, y = propI, fill = Partner)) +
  geom_bar(stat = "identity")

#bigger range
d2b <- filter(d2, propI > 0.25)
ggplot(d2b, aes(x = Country, y = propI, fill = Partner)) +
  geom_bar(stat = "identity")

#bigger range
d2c <- filter(d2, propI > 0.1)
ggplot(d2c, aes(x = Country, y = propI, fill = Partner)) +
  geom_bar(stat = "identity")

#arrange based on magnitude of difference
d2c$Partner <- factor(d2c$Partner, levels = unique(d2c$Partner))

ggplot(d2c, aes(x = Country, y = propI, fill = Partner)) +
  geom_bar(stat = "identity") 



#I think I want to use an NMDS plot to see if there are patterns in similarities of 
#trade partners based on different variables (including FMIdiff and region)
library(vegan)

#first need to create a matrix with proportion of imports associated with each 
#partner, and partners as columns

#remove low proportional partners
d2i <- filter(d2, propI > 0.075 &
                Partner != "Other Countries, NES" & 
                Partner != "Duty Free (Cartagena)")

#create the matrix
d3 <- d2i %>% 
  pivot_wider(names_from = Partner, values_from = propI, values_fill = 0) 

d3nmds <- d3 %>% 
  select(!Country:diffPC) %>% 
  as.matrix()

#run the nmds
set.seed(123)
nmds <- metaMDS(d3nmds, distance = "bray")
nmds

#extract scores
score <- as.data.frame(scores(nmds))

nmds2 <- d3 %>% select(Country, FMI, FMI_Cons, Region, IncomeLevel, diffPC) %>% 
  mutate(NMDS1 = score[,1],
         NMDS2 = score[,2])

#bin diffPCs
nmds2$diffbin <- NA
for(i in 1:nrow(nmds2)){
  if(nmds2$diffPC[i] < 0){
    nmds2$diffbin[i] <- "FMIp - FMIc < 0"
  } else {
    if(nmds2$diffPC[i] >= 0 & nmds2$diffPC[i] < 0.1){
      nmds2$diffbin[i] <- "0 < FMIp - FMIc < 0.1"
    } else {
      if(nmds2$diffPC[i] >= 0.1 & nmds2$diffPC[i] < 0.2){
        nmds2$diffbin[i] <- "0.1 < FMIp - FMIc < 0.2"
      } else {
        if(nmds2$diffPC[i] >= 0.2 & nmds2$diffPC[i] < 0.3){
          nmds2$diffbin[i] <- "0.2 < FMIp - FMIc < 0.3"
        } else {
          if(nmds2$diffPC[i] >= 0.3 & nmds2$diffPC[i] < 0.4){
            nmds2$diffbin[i] <- "0.3 < FMIp - FMIc < 0.4"
          } else {
            nmds2$diffbin[i] <- "0.4 < FMIp - FMIc"
          } 
        }
      }
    }
  }
}

nmds2$diffbin <- factor(nmds2$diffbin, levels = c("FMIp - FMIc < 0",
                                                  "0 < FMIp - FMIc < 0.1",
                                                  "0.1 < FMIp - FMIc < 0.2",
                                                  "0.2 < FMIp - FMIc < 0.3",
                                                  "0.3 < FMIp - FMIc < 0.4",
                                                  "0.4 < FMIp - FMIc") )


#create hull data for different groups
grp.1 <- nmds2[nmds2$diffbin == "FMIp - FMIc < 0", ][chull(nmds2[nmds2$diffbin == "FMIp - FMIc < 0", c("NMDS1", "NMDS2")]), ]  
grp.2 <- nmds2[nmds2$diffbin == "0 < FMIp - FMIc < 0.1",][chull(nmds2[nmds2$diffbin == "0 < FMIp - FMIc < 0.1", c("NMDS1", "NMDS2")]),]  
grp.3 <- nmds2[nmds2$diffbin == "0.1 < FMIp - FMIc < 0.2",][chull(nmds2[nmds2$diffbin == "0.1 < FMIp - FMIc < 0.2", c("NMDS1", "NMDS2")]),]  
grp.4 <- nmds2[nmds2$diffbin == "0.2 < FMIp - FMIc < 0.3",][chull(nmds2[nmds2$diffbin == "0.2 < FMIp - FMIc < 0.3", c("NMDS1", "NMDS2")]),] 
grp.5 <- nmds2[nmds2$diffbin == "0.3 < FMIp - FMIc < 0.4",][chull(nmds2[nmds2$diffbin == "0.3 < FMIp - FMIc < 0.4", c("NMDS1", "NMDS2")]),]  
grp.6 <- nmds2[nmds2$diffbin == "0.4 < FMIp - FMIc",][chull(nmds2[nmds2$diffbin == "0.4 < FMIp - FMIc", c("NMDS1", "NMDS2")]),] 




hull.data <- rbind(grp.1, grp.2, grp.3, grp.4, grp.5, grp.6)
hull.data

#regional hulls
grp.1 <- nmds2[nmds2$Region == "South America" , ][chull(nmds2[nmds2$Region == "South America" , c("NMDS1", "NMDS2")]), ]  
grp.2 <- nmds2[nmds2$Region ==  "South-Eastern Asia",][chull(nmds2[nmds2$Region ==  "South-Eastern Asia", c("NMDS1", "NMDS2")]),]  
grp.3 <- nmds2[nmds2$Region == "Western Asia",][chull(nmds2[nmds2$Region == "Western Asia", c("NMDS1", "NMDS2")]),]  
grp.4 <- nmds2[nmds2$Region == "Eastern Asia", ][chull(nmds2[nmds2$Region == "Eastern Asia", c("NMDS1", "NMDS2")]), ]  
grp.5 <- nmds2[nmds2$Region == "Southern Asia",][chull(nmds2[nmds2$Region == "Southern Asia", c("NMDS1", "NMDS2")]),]  
grp.6 <- nmds2[nmds2$Region == "Central America",][chull(nmds2[nmds2$Region == "Central America", c("NMDS1", "NMDS2")]),]  
grp.7 <- nmds2[nmds2$Region == "Africa", ][chull(nmds2[nmds2$Region == "Africa", c("NMDS1", "NMDS2")]), ]  
grp.8 <- nmds2[nmds2$Region == "Northern Europe",][chull(nmds2[nmds2$Region == "Northern Europe", c("NMDS1", "NMDS2")]),]  
grp.9 <- nmds2[nmds2$Region == "Australia and New Zealand",][chull(nmds2[nmds2$Region == "Australia and New Zealand", c("NMDS1", "NMDS2")]),]  
grp.10 <- nmds2[nmds2$Region == "Eastern Europe", ][chull(nmds2[nmds2$Region == "Eastern Europe", c("NMDS1", "NMDS2")]), ]  
grp.11 <- nmds2[nmds2$Region == "Southern Europe",][chull(nmds2[nmds2$Region == "Southern Europe", c("NMDS1", "NMDS2")]),]  
grp.12 <- nmds2[nmds2$Region == "Northern America",][chull(nmds2[nmds2$Region == "Northern America", c("NMDS1", "NMDS2")]),]  
grp.13 <- nmds2[nmds2$Region == "Western Europe" ,][chull(nmds2[nmds2$Region == "Western Europe", c("NMDS1", "NMDS2")]),]  

hull.data2 <- rbind(grp.1, grp.2, grp.3, grp.4, grp.5, grp.6, grp.7, grp.8,
                    grp.9, grp.10, grp.11, grp.12, grp.13)
hull.data2


#extract partner name info
partners <- as.data.frame(scores(nmds, "species"))
partners$partners <- rownames(partners)




p1 <- ggplot(nmds2, aes(x = NMDS1, y = NMDS2)) + 
  geom_polygon(data=hull.data, 
               aes(x = NMDS1, y = NMDS2, fill = diffbin, group = diffbin), 
               alpha=0.30) +  
  geom_point(aes(color = diffbin), show.legend = F) +
  geom_text(data = partners, aes(x = NMDS1, y = NMDS2, label = partners))+ 
  theme(axis.text.y = element_text(colour = "black"), 
        axis.text.x = element_text(colour = "black"), 
        legend.text = element_text(size = 12, colour ="black", face = "bold"), 
        legend.position = "right", axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold", colour = "black"), 
        legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", fill = "", y = "NMDS2")  


png(filename="ConsumptionAngle/Figures/NMDS.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=12, 
    res=400)

p1

dev.off()
