#Kayla Blincow
#7/9/2021

#The purpose of this script is to generate plots and analysis to answer the first
#question of the Trade Analysis.

#Question 1: How does FMIp compare to FMIc?

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(learnr)

#load the data
d <- read.csv("ConsumptionAngle/Final_FAOFMI.csv", header = T)
d <- d[,-1]

#Ecuador has some weirdnesses so we are removing it 
d <- filter(d, Country != "Ecuador")


#plot the different FMI values by country
d$meanFMIc <- rowMeans(d[,c(2,3,5)], na.rm=TRUE)

all <- d %>% 
  mutate(Cons = FoodSupply/max(FoodSupply),
         diff = FMIp - meanFMIc) %>% 
  pivot_longer((cols = c("FMIp", "FMIc1", "FMIc2", "FMIc3", "FMIi")),
                             names_to = "FMItype",
                             values_to = "FMIvalue") %>% 
  arrange(diff)
  
all$Country <- factor(all$Country, levels = unique(all$Country))
all$FMItype <- factor(all$FMItype, levels = c("FMIc1", "FMIc2", "FMIc3",
                                              "FMIp", "FMIi"))
p1 <- ggplot(all) +
  geom_point(aes(x = Country, y = FMIvalue, color = FMItype, size = Cons,
                 shape = FMItype)) +
  scale_color_manual(name = "FMI",
                     values = c("red3", "red3", "red3","#000000", "#0072B2"), 
                     labels = c("Consumption FMI (Proportional)",
                                "Consumption FMI (Guillen)", 
                                "Consumption FMI (Gephart)",
                                "Production FMI",
                                "Import FMI")) +
  scale_shape_manual(name = "FMI",
                     values = c(15,17,18, 19, 19),
                     labels = c("Consumption FMI (Proportional)",
                                "Consumption FMI (Guillen)", 
                                "Consumption FMI (Gephart)",
                                "Production FMI",
                                "Import FMI")) +
  labs(size = "Scaled Consumption", y = "FMI") +  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


png(filename="ConsumptionAngle/Figures/1_FMIdiff.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=400)

p1

dev.off()



#look at different FMIc Calculations
#pivot longer for plotting
d3a <- d %>% 
  pivot_longer(cols = FMIc1:FMIc3_flp, names_to = "FMIc") %>% 
  mutate(diff = FMIp - value) %>% 
  filter(FMIc != "FMIc3")


m1 <- lm(FMIc1 ~ FMIp, data = d)
m2 <- lm(FMIc2 ~ FMIp, data = d)
m3 <- lm(FMIc3_flp ~ FMIp, data = d)


p2 <- ggplot(data = d3a, aes(x = FMIp, y = value, color = FMIc)) +
  geom_point(aes(shape = FMIc), size = 2) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", aes(fill = FMIc)) +
  scale_color_manual(name = expression(paste(FMI[c]," ", "Derivation")),
                     values = c("#a70000", "#ff0000", "#ff5252"),
                     labels = c("Proportional", "Guillen", "Gephart")) +
  scale_fill_manual(name = expression(paste(FMI[c]," ", "Derivation")),
                     values = c("#a70000", "#ff0000", "#ff5252"),
                     labels = c("Proportional", "Guillen", "Gephart")) +
  scale_shape_manual(name = expression(paste(FMI[c]," ", "Derivation")),
                     values = c(15,17,18),
                     labels = c("Proportional", "Guillen", "Gephart")) +
  labs(x = expression(FMI[P]), y = expression(FMI[C]),
       color = "Consumption FMI\n Derivation") +
  xlim(0.25,0.97) +
  ylim(0.25,0.97) +
  theme_classic()



png(filename="ConsumptionAngle/Figures/2_FMIcComp.png", 
    units="in", 
    width=10, 
    height=8, 
    pointsize=15, 
    res=400)

p2

dev.off()

