#Kayla Blincow
#7/10/2021

#The purpose of this script is to the address the fourth question of the trade analysis.
#Q4: Are there certain product types that are worse than others in terms of 
#lowering FMIc?


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(ggeffects)

#load data
d <- read.csv("ConsumptionAngle/Final_FAOFMI.csv", header = T)
d <- d[,-1]

#Ecuador has some weirdnesses so we are removing it 
d <- filter(d, Country != "Ecuador")

#calculate difference between production and consumption FMI
d$diffPC <- d$FMI - d$FMI_Cons


#do some modelling
hist(d$diffPC) #pretty normal looking, so that's good

#test for effect of year and producttype
m1 <- lm(diffPC ~ Year + ProductType, data = d)
plot(m1) #not too bad, in terms of assumptions
summary(m1)

m1a <- lm(diffPC ~ ProductType, data = d)
summary(m1a)

m1b <- lm(diffPC ~ as.factor(Year), data = d)
summary(m1b)

m1c <- lm(diffPC ~ Year, data = d)
summary(m1c)

#year differences are not significant


#let's plot it!
pred <- ggpredict(m1, terms = c("ProductType"))

p1 <- ggplot() +
  geom_violin(data = d, aes(x = ProductType, y = diffPC),
             alpha = 0.5, fill = "gray60") + 
  geom_jitter(data = d, aes(x = ProductType, y = diffPC),
              alpha = 0.2, color = "gray60") + 
  geom_errorbar(data = pred, aes(x = x, ymin = conf.low, 
                                 ymax = conf.high),
                width = 0.1) +
  geom_point(data = pred, aes(x = x, y = predicted), 
             fill = "red", shape = 22, size = 2.5) +
  labs(y = "FMIp - FMIc", x = "Product Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) 

png(filename="ConsumptionAngle/Figures/ProductDiff.png", 
    units="in", 
    width=10, 
    height=7, 
    pointsize=12, 
    res=400)

p1

dev.off()
