#Kayla Blincow
#7/9/2021

#The purpose of this script is to perform an analysis to address the second trade
#analysis question.
#Q2: Are there country-level characteristics that contribute to/help predict the
#magnitude of disparity between FMIp and FMIc/FMIi?

#Response Variables: FMIp-FMIc OR FMIp - FMIi
#Explanatory Variables: region, # of trade partners, quantity of trade, GDP, 
#population size, income class


#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(lme4)
library(ggeffects)
library(learnr) #for positive reinforcement

#load FMI data
d <- read.csv("ConsumptionAngle/Final_FAOFMI.csv", header = T)
d <- d[,-1]

#Ecuador has some weirdnesses so we are removing it 
d <- filter(d, Country != "Ecuador")


#load Country Metrics data
mets <- read.csv("CountryMetrics_Full.csv")

#wrangle the metric data to match the timeline of the FMI data
mets <- filter(mets, Year != 2018 & Year != 2019) %>% 
  group_by(Reporter) %>% 
  summarize(IncomeGrp = first(IncomeGrp),
            GDP = mean(GDP, na.rm = T),
            POP = mean(POP, na.rm = T),
            GDPpc = GDP/POP)

#join with data file
d2 <- left_join(d, mets, by = c("Country" = "Reporter"))

#load FAO region/income data
reg <- read.csv("ConsumptionAngle/FAOMetrics.csv", header = T)


d3 <- left_join(d2, reg)

#load GTA data to calculate number of trade partners for each country
GTA <- read.csv("ConsumptionAngle/GTA_FAOmatch.csv", header = T)

partners <- GTA %>% group_by(Reporter) %>% 
  summarize(Partners = n_distinct(Partner))
 
d4 <- left_join(d3, partners, by = c("Country" = "Reporter"))

#calculate the response variables
d4$meanFMIc <- rowMeans(d4[,c(2,3,5)], na.rm=TRUE)
d4$diffPC <- d4$FMIp - d4$meanFMIc

d4$Region <- factor(d4$Region, levels = c( 
  "Southern Asia", "South-Eastern Asia", 
  "Eastern Asia", "Western Asia", "Australia and New Zealand",
  "Africa", 
  "Southern Europe", "Western Europe", "Eastern Europe", 
  "Northern Europe", "South America", "Central America",
  "Northern America"))

d4[d4$Country == "Bolivia",]$Region <- "South America"
d4[d4$Country == "Czech Republic",]$Region <- "Eastern Europe"
d4[d4$Country == "Taiwan",]$Region <- "Eastern Asia"

#let's run some models!!!
#check the distribution of the data
hist(d4$diffPC) #distribution isn't bad

m1 <- lm(diffPC ~ Region + GDPpc, data = d4)

summary(m1)


#make some visualizations
#PCdiff - important variables Region, Exports_sc, Imports_sc
reg_est <- data.frame(Region = c("Southern Asia", "South-Eastern Asia", 
                                 "Eastern Asia", "Western Asia", 
                                 "Australia and New Zealand",
                                 "Africa", 
                                 "Southern Europe", "Western Europe", "Eastern Europe", 
                                 "Northern Europe", "South America", "Central America",
                                 "Northern America"),
                      Estimate = coef(m1)[1:13],
                      lower = confint(m1)[1:13,1],
                      upper = confint(m1)[1:13,2])

reg_est$Region <- factor(reg_est$Region, 
                         levels =  c("Southern Asia", "South-Eastern Asia", 
                                     "Eastern Asia", "Western Asia", 
                                     "Australia and New Zealand",
                                     "Africa", 
                                     "Southern Europe", "Western Europe", "Eastern Europe", 
                                     "Northern Europe", "South America", "Central America",
                                     "Northern America"))

p1 <- ggplot() +
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_errorbar(data = reg_est, aes(x = Region, ymin = lower, ymax = upper),
                width = 0.2) +
  geom_point(data = reg_est, aes(x = Region, y = Estimate), 
             fill = "red", shape = 22, size = 2.5) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 5.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 10.5) +
  labs(x = "Region", y = expression(paste(FMI[P], " ", "-", " mean", (FMI[C])))) +
  annotate("text", x = 1, y = 0.3, label = "b)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#effect of GDP
pred <- ggpredict(m1, terms = c("GDPpc"))
p2 <- plot(pred, show.title = F) +
  geom_hline(yintercept = 0, alpha = 0.2) +
  geom_point(data = d4, aes(x = GDPpc, y = diffPC), alpha = 0.5, size = 2) +
  labs(y = expression(paste(FMI[P], " ", "-", " mean", (FMI[C]))), 
       x = "Per Capita GDP (USD)") +
  annotate("text", x = -50, y = 0.25, label = "a)") +

  theme_bw()


library(patchwork)

p2/p1

png(filename="ConsumptionAngle/Figures/RegionDiff.png", 
    units="in", 
    width=8, 
    height=9, 
    pointsize=15, 
    res=400)

p2/p1

dev.off()





#OLD CODE
#take a look at differnece in FMIp and FMIc

m1 <- lm(diffPC ~ IncomeLevel, data = d4)
m2 <- lm(diffPC ~ Region, data = d4)
m3 <- lm(diffPC ~ GDP_sc, data = d4)

AIC(m1, m2, m3) #region is important and maybe incomelevel

#explore other options
m1a <- lm(diffPC ~  Region, data = d4)

m1b <- lm(diffPC ~  Region + Partners, data = d4)
m1c <- lm(diffPC ~  Region + Imports, data = d4)
m1d <- lm(diffPC ~  Region + Exports, data = d4)
m1e <- lm(diffPC ~  Region + POP, data = d4)
m1f <- lm(diffPC ~  Region + Partners + Imports, data = d4)
m1g <- lm(diffPC ~  Region + Partners + Exports, data = d4)
m1h <- lm(diffPC ~  Region + Partners + POP, data = d4)
m1i <- lm(diffPC ~  Region + Imports + Exports, data = d4)
m1j <- lm(diffPC ~  Region + Imports + POP, data = d4)
m1k <- lm(diffPC ~  Region + Exports + POP, data = d4)
m1l <- lm(diffPC ~  Region + Partners + Imports + Exports, data = d4)
m1m <- lm(diffPC ~  Region + Partners + Imports + POP, data = d4)
m1n <- lm(diffPC ~  Region + Partners + Exports + POP, data = d4)
m1o <- lm(diffPC ~  Region + Imports + Exports + POP, data = d4)
m1p <- lm(diffPC ~  Region + Partners + Imports + Exports + 
            POP, data = d4)
m1q <- lm(diffPC ~  Region + Partners + Imports + Exports + 
            POP + GDP, data = d4)

m1r <- lm(diffPC ~  Region + Partners + Imports + Exports + 
            POP + IncomeLevel, data = d4)
m1s <- lm(diffPC ~  Imports + Exports + IncomeLevel, data = d4)


AIC(m1a, m1b, m1c, m1d, m1e, m1f, m1g, m1h, m1i, m1j, m1k, m1l, m1m, m1n, 
    m1o, m1p, m1q, m1r, m1s)

#plot(m1i) #China is an outlier (sitting outside the cook's distance line)
summary(m1i)

#ran this a bunch of times and a bunch of different ways, and m1i always comes 
#out on top

#make some visualizations
#PCdiff - important variables Region, Exports_sc, Imports_sc
reg_est <- data.frame(Region = c("Southern Asia", "South-Eastern Asia", 
                                 "Eastern Asia", "Western Asia", 
                                 "Australia and New Zealand",
                                 "Africa", 
                                 "Southern Europe", "Western Europe", "Eastern Europe", 
                                 "Northern Europe", "South America", "Central America",
                                 "Northern America"),
                      Estimate = coef(m1i)[1:13],
                      lower = confint(m1i)[1:13,1],
                      upper = confint(m1i)[1:13,2])


p1 <- ggplot() +
  geom_point(data = d4, aes(x = Region, y = (FMIp - FMIc)),
             alpha = 0.5, size = 2) + 
  geom_errorbar(data = reg_est, aes(x = Region, ymin = lower, ymax = upper),
                width = 0.2) +
  geom_point(data = reg_est, aes(x = Region, y = Estimate), 
             fill = "red", shape = 22, size = 2.5) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 5.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 10.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))


png(filename="ConsumptionAngle/Figures/RegionDiff.png", 
    units="in", 
    width=8, 
    height=5, 
    pointsize=15, 
    res=400)

p1

dev.off()

#also an effect of exports...
pred <- ggpredict(m1i, terms = c("Exports"))
p2 <- plot(pred, show.title = F) +
  geom_point(data = d4, aes(x = Exports, y = diffPC), alpha = 0.5, size = 2) +
  labs(y = "FMIp - FMIc", x = "Total Exports (2012 - 2017)") +
  theme_bw()

png(filename="ConsumptionAngle/Figures/ExportsRegression.png", 
    units="in", 
    width=6, 
    height=5, 
    pointsize=15, 
    res=400)

p2

dev.off()


#what if I remove China?
d5 <- d4[-13,]

m1a <- lm(diffPC ~  Region, data = d5)

m1b <- lm(diffPC ~  Region + Partners, data = d5)
m1c <- lm(diffPC ~  Region + Imports, data = d5)
m1d <- lm(diffPC ~  Region + Exports, data = d5)
m1e <- lm(diffPC ~  Region + POP, data = d5)
m1f <- lm(diffPC ~  Region + Partners + Imports, data = d5)
m1g <- lm(diffPC ~  Region + Partners + Exports, data = d5)
m1h <- lm(diffPC ~  Region + Partners + POP, data = d5)
m1i <- lm(diffPC ~  Region + Imports + Exports, data = d5)
m1j <- lm(diffPC ~  Region + Imports + POP, data = d5)
m1k <- lm(diffPC ~  Region + Exports + POP, data = d5)
m1l <- lm(diffPC ~  Region + Partners + Imports + Exports, data = d5)
m1m <- lm(diffPC ~  Region + Partners + Imports + POP, data = d5)
m1n <- lm(diffPC ~  Region + Partners + Exports + POP, data = d5)
m1o <- lm(diffPC ~  Region + Imports + Exports + POP, data = d5)
m1p <- lm(diffPC ~  Region + Partners + Imports + Exports + 
            POP, data = d5)
m1q <- lm(diffPC ~  Region + Partners + Imports + Exports + 
            POP + GDP, data = d5)

m1r <- lm(diffPC ~  Region + Partners + Imports + Exports + 
            POP + IncomeLevel, data = d5)
m1s <- lm(diffPC ~  Imports + Exports + IncomeLevel, data = d5)


AIC(m1a, m1b, m1c, m1d, m1e, m1f, m1g, m1h, m1i, m1j, m1k, m1l, m1m, m1n, 
    m1o, m1p, m1q, m1r, m1s)

#m1i is still the best model
#plot(m1i)
summary(m1i)

#plot the exports regression
pred <- ggpredict(m1i, terms = c("Exports"))
p3 <- plot(pred, show.title = F) +
  geom_point(data = d5, aes(x = Exports, y = diffPC), alpha = 0.5, size = 2) +
  labs(y = "FMIp - FMIc", x = "Total Exports (2012 - 2017)") +
  theme_bw()

png(filename="ConsumptionAngle/Figures/ExportsRegression_NoChina.png", 
    units="in", 
    width=6, 
    height=5, 
    pointsize=15, 
    res=400)

p3

dev.off()




#OLD CODE

#start with the difference between FMIp and FMIi
#GDP and IncomeLevel are all likely confounded let's start by testing
#which of those variables is best
m1 <- lm(diffPI ~ Region + Partners_sc + Imports_sc + Exports_sc + POP_sc,
         data = d4)
plot(m1)
#2 outliers, but otherwise it looks like the assumptions are violated
summary(m1)


m2 <- lm(diffPI ~ Partners_sc + Imports_sc + Exports_sc + POP_sc + GDP_sc, 
         data = d4)
plot(m2)
summary(m2)

m3 <- lm(diffPI ~ Partners_sc + Imports_sc + Exports_sc + POP_sc + IncomeLevel, 
         data = d4)
plot(m3) #one outlier
summary(m3)

AIC(m1, m2, m3) #m1 is the best? Use region?
#I don't think I like the region or income level variables, I'm more interested in GDP

#explore other options
m1a <- lm(diffPI ~  Region, data = d4)
summary(m1a)

m1b <- lm(diffPI ~  Region + Partners_sc, data = d4)
m1c <- lm(diffPI ~  Region + Imports_sc, data = d4)
m1d <- lm(diffPI ~  Region + Exports_sc, data = d4)
m1e <- lm(diffPI ~  Region + POP_sc, data = d4)
m1f <- lm(diffPI ~  Region + Partners_sc + Imports_sc, data = d4)
m1g <- lm(diffPI ~  Region + Partners_sc + Exports_sc, data = d4)
m1h <- lm(diffPI ~  Region + Partners_sc + POP_sc, data = d4)
m1i <- lm(diffPI ~  Region + Imports_sc + Exports_sc, data = d4)
m1j <- lm(diffPI ~  Region + Imports_sc + POP_sc, data = d4)
m1k <- lm(diffPI ~  Region + Exports_sc + POP_sc, data = d4)
m1l <- lm(diffPI ~  Region + Partners_sc + Imports_sc + Exports_sc, data = d4)
m1m <- lm(diffPI ~  Region + Partners_sc + Imports_sc + POP_sc, data = d4)
m1n <- lm(diffPI ~  Region + Partners_sc + Exports_sc + POP_sc, data = d4)
m1o <- lm(diffPI ~  Region + Imports_sc + Exports_sc + POP_sc, data = d4)
m1p <- lm(diffPI ~  Region + Partners_sc + Imports_sc + Exports_sc + POP_sc, data = d4)
m1q <- lm(diffPI ~  Region + IncomeLevel, data = d4)
m1r <- lm(diffPI ~  Region + IncomeLevel + POP_sc + Partners_sc, data = d4)
AIC(m1a, m1b, m1c, m1d, m1e, m1f, m1g, m1h, m1i, m1j, m1k, m1l, m1m, m1n, 
    m1o, m1p, m1q, m1r)

#m1r  have the lowest AIC value
plot(m1r)


summary(m1r)
