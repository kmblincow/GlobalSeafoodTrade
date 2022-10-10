#Kayla Blincow
#7/9/2021

#The purpose of this script is to generate plots and analysis to answer the first
#question of the Trade Analysis.

#Question 1: How does FMIp compare to FMIc?

#UPDATE (11/12/2021)
#reverting back to the proporational calculation of FMIi
#new files will have "_prop" at the end, while original file names will preserve the
#original dissertation 50/50 assumption

#UPDATE(6/8/2022)
#adding raw estimates/ranges of consumption to new plot (4/8/2022)
#adding additional panel to the plot showing proportion of consumption
#associated with imports versus locally produced seafood

#UPDATE (6/16/2022)
#changing the x axis of the plot to be FMI and plotting raw values with bar as the difference

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(learnr)

#load the data
#original 50/50 data
#d <- read.csv("ConsumptionAngle/Final_FAOFMI.csv", header = T)

#proportional data
d <- read.csv("ConsumptionAngle/Final_FAOFMI_prop.csv", header = T)
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


# png(#filename="ConsumptionAngle/Figures/1_FMIdiff.png", 
#   filename="ConsumptionAngle/Figures/1_FMIdiff_prop.png", 
#     units="in", 
#     width=15, 
#     height=8, 
#     pointsize=15, 
#     res=400)
# 
# p1
# 
# dev.off()



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
  labs(x = expression("Production Sustainability ("*FMI[P]*")"),
       y = expression("Consumption Sustainability ("*FMI[C]*")"),
       color = "Consumption FMI\n Derivation") +
  xlim(0.25,0.97) +
  ylim(0.25,0.97) +
  theme_classic()



# png(#filename="ConsumptionAngle/Figures/2_FMIcComp.png", 
#   filename="ConsumptionAngle/Figures/2_FMIcComp_prop.png",   
#   units="in", 
#     width=10, 
#     height=8, 
#     pointsize=15, 
#     res=400)
# 
# p2
# 
# dev.off()

#try something else
d$diff <- d$FMIp -d$meanFMIc
d[order(d$diff),] %>% head(5)


d$Country <- factor(d$Country, levels = d[order(d$FMIp),]$Country)

#top 25 seafood producers...
FMI <- read.csv("DataExports/FMI/fmi_res.csv", header = T)
d2 <- d %>% slice_max(Production, n = 25)
d2$Cons_sc <- d2$FoodSupply/max(d2$FoodSupply)

#percent decrease...
#calculate difference between two numbers you are comparing, then divide the 
#decrease by the original number and multiply by 100
d2$perc_dec1 <- (d2$FMIp - d2$FMIc1)/(d2$FMIp)*100
d2$perc_dec2 <- (d2$FMIp - d2$FMIc2)/(d2$FMIp)*100
d2$perc_dec3 <- (d2$FMIp - d2$FMIc3_flp)/(d2$FMIp)*100
d2$perc_dec <- (d2$perc_dec1 + d2$perc_dec2 +d2$perc_dec3)/3
d2$perc_deca <- (d2$FMIp - d2$meanFMIc)/(d2$FMIp)*100
d2$perc_decb <- (d2$meanFMIc - d2$FMIp)/(d2$meanFMIc)*100

#percent difference...
#calculate difference between two numbers, then divide by the mean of the numbers
#and multiply by 100
#d2$perc_diff <- (d2$diff/((d2$FMIp + d2$meanFMIc)/2))*100
#do that for each individual FMIc estimate then calculate the mean difference?
#yeah... should probs do that..
d2$perc_diff1 <- ((d2$FMIp - d2$FMIc1)/((d2$FMIp + d2$FMIc1)/2))*100
d2$perc_diff2 <- ((d2$FMIp - d2$FMIc2)/((d2$FMIp + d2$FMIc2)/2))*100
d2$perc_diff3 <- ((d2$FMIp - d2$FMIc3_flp)/((d2$FMIp + d2$FMIc3_flp)/2))*100
d2$perc_diff <- (d2$perc_diff1 + d2$perc_diff2 +d2$perc_diff3)/3


library(grid)

#absolute difference
d2$diff1 <- (d2$FMIp - d2$FMIc1)
d2$diff2 <- (d2$FMIp - d2$FMIc2)
d2$diff3 <- (d2$FMIp - d2$FMIc3_flp)
d2$diff <- (d2$diff1 + d2$diff2 +d2$diff3)/3


#OG PLOT THAT I THINK IS THE BEST STILL

bars <- data.frame(min = rep(NA, nrow(d2)), max = rep(NA, nrow(d2)),
                   Country = d2$Country)

for(i in 1:nrow(d2)) {
  bars$min[i] <- min(d2$diff1[i], d2$diff2[i], d2$diff3[i])
  bars$max[i] <- max(d2$diff1[i], d2$diff2[i], d2$diff3[i])
}

p3 <- ggplot(data = d2) +
  geom_col(aes(x = diff, y = Country, fill = FMIp)) +
  geom_point(aes(x = diff1, y = Country), alpha = 0.5) +
  geom_point(aes(x = diff2, y = Country), alpha = 0.5) +
  geom_point(aes(x = diff3, y = Country), alpha = 0.5) +
  geom_errorbarh(data = bars, aes(xmin = min,
                                  xmax = max,
                                  y = Country),
                 alpha = 0.5, height = 0) +
  labs(x = "Difference Between Production and Consumption Sustainability",
       y = "Top 25 Seafood Producing Countries",
       fill = "Production\nSustainability") +
  theme_bw() +
  coord_cartesian(clip="off") +
  theme(legend.position = "top",
        plot.margin = margin(1, 0, 1, 1, unit = "cm"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  geom_segment(x = -0.1, xend = -0.2, y = 12, yend = 12,
               arrow = arrow(length = unit(1, "cm")),
               size = 3, color = "gray20") +
  geom_segment(x = 0.1, xend = 0.2, y = 12, yend = 12,
               arrow = arrow(length = unit(1, "cm")),
               size = 3, color = "gray20") +
  # geom_segment(x = -0.2, xend = -.2, y = 6, yend = 19,
  #              arrow = arrow(length = unit(1.5, "cm")),
  #              size = 10, color = "gray70") +
  # geom_text(x = -0.2005, y = 12.5,
  #          label = "Increasing Production Sustainability",
  #          angle = 90, size = 5) +
  scale_fill_gradient(low = "#DC3220", high = "#005AB5",
                      limits = range(FMI$FMI)) +
  annotate(geom = "text", x = -0.177, y = 14,
             label = "Production\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = -0.114, y = 14,
           label = "Consumption\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = -0.1455, y = 14,
           label = "<", size = 7) +
  annotate(geom = "text", x = 0.18, y = 14,
           label = "Consumption\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = 0.11, y = 14,
           label = "Production\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = 0.145, y = 14,
           label = ">", size = 7) +
  annotate(geom = "text", x = -.23, y = 24.5,
           label = "(A)")


# p3
# 
# png(filename="ConsumptionAngle/Figures/2_Top25Disparity_b.png",
#     width=5000,
#     height=2500,
#     res=300)
# 
# p3
# 
# dev.off()



# #next thingy
# bars <- data.frame(min = rep(NA, nrow(d2)), max = rep(NA, nrow(d2)),
#                    Country = d2$Country)
#   
# for(i in 1:nrow(d2)) {
#   bars$min[i] <- min(d2$perc_diff1[i], d2$perc_diff2[i], d2$perc_diff3[i])
#   bars$max[i] <- max(d2$perc_diff1[i], d2$perc_diff2[i], d2$perc_diff3[i])
# }
# 
# #PERCENT DIFFERENCE
# p3 <- ggplot(data = d2) +
#   geom_col(aes(x = perc_diff, y = Country, fill = FMIp)) +
#   geom_point(aes(x = perc_diff1, y = Country), alpha = 0.5) +
#   geom_point(aes(x = perc_diff2, y = Country), alpha = 0.5) +
#   geom_point(aes(x = perc_diff3, y = Country), alpha = 0.5) +
#   geom_errorbarh(data = bars, aes(xmin = min,
#                    xmax = max,
#                    y = Country),
#                alpha = 0.5, height = 0) +
#   labs(x = "Percent Difference Between\n Production and Consumption Sustainability",
#        y = "Top 25 Seafood Producing Countries",
#        fill = "Production\nSustainability") +
#   theme_bw() +
#   coord_cartesian(clip="off") +
#   theme(legend.position = "top",
#         plot.margin = margin(1, 1, 1, 1, unit = "cm"),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 14)) +
#   geom_segment(x = -22, xend = -42, y = 13, yend = 13,
#                arrow = arrow(length = unit(1, "cm")),
#                size = 3, color = "gray20") +
#   geom_segment(x = 12, xend = 32, y = 13, yend = 13,
#                arrow = arrow(length = unit(1, "cm")),
#                size = 3, color = "gray20") +
#   # geom_segment(x = -0.2, xend = -.2, y = 6, yend = 19,
#   #              arrow = arrow(length = unit(1.5, "cm")),
#   #              size = 10, color = "gray70") +
#   # geom_text(x = -0.2005, y = 12.5,
#   #          label = "Increasing Production Sustainability",
#   #          angle = 90, size = 5) +
#   scale_fill_gradient(low = "#DC3220", high = "#005AB5",
#                       limits = range(FMI$FMI)) +
#   annotate(geom = "text", x = -35, y = 15,
#            label = "Production\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = -25, y = 15,
#            label = "Consumption\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = -30, y = 15,
#            label = "<", size = 7) +
#   annotate(geom = "text", x = 25, y = 15,
#            label = "Consumption\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = 15, y = 15,
#            label = "Production\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = 20, y = 15,
#            label = ">", size = 7) 
# 
# 
# p3
# 
# png(filename="ConsumptionAngle/Figures/2_Top25Disparity_percentdifference.png",
#   width=5000,
#   height=2500,
#   res=300)
# 
# p3
# 
# dev.off()
# 
# #next thingy
# 
# bars2 <- data.frame(min = rep(NA, nrow(d2)), max = rep(NA, nrow(d2)),
#                    Country = d2$Country)
# 
# for(i in 1:nrow(d2)) {
#   bars2$min[i] <- min(d2$perc_dec1[i], d2$perc_dec2[i], d2$perc_dec3[i])
#   bars2$max[i] <- max(d2$perc_dec1[i], d2$perc_dec2[i], d2$perc_dec3[i])
# }
# 
# 
# #PERCENT CHANGE
# p3 <- ggplot(data = d2) +
#   geom_col(aes(x = perc_dec, y = Country, fill = FMIp)) +
#   geom_point(aes(x = perc_dec1, y = Country), alpha = 0.5) +
#   geom_point(aes(x = perc_dec2, y = Country), alpha = 0.5) +
#   geom_point(aes(x = perc_dec3, y = Country), alpha = 0.5) +
#   geom_errorbarh(data = bars2, aes(xmin = min,
#                                   xmax = max,
#                                   y = Country),
#                  alpha = 0.5, height = 0) +
#   labs(x = "Percent Difference Between\n Production and Consumption Sustainability",
#        y = "Top 25 Seafood Producing Countries",
#        fill = "Production\nSustainability") +
#   theme_bw() +
#   coord_cartesian(clip="off") +
#   theme(legend.position = "top",
#         plot.margin = margin(1, 1, 1, 1, unit = "cm"),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 14)) +
#   geom_segment(x = -22, xend = -42, y = 13, yend = 13,
#                arrow = arrow(length = unit(1, "cm")),
#                size = 3, color = "gray20") +
#   geom_segment(x = 12, xend = 32, y = 13, yend = 13,
#                arrow = arrow(length = unit(1, "cm")),
#                size = 3, color = "gray20") +
#   # geom_segment(x = -0.2, xend = -.2, y = 6, yend = 19,
#   #              arrow = arrow(length = unit(1.5, "cm")),
#   #              size = 10, color = "gray70") +
#   # geom_text(x = -0.2005, y = 12.5,
#   #          label = "Increasing Production Sustainability",
#   #          angle = 90, size = 5) +
#   scale_fill_gradient(low = "#DC3220", high = "#005AB5",
#                       limits = range(FMI$FMI)) +
#   annotate(geom = "text", x = -35, y = 15,
#            label = "Production\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = -25, y = 15,
#            label = "Consumption\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = -30, y = 15,
#            label = "<", size = 7) +
#   annotate(geom = "text", x = 25, y = 15,
#            label = "Consumption\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = 15, y = 15,
#            label = "Production\nSustainability",
#            size = 4.5) +
#   annotate(geom = "text", x = 20, y = 15,
#            label = ">", size = 7) 
# 
# 
# p3
# 
# png(filename="ConsumptionAngle/Figures/2_Top25Disparity_percentchange.png",
#     width=5000,
#     height=2500,
#     res=300)
# 
# p3
# 
# dev.off()


#Okay... Now to add the new panel.
#Turns out the thing Brice wants to plot is the thing we actually can't calculate...

#Going to spin up a couple different options that kind of get at a similar thing

FAO <- read.csv("ConsumptionAngle/FAO_FMI_class.csv", header = T)

d4 <- FAO %>% filter(Country %in% d2$Country) %>% 
  group_by(Country) %>% 
  summarize(propI = sum(p_imp)/n(),
            propP = sum(p_prod)/n(),
            propIgep = .665,
            propPgep = .365,
            propIgui = 0.26,
            propPgui = 0.74,
            FoodSupply = sum(FoodSupply))
d4a <- d4 %>% 
  pivot_longer(cols = starts_with("propI"),
                    names_to = "ImportType",
                    values_to = "pImport") %>% 
  pivot_longer(cols = starts_with("propP"),
               names_to = "ProdType",
               values_to = "pProd")
d4b <- d4a %>% group_by(Country, FoodSupply) %>% 
  summarize(min = min(pProd),
            max = 1 - max(pProd),
            uncertainty = max(pProd) - min,
            check = min + max + uncertainty) %>% 
  mutate(minTot = min*FoodSupply,
         maxTot = max*FoodSupply,
         uncTot = uncertainty*FoodSupply,
         check2 = FoodSupply - (minTot + maxTot + uncTot))

d4c <- d4b %>% 
  pivot_longer(cols = 7:9, 
               names_to = "Break",
               values_to = "Value")

d4c$Break <- factor(d4c$Break, levels = c("maxTot", "uncTot", "minTot"),
                    labels = c("Imports", "Uncertainty", "Production"))

d4c$Country <- factor(d4c$Country, levels = d2[order(d2$FMIp),]$Country)


ggplot(d4c, aes(fill = Break, x = Country, y = Value)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(y = "Total Consumption", fill = "") +
  scale_fill_manual(values = c("deepskyblue3", "gray50", "deepskyblue4")) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_flip()

#this looks silly...
ggplot(d4c, aes(fill = Break, x = Country, y = Value)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(y = "Total Consumption") +
  scale_fill_manual(values = c("black", "gray50", "gray90")) +
  theme_bw() +
  coord_flip()

#Let's do imports stacked with production and exports on the back end
d5 <- FAO %>% 
  filter(Country %in% d2$Country) %>% 
  group_by(Country) %>% 
  summarize(Imports = sum(Imports),
            Production = sum(Production),
            Exports = sum(Exports),
            FoodSupply = sum(FoodSupply))


d5$Country <- factor(d5$Country, levels = d2[order(d2$FMIp),]$Country)

d5a <- d5 %>% select(Country, Imports, Production) %>% 
  pivot_longer(2:3, names_to = "Inputs")

d5b <- d5 %>% select(Country, Exports, Imports, Production) %>% 
  mutate(Exports = Exports * -1,
         Exports_scld = Exports/(Production + Imports))



#I like this one more
p4 <- ggplot() +
  geom_bar(data = d5a, aes(fill = Inputs, x = Country, y = value),
           position = "fill", stat = "identity") +
  geom_bar(data = d5b, aes(x = Country, y = Exports_scld, fill = "Exports"),
           stat = "identity") +
  scale_fill_manual(values = c("gray40", "deepskyblue4", "deepskyblue3")) +
  geom_hline(yintercept = 0) +
  labs(fill = "", y = "Exports as a Share of\nNational Imports and Production", x = "") +
  theme_bw() +
  annotate(geom = "text", x = 24.5, y = -.7,
           label = "(B)") +
  theme(axis.text.y = element_blank(),
        legend.position = "top",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin = margin(1, 1, 1, 0, unit = "cm")) +
  coord_flip()
p4

#UPDATE 7/19/2022 Changing it so exports have positive value also
d5c <- data.frame(
  Country = factor(d5$Country, levels = d2[order(d2$FMIp),]$Country),
  Imports = d5$Imports/(d5$Imports + d5$Production),
  Production = d5$Production/(d5$Imports + d5$Production),
  Exports = d5$Exports/(d5$Imports + d5$Production),
  Gap = NA
)

d5c <- pivot_longer(d5c, 2:5, names_to = "Flow")

d5c$IO <- NA

d5c[d5c$Flow == "Production" | d5c$Flow == "Imports",]$IO <- "Input"
d5c[d5c$Flow == "Exports",]$IO <- "Output"


for(i in 1:nrow(d5c)){
  if(!is.na(d5c$value[i])){
    d5c$IO[i] <- paste(d5c$Country[i], d5c$IO[i], sep = "")  
  } else {
    d5c$IO[i] <- paste(d5c$Country[i], "gap", sep = "")
  }
}

d5c$IO <- factor(d5c$IO, 
                 levels = c("United StatesOutput", "United StatesInput",
                            "United Statesgap",
                            "IcelandOutput", "IcelandInput", "Icelandgap",
                            "NorwayOutput", "NorwayInput", "Norwaygap",
                            "DenmarkOutput", "DenmarkInput", "Denmarkgap",
                            "RussiaOutput", "RussiaInput", "Russiagap",
                            "CanadaOutput", "CanadaInput", "Canadagap",
                            "United KingdomOutput", "United KingdomInput", 
                            "United Kingdomgap",
                            "ChileOutput", "ChileInput", "Chilegap",
                            "ArgentinaOutput", "ArgentinaInput",
                            "Argentinagap",
                            "South KoreaOutput", "South KoreaInput", 
                            "South Koreagap",
                            "SpainOutput", "SpainInput", "Spaingap",
                            "JapanOutput", "JapanInput", "Japangap",
                            "FranceOutput", "FranceInput", "Francegap",
                            "MoroccoOutput", "MoroccoInput", "Moroccogap",
                            "PeruOutput", "PeruInput", "Perugap",
                            "TaiwanOutput", "TaiwanInput", "Taiwangap",
                            "MexicoOutput", "MexicoInput", "Mexicogap",
                            "IndonesiaOutput", "IndonesiaInput", "Indonesiagap",
                            "TurkeyOutput", "TurkeyInput", "Turkeygap",
                            "IndiaOutput", "IndiaInput", "Indiagap",
                            "MalaysiaOutput", "MalaysiaInput", "Malaysiagap",
                            "PhilippinesOutput", "PhilippinesInput",
                            "Philippinesgap",
                            "ChinaOutput", "ChinaInput", "Chinagap",
                            "ThailandOutput", "ThailandInput", "Thailandgap",
                            "BrazilOutput", "BrazilInput", "Brazilgap"))

p4a <- ggplot() +
  geom_bar(data = d5c, aes(fill = Flow, x = IO, y = value),
           position = "stack", stat = "identity", width = 1) +
  scale_fill_manual(values = c("gray40", "deepskyblue4", "deepskyblue3")) +
  labs(fill = "", y = "Exports as a Share of\nNational Imports and Production", 
       x = "") +
  scale_x_discrete(limits = rev, breaks = seq(2.5, 72.5, by = 3),
                   expand = expansion(mult = c(0, .015)), 
                   position = "right") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        legend.position = "top",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin = margin(1, 1, 0, 0, unit = "cm")) +
  coord_flip() 
p4a





#UPDATE 6/16/2022 Changing the x axis of the main plot
d6 <- d2 %>% select(Country, FMIp, FMIc1, FMIc2, FMIc3_flp, meanFMIc) %>% 
  pivot_longer(cols = 3:5, names_to = "FMI", values_to = "value")

d2$direction <- "Decrease"
d2[d2$FMIp - d2$meanFMIc < 0,]$direction <- "Increase"
d2$direction <- factor(d2$direction)

d2_dec <- filter(d2, direction == "Decrease") 
d2_inc <- filter(d2, direction == "Increase")

bars <- d6 %>% group_by(Country) %>% 
  summarize(max = max(value),
            min = min(value))
shapes <- c(8, 17, 15)

p5 <- ggplot() +
  geom_point(data = d6, aes(x = value, y = Country), 
             shape = 8, alpha = 0.2) +
  geom_segment(data = d2_inc, aes(x = FMIp, xend = meanFMIc, 
                                  y = Country, yend = Country), 
               color = "darkgreen", size = 4, alpha = 0.5) +
  geom_segment(data = d2_dec, aes(x = FMIp, xend = meanFMIc, 
                                  y = Country, yend = Country), 
               color = "brown3", size = 4, alpha = 0.5) +
  geom_point(data = d6, aes(x = value, y = Country, shape = "Consumption Sustainability (3 Derivations)"),
             alpha = 0.2) +
  geom_point(data = d2, aes(x = FMIp, y = Country, shape = "Production Sustainability")) +
  geom_point(data = d2, aes(x = meanFMIc, y = Country, shape = "Mean Consumption Sustainability")) +
  geom_errorbarh(data = bars, aes(xmin = min,
                                  xmax = max,
                                  y = Country),
                 alpha = 0.5, height = 0) +
  geom_hline(yintercept = 8.5, linetype = "longdash") +
  scale_shape_manual(values = shapes) +
  labs(x = "Sustainability Metric (FMI) Value",
       y = "Top 25 Seafood Producing Countries\n(Arranged by Production Sustainability)",
       shape = "") +
  theme_bw() +
  #coord_cartesian(clip="off") +
  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme(legend.position = "top",
        plot.margin = margin(1, 0.5, 1, 1, unit = "cm"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  annotate(geom = "text", x = 0.7, y = 4,
           label = "Production\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = 0.9, y = 4,
           label = "Consumption\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = 0.8, y = 4,
           label = "<", size = 7) +
  annotate(geom = "text", x = 0.4, y = 20,
           label = "Consumption\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = 0.2, y = 20,
           label = "Production\nSustainability",
           size = 4.5) +
  annotate(geom = "text", x = 0.3, y = 20,
           label = ">", size = 7) 




#combine the things
library(patchwork)

bigp <- p5 + p4a + plot_layout(widths = c(3, 1)) + 
  plot_annotation(tag_levels = 'A', tag_prefix = "(", tag_suffix = ")")

bigp

png(filename="ConsumptionAngle/Figures/2_Top25RawDiff_Flow2.png",
    width=4000,
    height = 2500,
    res=300)

bigp

dev.off()
