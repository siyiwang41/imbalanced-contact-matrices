#################################################################### Code Summary ##################################################
### TITLE:            SensAnalysis_BiasDirection
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022-04-07
### DESCRIPTION:      Code to assess the impact of imbalanced contact rates (imbalanced opposite to trends observed in Prem et al.) 
###                     on basic reproduction number
### DATASETS USED:    Contacts.RData, Pop.RData, Aim1_Impact_balancing_R0.RData, ISO3C_byRegion.xlsx, FigureS2a.RData
### DATASETS OUTPUT:  FigureS2b.RData, Figure2.png
#################################################################### Code Setup ####################################################

setwd("~/GitHub/Balancing_C_Impact2")

library(dplyr)
library(tidyr)
library(writexl)
library(tibble)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library("viridis")
library(wesanderson)
library(ggsci)
library(rworldmap)
library(gridGraphics)
library(cowplot)
library(colorspace)

source("./Code/Functions.R")
source("./Code/Model.R")

iso3c_continent = read_xlsx("./Data/ISO3C_byRegion.xlsx")
load("./Data/Pop.RData")
load("./Data/Contacts.RData")
load("./Output/Aim1_Impact_balancing_R0.RData")
load("./Figures/FigureS3a.RData")

#################################################################### Change directionality of bias in C #################################

pop_contacts_imbalanced_2 = lapply(pop_contacts_imbalanced, function(x){change_bias_direction_c(x)})
contacts_imbalanced_2 = extensive_to_intensive(pop_contacts_imbalanced_2, N_list)

#################################################################### Aim 1: assess impact on R0 #################################

R0_matrix_balanced = R0_matrix(contacts_balanced, N_list, beta, gamma)     
R0_yo_balanced     = lapply(R0_matrix_balanced, function(x){x[1,2]})       
R0_oy_balanced     = lapply(R0_matrix_balanced, function(x){x[2,1]})
R0_balanced        = R0_matrix_determinant(R0_matrix_balanced)

### Imbalanced Reproduction numbers (with new direction of bias) ###

R0_matrix_imbalanced_2 = R0_matrix(contacts_imbalanced_2, N_list, beta, gamma)
R0_yo_imbalanced_2     = lapply(R0_matrix_imbalanced_2, function(x){x[1,2]})
R0_oy_imbalanced_2     = lapply(R0_matrix_imbalanced_2, function(x){x[2,1]})
R0_imbalanced_2        = R0_matrix_determinant(R0_matrix_imbalanced_2)

### Direction and magnitude of imbalanced contacts (y-o) ###

C_yo_balanced     = lapply(pop_contacts_balanced, function(x){x[1,2]})
C_yo_imbalanced_2 = lapply(pop_contacts_imbalanced_2, function(x){x[1,2]})
C_oy_imbalanced_2 = lapply(pop_contacts_imbalanced_2, function(x){x[2,1]})

#Merge data into single dataframe

C_yo_balanced_df = 
  as.data.frame(C_yo_balanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_balanced")

C_yo_imbalanced_df = 
  as.data.frame(C_yo_imbalanced_2) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_imbalanced_2")

C_oy_imbalanced_df = 
  as.data.frame(C_oy_imbalanced_2) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_oy_imbalanced_2")

R0_balanced_df = 
  as.data.frame(R0_balanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_balanced")

R0_yo_balanced_df = 
  as.data.frame(R0_yo_balanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_balanced")

R0_oy_balanced_df = 
  as.data.frame(R0_oy_balanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_balanced")

R0_imbalanced_df = 
  as.data.frame(R0_imbalanced_2) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_imbalanced_2")

R0_yo_imbalanced_df = 
  as.data.frame(R0_yo_imbalanced_2) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_imbalanced_2")

R0_oy_imbalanced_df = 
  as.data.frame(R0_oy_imbalanced_2) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_imbalanced_2")

Impact_balancing_R0_sens2 = 
  merge(C_yo_balanced_df,
        C_yo_imbalanced_df) %>%
  merge(C_oy_imbalanced_df) %>%
  merge(R0_balanced_df) %>%
  merge(R0_yo_balanced_df) %>%
  merge(R0_oy_balanced_df) %>%
  merge(R0_imbalanced_df) %>%
  merge(R0_yo_imbalanced_df) %>%
  merge(R0_oy_imbalanced_df) %>%
  merge(iso3c_continent, by = "iso3c", all.x = T)


Impact_balancing_R0_sens2 = Impact_balancing_R0_sens2[c('iso3c', 'region', 'sub-region', 'intermediate-region', 'C_yo_balanced', 'C_yo_imbalanced_2', 'C_oy_imbalanced_2', 
                                            'R0_balanced', 'R0_yo_balanced', 'R0_oy_balanced','R0_imbalanced_2', 'R0_yo_imbalanced_2', 'R0_oy_imbalanced_2')] %>%
  mutate(C_oy_imbalance_2 = C_oy_imbalanced_2/C_yo_balanced,
         C_yo_imbalance_2 = C_yo_imbalanced_2/C_yo_balanced)

save(Impact_balancing_R0_sens2, file = "./Output/Sensitivity_BiasDirection_R0.RData")
write_xlsx(Impact_balancing_R0_sens2, "./Output/Sensitivity_BiasDirection_R0.RData.xlsx")

C_oy_ratio_GMB_plotb = subset(Impact_balancing_R0_sens2, iso3c =="GMB")[[c("C_oy_imbalance_2")]]
R0_ratio_GMB_plotb   = subset(Impact_balancing_R0_sens2, iso3c =="GMB")[[c("R0_imbalanced_2")]]/subset(Impact_balancing_R0_sens2, iso3c =="GMB")[[c("R0_balanced")]]
C_oy_ratio_LUX_plotb = subset(Impact_balancing_R0_sens2, iso3c =="LUX")[[c("C_oy_imbalance_2")]]
R0_ratio_LUX_plotb   = subset(Impact_balancing_R0_sens2, iso3c =="LUX")[[c("R0_imbalanced_2")]]/subset(Impact_balancing_R0_sens2, iso3c =="LUX")[[c("R0_balanced")]]
C_oy_ratio_SGP_plotb = subset(Impact_balancing_R0_sens2, iso3c =="SGP")[[c("C_oy_imbalance_2")]]
R0_ratio_SGP_plotb   = subset(Impact_balancing_R0_sens2, iso3c =="SGP")[[c("R0_imbalanced_2")]]/subset(Impact_balancing_R0_sens2, iso3c =="SGP")[[c("R0_balanced")]]

summary(Impact_balancing_R0_sens2$C_oy_imbalance_2)

FigureS3b = 
  ggplot(Impact_balancing_R0_sens2,
         aes(x = C_oy_imbalance_2,
             y = R0_imbalanced_2/R0_balanced)) +
  geom_point(size= 1.4, aes(colour = C_oy_imbalance_2)) +
  geom_point(aes(x=C_oy_ratio_GMB_plotb, y=R0_ratio_GMB_plotb), colour="black", size = 2.2) +
  annotate("text", x = 1.42, y = 0.9425, label = "Gambia", colour = "black", size = 3.6) +
  geom_point(aes(x=C_oy_ratio_LUX_plotb, y=R0_ratio_LUX_plotb), colour="black", size = 2.2) +
  annotate("text", x = 1.0, y = 0.993, label = "Luxembourg", colour = "black", size = 3.6) +
  geom_point(aes(x=C_oy_ratio_SGP_plotb, y=R0_ratio_SGP_plotb), colour="black", size = 2.2) +
  annotate("text", x = 0.7, y = 0.97, label = "Singapore", colour = "black", size = 3.6) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab(bquote(C['oy, imbal']~'/'~C['oy, bal'])) +
  ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +
  labs(color = bquote(C['oy, imbal']~'/'~C['oy, bal'])) +
  scale_x_continuous(limits = c(0.4, 1.6),
                     breaks = seq(0.4, 1.6, 0.4)) +
  scale_y_continuous(limits = c(0.94, 1.00),
                     breaks = seq(0.94, 1.00, 0.02)) + 
  scale_color_continuous_diverging("Blue-Red 3", trans = "log", limits = c(0.35, 2.9), breaks = c(0.5, 1, 2))


FigureS3b

save(FigureS3b, file = "./Figures/FigureS3b.RData")


FigureS3 = ggarrange(FigureS3a,
                     FigureS3b + rremove("ylab"),
                     align = "hv",
                     nrow = 1, 
                     ncol = 2,
                     labels = "AUTO",
                     common.legend = TRUE,
                     legend = "right")


FigureS3

ggsave(file= paste0("./Figures/FigureS3.png"), 
       width = 10, height = 3.7, limitsize = FALSE, FigureS3)


