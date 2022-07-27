#################################################################### Code Summary ##################################################
### TITLE:            SensAnalysis_AgeStratification.R
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022-04-07
### DESCRIPTION:      Code to assess the impact of imbalanced contact rates between population stratified into two different age groups: 
###                   <40 and 40+, on basic reproduction number 
### DATASETS USED:    contacts_all_prem.RData, poptotal_prem.RData, ISO3C_byRegion.xlsx
### DATASETS OUTPUT:  FigureS2a.RData
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
load("./Data/Raw Prem/contacts_all_prem.RData")  
load("./Data/Raw Prem/poptotal_prem.RData")


#################################################################### Transform population age structure ##############################

pop_subset = 
  subset(poptotal, iso3c %in% names(contact_all)) %>%         #remove country population data if contact rates were not produced by Prem
  arrange(iso3c); rm(poptotal)

pop_list =                                                    #Generate list of population vectors from prem pop data
  pop_subset[4:24] %>%                                        #remove unnecessary columns from dataframe
  split(1:nrow(pop_subset)) %>%                               #split rows of dataframe into list of vectors (per country)
  lapply(function(x){as.numeric(x)})                          #transform data frame to numeric vector class type

names(pop_list) = pop_subset$iso3c; rm(pop_subset)            #assign iso3c country codes to each pop vector

N_list = lapply(pop_list, function(x){transform_age2(x)})      #Transform age stratification to <40, 40+

#################################################################### Transform contact age structure ####################################

pop_list_16 = lapply(pop_list, function(x){                                                       #clean prem raw age data to match raw contact data (sum ages 75+)                                    
  
  
  output = vector(, length = 16)
  
  i = 1
  
  while(i <= 15){
    
    output[i] = x[i]
    i = i + 1
  }
  
  output[16] = sum(x[16:21])
  return(output)
  
}); rm(pop_list)  


pop_contacts_imbalanced_temp = intensive_to_extensive(c_list   = contact_all,                      #multiply contacts per person per day by age group population size to obtain population contacts   
                                                      pop_list = pop_list_16)
rm(list = c("pop_list_16", "contact_all"))  

pop_contacts_imbalanced      = lapply(pop_contacts_imbalanced_temp, function(x){transform_age2(x)}) #Transform age stratification of contact rates to <15, 15+
rm(pop_contacts_imbalanced_temp)

contacts_imbalanced          = extensive_to_intensive(pop_c_list = pop_contacts_imbalanced,        #Transform population contacts contacts per person per day
                                                      pop_list   = N_list)

#################################################################### Balance Contact Matrices #############################################

pop_contacts_balanced = lapply(pop_contacts_imbalanced, function(x){balance_pop_c_matrix(x)})   
contacts_balanced     = extensive_to_intensive(pop_contacts_balanced, N_list)

############################################ Sensitivity Analysis 1: Impact Basic Reproduction Number (new age stratification) ###################

### Balanced Reproduction numbers ###

R0_matrix_balanced = R0_matrix(contacts_balanced, N_list, beta, gamma)     
R0_yo_balanced     = lapply(R0_matrix_balanced, function(x){x[1,2]})       
R0_oy_balanced     = lapply(R0_matrix_balanced, function(x){x[2,1]})
R0_balanced        = R0_matrix_determinant(R0_matrix_balanced)

### Imbalanced Reproduction numbers ###

R0_matrix_imbalanced = R0_matrix(contacts_imbalanced, N_list, beta, gamma)
R0_yo_imbalanced     = lapply(R0_matrix_imbalanced, function(x){x[1,2]})
R0_oy_imbalanced     = lapply(R0_matrix_imbalanced, function(x){x[2,1]})
R0_imbalanced        = R0_matrix_determinant(R0_matrix_imbalanced)


### Direction and magnitude of imbalanced contacts (y-o) ###

C_yo_balanced   = lapply(pop_contacts_balanced, function(x){x[1,2]})
C_yo_imbalanced = lapply(pop_contacts_imbalanced, function(x){x[1,2]})
C_oy_imbalanced = lapply(pop_contacts_imbalanced, function(x){x[2,1]})

#Merge data into single dataframe

C_yo_balanced_df = 
  as.data.frame(C_yo_balanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_balanced")

C_yo_imbalanced_df = 
  as.data.frame(C_yo_imbalanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_imbalanced")

C_oy_imbalanced_df = 
  as.data.frame(C_oy_imbalanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_oy_imbalanced")

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
  as.data.frame(R0_imbalanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_imbalanced")

R0_yo_imbalanced_df = 
  as.data.frame(R0_yo_imbalanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_imbalanced")

R0_oy_imbalanced_df = 
  as.data.frame(R0_oy_imbalanced) %>%
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_imbalanced")

Impact_balancing_R0 = 
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


Impact_balancing_R0_sens1 = Impact_balancing_R0[c('iso3c', 'region', 'sub-region', 'intermediate-region', 'C_yo_balanced', 'C_yo_imbalanced', 'C_oy_imbalanced', 
                                            'R0_balanced', 'R0_yo_balanced', 'R0_oy_balanced','R0_imbalanced', 'R0_yo_imbalanced', 'R0_oy_imbalanced')]

save(Impact_balancing_R0_sens1, file = "./Output/Sensitivity_AgeStrat_R0.RData")
write_xlsx(Impact_balancing_R0_sens1, "./Output/Sensitivity_AgeStrat_R0.RData.xlsx")

############################################ Plot bias in basic reproduction number ###################

### Magnitude and direction of bias of R0 according to imbalance in C-oy and C-yo and ###

Impact_balancing_R0_sens1 = mutate(Impact_balancing_R0_sens1, 
                                   C_oy_imbalance = C_oy_imbalanced/C_yo_balanced,
                                   C_yo_imbalance = C_yo_imbalanced/C_yo_balanced)

summary(Impact_balancing_R0_sens1$R0_imbalanced/Impact_balancing_R0_sens1$R0_balanced)

##Supplementary Figure 3a: Sensitivity analysis.  Impact of imbalanced contact rates with different age stratification on R0

C_oy_ratio_GMB = subset(Impact_balancing_R0_sens1, iso3c =="GMB")[[c("C_oy_imbalance")]]
R0_ratio_GMB   = subset(Impact_balancing_R0_sens1, iso3c =="GMB")[[c("R0_imbalanced")]]/subset(Impact_balancing_R0_sens1, iso3c =="GMB")[[c("R0_balanced")]]
C_oy_ratio_LUX = subset(Impact_balancing_R0_sens1, iso3c =="LUX")[[c("C_oy_imbalance")]]
R0_ratio_LUX   = subset(Impact_balancing_R0_sens1, iso3c =="LUX")[[c("R0_imbalanced")]]/subset(Impact_balancing_R0_sens1, iso3c =="LUX")[[c("R0_balanced")]]
C_oy_ratio_SGP = subset(Impact_balancing_R0_sens1, iso3c =="SGP")[[c("C_oy_imbalance")]]
R0_ratio_SGP   = subset(Impact_balancing_R0_sens1, iso3c =="SGP")[[c("R0_imbalanced")]]/subset(Impact_balancing_R0_sens1, iso3c =="SGP")[[c("R0_balanced")]]

summary(Impact_balancing_R0_sens1$C_oy_imbalance)

FigureS3a = 
  ggplot(Impact_balancing_R0_sens1,
         aes(x = C_oy_imbalance,
             y = R0_imbalanced/R0_balanced)) +
  geom_point(size= 1.4, aes(colour = C_oy_imbalance)) +
  geom_point(aes(x=C_oy_ratio_GMB, y=R0_ratio_GMB), colour="black", size = 2.2) +
  annotate("text", x = 0.55, y = 0.945, label = "Gambia", colour = "black", size = 3.6) +
  geom_point(aes(x=C_oy_ratio_LUX, y=R0_ratio_LUX), colour="black", size = 2.2) +
  annotate("text", x = 1.0, y = 0.993, label = "Luxembourg", colour = "black", size = 3.6) +
  geom_point(aes(x=C_oy_ratio_SGP, y=R0_ratio_SGP), colour="black", size = 2.2) +
  annotate("text", x = 1.2, y = 0.975, label = "Singapore", colour = "black", size = 3.6) +
  theme_bw() +
  theme(legend.position = "right") +
  xlab(bquote(C['oy, imbal']~'/'~C['oy, bal'])) +
  ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +
  labs(color = bquote(C['oy, imbal']~'/'~C['oy, bal'])) +
  scale_x_continuous(limits = c(0.35, 1.6),
                     breaks = seq(0.35, 1.6, 0.4)) +
  scale_y_continuous(limits = c(0.92, 1.00),
                     breaks = seq(0.92, 1.00, 0.02)) + 
  scale_color_continuous_diverging("Blue-Red 3", trans = "log", limits = c(0.35, 2.9), breaks = c(0.5, 1, 2))

FigureS3a

FigureS3a_data = c("C_oy_ratio_GMB", "C_oy_ratio_LUX", "C_oy_ratio_SGP", "R0_ratio_GMB", "R0_ratio_LUX", "R0_ratio_SGP", "FigureS2a")

save(list = FigureS3a_data, file = "./Figures/FigureS3a.RData")
