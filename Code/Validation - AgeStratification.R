#################################################################### Code Summary ##################################################
### TITLE:            Validation_AgeStratification.R
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022-04-07
### DESCRIPTION:      Code to assess the impact of imbalanced contact rates between population stratified into two different age groups: 
###                   <40 and 40+, on basic reproduction number 
### DATASETS USED:    contacts_all_prem.RData, poptotal_prem.RData, ISO3C_byRegion.xlsx
### DATASETS OUTPUT:  Validity_AgeStrat_R0.RData, Validity_AgeStrat_R0.xlsx, FigureS3a.RData
#################################################################### Code Setup ####################################################

library(dplyr)
library(tidyr)
library(tibble)
library(writexl)
library(ggplot2)
library(ggpubr)
library(colorspace)

setwd("~/GitHub/imbalanced-contact-matrices")                   #Set working directory

source("./Code/Functions.R")                                    #Load general functions
source("./Code/Model.R")                                        #Load SEIR model function and model parameters

iso3c_continent = read_xlsx("./Data/ISO3C_byRegion.xlsx")       #Load ISO3C codes per country across the world
load("./Data/Raw Prem/contacts_all_prem.RData")                 #Load raw contact data from Prem
load("./Data/Raw Prem/poptotal_prem.RData")                     #Load population data from Prem 


#################################################################### Transform population age structure ##############################

pop_subset = 
  subset(poptotal, iso3c %in% names(contact_all)) %>%         #remove country population data if contact rates were not produced by Prem
  arrange(iso3c); rm(poptotal)

pop_list =                                                    #Generate list of population vectors from prem pop data
  pop_subset[4:24] %>%                                        #remove unnecessary columns from dataframe
  split(1:nrow(pop_subset)) %>%                               #split rows of dataframe into list of vectors (per country)
  lapply(function(x){as.numeric(x)})                          #transform data frame to numeric vector class type

names(pop_list) = pop_subset$iso3c; rm(pop_subset)            #assign iso3c country codes to each pop vector

N_list = lapply(pop_list, function(x){transform_age2(x)})     #Transform age stratification to <40 and 40+

#################################################################### Transform contact age structure ####################################

pop_list_16 = lapply(pop_list, function(x){     #Clean prem raw age data to match raw contact data (sum ages 75+)                                                                                      
                                                   
                 
  output = vector(, length = 16)                #Create empty vector to save new transformed population data in
  
  i = 1
  
  while(i <= 15){                               #Iterate through raw population vector from Prem
    
    output[i] = x[i]                            #For all vector elements from age groups <75, output original value
    i = i + 1
  }
  
  output[16] = sum(x[16:21])                    #Sum vector elements for age groups 75+ and save in last vector element of output
  return(output)
  
}); rm(pop_list)                                #Remove raw Prem population data from envrionemnt (no longer needed in analysis)


pop_contacts_imbalanced_temp = intensive_to_extensive(c_list   = contact_all,                       #Multiply contacts per person per day by age group population size to obtain population contacts   
                                                      pop_list = pop_list_16)                       # using intensive_to_extensive function (in Functions.R code)

rm(list = c("pop_list_16", "contact_all"))                                                          #Remove unnecessary data from environment 

pop_contacts_imbalanced      = lapply(pop_contacts_imbalanced_temp, function(x){transform_age2(x)}) #Transform age stratification of contact rates to <40, 40+ using transform_age2 function (in Functions.R)

rm(pop_contacts_imbalanced_temp)                                                                    #Remove unnecessary data from environment

contacts_imbalanced          = extensive_to_intensive(pop_c_list = pop_contacts_imbalanced,         #Transform population contacts to contacts per person per day using extensive_to_intensive
                                                      pop_list   = N_list)                          # function (in Functions.R)

#################################################################### Balance Contact Matrices #############################################

pop_contacts_balanced = lapply(pop_contacts_imbalanced, function(x){balance_pop_c_matrix(x)})       #Balance population level age-transformed contact matrices from Prem using balance_pop_c_matrix 
                                                                                                    # function (in functions.R code)
contacts_balanced     = extensive_to_intensive(pop_contacts_balanced, N_list)                       #Transform balanced population contacts to contacts per person per day using extensive_to_intesnive
                                                                                                    # function (in Functions.R)
pop_contacts_imbalanced[[1]]                                                                        #Function check to ensure population contacts are balanced correctly
pop_contacts_balanced[[1]]

############################################ Validation 1: Impact Basic Reproduction Number (new age stratification) ###################

### Imbalanced Reproduction numbers ###

R0_matrix_imbalanced = R0_matrix(contacts_imbalanced, N_list, beta, gamma)       #Calculate baby R0s (i.e. R0 per age group and mixing pattern) per demographic setting using raw matrices and population data from Prem
R0_yo_imbalanced     = lapply(R0_matrix_imbalanced, function(x){x[1,2]})         #Save baby R0 among <40 from contact with 40+
R0_oy_imbalanced     = lapply(R0_matrix_imbalanced, function(x){x[2,1]})         #Save baby R0 among 40+ from contact with <40
R0_imbalanced        = R0_matrix_determinant(R0_matrix_imbalanced)               #Calculate overall R0 (i.e. dominant eigenvalue of baby R0 matrix) per demographic setting studied in Prem

### Balanced Reproduction numbers ###

R0_matrix_balanced = R0_matrix(contacts_balanced, N_list, beta, gamma)           #Calculate baby R0s (i.e. R0 per age group and mixing pattern) per demographic setting using balanced matrices and population data from Prem
R0_yo_balanced     = lapply(R0_matrix_balanced, function(x){x[1,2]})             #Save baby R0 among <40 from contact with 40+
R0_oy_balanced     = lapply(R0_matrix_balanced, function(x){x[2,1]})             #Save baby R0 among 40+ from contact with <40
R0_balanced        = R0_matrix_determinant(R0_matrix_balanced)                   #Calculate overall R0 (i.e. dominant eigenvalue of baby R0 matrix) per demographic setting studied in Prem

### Direction and magnitude of imbalanced contacts (y-o) ###

C_yo_balanced   = lapply(pop_contacts_balanced, function(x){x[1,2]})             #Create list of balanced population contacts between <40 and 40+ per demographic setting in Prem
C_yo_imbalanced = lapply(pop_contacts_imbalanced, function(x){x[1,2]})           #Create list of population contacts <40 reported with 40+ per demographic setting in Prem (imbalanced)  
C_oy_imbalanced = lapply(pop_contacts_imbalanced, function(x){x[2,1]})           #Create list of population contacts 40+ reported with <40 per demographic setting in Prem (imbalanced)

#Merge lists of balanced and imbalanced contacts between <40 and 40+, and R0s into single dataframe

C_yo_balanced_df =                                                                 
  as.data.frame(C_yo_balanced) %>%                                               #Transform list of balanced contacts between <40 and 40+ into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_balanced")    #Transform dataframe from wide to long format according to country ISO3C code

C_yo_imbalanced_df = 
  as.data.frame(C_yo_imbalanced) %>%                                             #Transform list of imbalanced contacts <40 reported with 40+ into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_imbalanced")  #Transform dataframe from wide to long format according to country ISO3C code

C_oy_imbalanced_df = 
  as.data.frame(C_oy_imbalanced) %>%                                             #Transform list of imbalanced contacts 40+ reported with <40 into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_oy_imbalanced")  #Transform dataframe from wide to long format according to country ISO3C code

R0_balanced_df = 
  as.data.frame(R0_balanced) %>%                                                 #Transform list of R0s calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_balanced")      #Transform dataframe from wide to long format according to country ISO3C code

R0_yo_balanced_df = 
  as.data.frame(R0_yo_balanced) %>%                                              #Transform list of baby R0s among <40 from contact with 40+ calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_balanced")   #Transform dataframe from wide to long format according to country ISO3C code

R0_oy_balanced_df = 
  as.data.frame(R0_oy_balanced) %>%                                              #Transform list of baby R0s among 40+ from contact with <40 calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_balanced")   #Transform dataframe from wide to long format according to country ISO3C code

R0_imbalanced_df = 
  as.data.frame(R0_imbalanced) %>%                                               #Transform list of R0s calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_imbalanced")    #Transform dataframe from wide to long format according to country ISO3C code

R0_yo_imbalanced_df = 
  as.data.frame(R0_yo_imbalanced) %>%                                            #Transform list of baby R0s among <40 from contact with 40+ calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_imbalanced") #Transform dataframe from wide to long format according to country ISO3C code

R0_oy_imbalanced_df = 
  as.data.frame(R0_oy_imbalanced) %>%                                            #Transform list of baby R0s among 40+ from contact with <40 calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_imbalanced") #Transform dataframe from wide to long format according to country ISO3C code

Impact_balancing_R0_valid1 =                                                      #Merge all dataframes together
  merge(C_yo_balanced_df,
        C_yo_imbalanced_df) %>%
  merge(C_oy_imbalanced_df) %>%
  merge(R0_balanced_df) %>%
  merge(R0_yo_balanced_df) %>%
  merge(R0_oy_balanced_df) %>%
  merge(R0_imbalanced_df) %>%
  merge(R0_yo_imbalanced_df) %>%
  merge(R0_oy_imbalanced_df) %>%
  merge(iso3c_continent, by = "iso3c", all.x = T) %>%
  mutate(C_oy_imbalance_ratio = C_oy_imbalanced/C_yo_balanced,                   #Calculate ratio of imbalanced contacts 40+ reported with <40 to balanced contacts between 40+ and <40
         C_yo_imbalance_ratio = C_yo_imbalanced/C_yo_balanced,                   #Calculate ratio of imbalanced contacts <40 reported with 40+ to balanced contacts between 40+ and <40
         R0_oy_imbalance_ratio = R0_oy_imbalanced/R0_oy_balanced,                #Calculate ratio of imbalanced to balanced baby R0 among 40+ from contact with <40 
         R0_yo_imbalance_ratio = R0_yo_imbalanced/R0_yo_balanced,                #Calculate ratio of imbalanced to balanced baby R0 among <40 from contact with 40+ 
         R0_change = (R0_imbalanced - R0_balanced)/R0_balanced*100) %>%          #Calculate percent change in R0 when calculated with imabalanced versus balanced contact matrices
  select(iso3c, region, `sub-region`, `intermediate-region`, C_yo_balanced,      #Reorder columns of dataframe
         C_yo_imbalanced, C_oy_imbalanced,  C_yo_imbalance_ratio,  
         C_oy_imbalance_ratio, R0_balanced, R0_yo_balanced, R0_oy_balanced,
         R0_imbalanced, R0_yo_imbalanced, R0_oy_imbalanced, R0_yo_imbalance_ratio,
         R0_oy_imbalance_ratio, R0_change)

save(Impact_balancing_R0_valid1, file = "./Output/Validity_AgeStrat_R0.RData")      #Save data in RData format
write_xlsx(Impact_balancing_R0_valid1, "./Output/Validity_AgeStrat_R0.RData.xlsx")  #Save data in xlsx format

############################################ Plot bias in basic reproduction number ###################

### Supplementary Figure 3a: Underestimation of R0 is robust to changes in age group stratification. ###

#Assign ratios of Coy (population contacts 40+ reported with <40) and R0 (imbalanced/balanced) to variables for easy annotations in ggplot Figure S3a code
C_oy_ratio_GMB = subset(Impact_balancing_R0_valid1, iso3c =="GMB")[[c("C_oy_imbalance_ratio")]]    #Assign Gambia C_oy_ratio (imbalanced/balanced) to variable
R0_ratio_GMB   = subset(Impact_balancing_R0_valid1, iso3c =="GMB")[[c("R0_imbalanced")]]/          #Assign Gambia R0 ratio (imbalanced/balanced) to variable
  subset(Impact_balancing_R0_valid1, iso3c =="GMB")[[c("R0_balanced")]]
C_oy_ratio_LUX = subset(Impact_balancing_R0_valid1, iso3c =="LUX")[[c("C_oy_imbalance_ratio")]]    #Assign Luxembourg C_oy_ratio (imbalanced/balanced) to variable
R0_ratio_LUX   = subset(Impact_balancing_R0_valid1, iso3c =="LUX")[[c("R0_imbalanced")]]/          #Assign Luxembourg R0 ratio (imbalanced/balanced) to variable
  subset(Impact_balancing_R0_valid1, iso3c =="LUX")[[c("R0_balanced")]]
C_oy_ratio_SGP = subset(Impact_balancing_R0_valid1, iso3c =="SGP")[[c("C_oy_imbalance_ratio")]]    #Assign Singapore C_oy_ratio  (Imbalnaced/balanced) to variable
R0_ratio_SGP   = subset(Impact_balancing_R0_valid1, iso3c =="SGP")[[c("R0_imbalanced")]]/          #Assign Singapore R0 ratio (imbalanced/balanced) to variable
  subset(Impact_balancing_R0_valid1, iso3c =="SGP")[[c("R0_balanced")]]

summary(Impact_balancing_R0_valid1$C_oy_imbalance_ratio)                                           #Evaluate descriptive stats of C_oy_ratio to determine x axis range for FigureS3a

FigureS3a =                                                                                        #Plot FigureS3a (relationship between imbalance in Coy and R0 with new age stratification)
  ggplot(Impact_balancing_R0_valid1,
         aes(x = C_oy_imbalance_ratio,                                                             #Plot imbalance in contacts 40+ reported with <40 on x axis
             y = R0_imbalanced/R0_balanced)) +                                                     #Plot imbalanced to balanced R0 ratio on y axis
  geom_point(size= 1.4, aes(colour = C_oy_imbalance_ratio)) +                                      #Colour data points according to imbalance in C_oy
  geom_point(aes(x=C_oy_ratio_GMB, y=R0_ratio_GMB), colour="black", size = 2.2) +                  #Highlight Gambia data point
  annotate("text", x = 0.55, y = 0.945, label = "Gambia", colour = "black", size = 3.6) +          #Annotate Gambia data point
  geom_point(aes(x=C_oy_ratio_LUX, y=R0_ratio_LUX), colour="black", size = 2.2) +                  #Highlight Luxembourg data point
  annotate("text", x = 1.0, y = 0.993, label = "Luxembourg", colour = "black", size = 3.6) +       #Annotate Luxembourg data point
  geom_point(aes(x=C_oy_ratio_SGP, y=R0_ratio_SGP), colour="black", size = 2.2) +                  #Highlight Singapore data point
  annotate("text", x = 1.2, y = 0.975, label = "Singapore", colour = "black", size = 3.6) +        #Annotate Singapore data point
  theme_bw() +
  theme(legend.position = "right") +                                                               #Position colour legend on right side of plot
  xlab(bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                  #Label x axis
  ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +                                                    #Label y axis
  labs(color = bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                          #Label legend
  scale_x_continuous(limits = c(0.35, 1.6),                                                        #Fix x axis range and break points
                     breaks = seq(0.35, 1.6, 0.4)) + 
  scale_y_continuous(limits = c(0.92, 1.00),                                                       #Fix y axis range and break points
                     breaks = seq(0.92, 1.00, 0.02)) + 
  scale_color_continuous_diverging("Blue-Red 3", trans = "log",                                    #Manually colour data points according to extent of imbalance in C_oy (according to fixed colour scale)
                                   limits = c(0.35, 2.9), breaks = c(0.5, 1, 2))

FigureS3a

FigureS3a_data = c("C_oy_ratio_GMB", "C_oy_ratio_LUX", "C_oy_ratio_SGP",                           #Save figure data (to be used to merge with FigureS3b in Validation-BiasDirection.R)
                   "R0_ratio_GMB", "R0_ratio_LUX", "R0_ratio_SGP", "FigureS3a")
save(list = FigureS3a_data, file = "./Figures/FigureS3a.RData")
