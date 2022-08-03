########################################### Code Summary ##############################################
### TITLE:            Main.R
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022/03/03
### DESCRIPTION:      Code to produce results for each aim of study: 
###                     1. Aim 1: Compare R0 when contact rates are imbalanced vs. 
###                          balanced
###                     2. Aim 2: Compare epidemic trajectory when contact rates are 
###                         imbalanced vs. balanced
###                     3. Aim 3: Compare impact of an age-specific vaccination strategy
###                         when contact rates are imbalanced vs. balanced
### DATASETS USED:    Contacts.RData (Imbalanced and balanced contact matrices from Prem et al.), 
###                   Pop.Rdata (population data from Prem et al.), ISO3C_byRegion.xlsx (information on 
###                   country demographics and location by iso3c code)
### DATASETS OUTPUT:  Aim1_Impact_balancing_R0.RData, Aim1_Impact_balancing_R0.xlsx,
###                   Aim2_SimulationOutput.RData, Aim2_SimulationOutput.xlsx,
###                   Aim2_Cum_Infections_difference.RData, Aim2_Cum_Infections_difference.xlsx,
###                   Aim3_VaxImpact_strat1.RData, Aim3_VaxImpact_strat1.xlsx,
###                   Aim3_VaxImpact_strat2.RData, Aim3_VaxImpact_strat2.RData
############################################ Code Setup ###############################################

library(readr)
library(dplyr)
library(writexl)
library(readxl)

setwd("~/GitHub/imbalanced-contact-matrices")                #Set working directory

source("./Code/Functions.R")                                 #Load general functions
source("./Code/Model.R")                                     #Load SEIR model function and model parameters

iso3c_continent = read_xlsx("./Data/ISO3C_byRegion.xlsx")    #Load ISO3C codes per country
load("./Data/Contacts.RData")                                #Load imbalanced and balanced contact matrices for 177 demographic  settings from Prem 2021 (matrices cleaned from raw data in Contacts.R)
load("./Data/Pop.RData")                                     #Load Prem 2021 population data from 177 demographic settings (raw data cleaned in Contacts.R)

############################################ Part 1: Impact Basic Reproduction Number ###################

### Imbalanced Reproduction numbers ###

R0_matrix_imbalanced = R0_matrix(contacts_imbalanced, N_list, beta, gamma)       #Calculate baby R0s (i.e. R0 per age group and mixing pattern) per demographic settings using raw matrices and population data from Prem
R0_yo_imbalanced     = lapply(R0_matrix_imbalanced, function(x){x[1,2]})         #Pull out baby R0 among <15 from contact with 15+
R0_oy_imbalanced     = lapply(R0_matrix_imbalanced, function(x){x[2,1]})         #Pull out baby R0 among 15+ from contact with <15 
R0_imbalanced        = R0_matrix_determinant(R0_matrix_imbalanced)               #Calculate overall R0 (i.e. dominant eigenvalue of baby R0 matrix) per demographic setting studied in Prem 

### Balanced Reproduction numbers ###

R0_matrix_balanced = R0_matrix(contacts_balanced, N_list, beta, gamma)           #Calculate baby R0s (i.e. R0 per age group and mixing pattern) per demographic settings using balanced matrices (calculated in Contacts.R) and population data from Prem
R0_yo_balanced     = lapply(R0_matrix_balanced, function(x){x[1,2]})             #Pull out baby R0 among <15 from contact with 15+
R0_oy_balanced     = lapply(R0_matrix_balanced, function(x){x[2,1]})             #Pull out baby R0 among 15+ from contact with <15 
R0_balanced        = R0_matrix_determinant(R0_matrix_balanced)                   #Calculate overall R0 (i.e. dominant eigenvalue of baby R0 matrix) per demographic setting studied in Prem 


### Direction and magnitude of imbalanced contacts (y-o) ###

C_yo_balanced   = lapply(pop_contacts_balanced, function(x){x[1,2]})             #Pull out balanced population contacts between <15 and 15+ per demographic setting
C_yo_imbalanced = lapply(pop_contacts_imbalanced, function(x){x[1,2]})           #Pull out population contacts <15 reported with 15+ per demographic setting in Prem (imbalanced)
C_oy_imbalanced = lapply(pop_contacts_imbalanced, function(x){x[2,1]})           #Pull out population contacts 15+ reported with <15 per demogrpahic setting in Prem (imbalanced)

#Merge data into single dataframe

C_yo_balanced_df =                                                             
  as.data.frame(C_yo_balanced) %>%                                               #Transform list of balanced contacts between <15 and 15+ into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_balanced")    #Transform dataframe from wide to long format according to country ISO3C code

C_yo_imbalanced_df = 
  as.data.frame(C_yo_imbalanced) %>%                                             #Transform list of imbalanced contacts reported by <15 with 15+ into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_imbalanced")  #Transform dataframe from wide to long format according to country ISO3C code

C_oy_imbalanced_df = 
  as.data.frame(C_oy_imbalanced) %>%                                             #Transform list of imbalanced contacts reported by 15+ and <15 into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_oy_imbalanced")  #Transform dataframe from wide to long format according to country ISO3C code

R0_balanced_df = 
  as.data.frame(R0_balanced) %>%                                                 #Transform list of R0s when calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_balanced")      #Transform dataframe from wide to long format according to country ISO3C code

R0_yo_balanced_df = 
  as.data.frame(R0_yo_balanced) %>%                                              #Transform list baby R0s among <15 from contact with 15+ when calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_balanced")   #Transform dataframe from wide to long format according to country ISO3C code

R0_oy_balanced_df = 
  as.data.frame(R0_oy_balanced) %>%                                              #Transform list of baby R0s among 15+ from contact with <15 when calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_balanced")   #Transform dataframe from wide to long format according to country ISO3C code

R0_imbalanced_df = 
  as.data.frame(R0_imbalanced) %>%                                               #Transform list of R0s when calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_imbalanced")    #Transform dataframe from wide to long format according to country ISO3C code

R0_yo_imbalanced_df = 
  as.data.frame(R0_yo_imbalanced) %>%                                            #Transform list baby R0s among <15 from contact with 15+ when calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_imbalanced") #Transform dataframe from wide to long format according to country ISO3C code

R0_oy_imbalanced_df = 
  as.data.frame(R0_oy_imbalanced) %>%                                            #Transform list of baby R0s among 15+ from contact with <15 when calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_imbalanced") #Transform dataframe from wide to long format according to country ISO3C code 

Impact_balancing_R0 =                                                            #Merge all dataframes together
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
  mutate(C_oy_imbalance = C_oy_imbalanced/C_yo_balanced,                         #Calculate ratio of imbalanced contacts 15+ reported with <15 to balanced contacts between 15+ and <15
         C_yo_imbalance = C_yo_imbalanced/C_yo_balanced,                         #Calculate ratio of imbalanced contacts <15 reported with 15+ to balanced contacts between <15 and 15+ 
         R0_change = (R0_imbalanced - R0_balanced)/R0_balanced*100)              #Calculate percent change in R0 when calculated with imbalanced matrices versus balanced matrices

############################################ Part 2: Simulate Transmission Dynamics ######################################

### Run model under 3 contexts of imbalance - Gambia, Luxembourg, Singapore ###
#   NOTE: model functions outlined in Functions.R (model_function) and Model.R (seir.5) 


## Gambia ##
# Imbalanced contacts reported by 15+ with <15 were 0.45 times balanced contacts between <15 and 15+ (underestimated), 
#  and imbalanced contacts reported by <15 with 15+ were 1.55 times balanced contacts between <15 and 15 (overestimated)

#Gambia - balanced 
C = contacts_balanced[["GMB"]]; C                                                            #Assign balanced contact matrix from Gambia to C (C is used as parameter model_function)
context1_balanced_novax = model_function(iso3c = "GMB", balance = "balanced", vax = 0)       #Run model in Gambia when contact matrix is balanced and population is unvaccinated
summary(context1_balanced_novax)                                                             #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context1_balanced_vax1 = model_function(iso3c = "GMB", balance = "balanced", vax = 1)        #Run model in Gambia when contact matrix is balanced and 50% of population <15 is vaccinated
summary(context1_balanced_vax1)                                                              #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context1_balanced_vax2 = model_function(iso3c = "GMB", balance = "balanced", vax = 2)        #Run model in Gambia when contact matrix is balanced and 50% of population 15+ is vaccinated
summary(context1_balanced_vax2)                                                              #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

#Gambia - imbalanced
C = contacts_imbalanced[["GMB"]];                                                            #Assign imbalanced contact matrix from Gambia to C (C is used as parameter model_function)
context1_imbalanced_novax  = model_function(iso3c = "GMB", balance = "imbalanced", vax = 0)  #Run model in Gambia when contact matrix is imbalanced and population is unvaccinated
summary(context1_imbalanced_novax)                                                           #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context1_imbalanced_vax1  = model_function(iso3c = "GMB", balance = "imbalanced", vax = 1)   #Run model in Gambia when contact matrix is imbalanced and 50% of population <15 is vaccinated
summary(context1_imbalanced_vax1)                                                            #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context1_imbalanced_vax2  = model_function(iso3c = "GMB", balance = "imbalanced", vax = 2)   #Run model in Gambia when contact matrix is imbalanced and 50% of population 15+ is vaccinated
summary(context1_imbalanced_vax2)                                                            #Summarize model output to ensure compartment values do not fall below 0 or exceed population size
 

## Luxembourg ##
# Imbalanced contacts reported by 15+ with <15 were 0.99 times balanced contacts between <15 and 15+, 
#  and imbalanced contacts reported by <15 with 15+ were 1.01 times balanced contacts between <15 and 15 (relatively equal)

#Luxembourg - balanced
 
C = contacts_balanced[["LUX"]]                                                               #Assign balanced contact matrix from Luxembourg to C (C is used as parameter model_function)
context2_balanced_novax    = model_function(iso3c = "LUX", balance = "balanced", vax = 0)    #Run model in Luxembourg when contact matrix is balanced and population is unvaccinated
summary(context2_balanced_novax)                                                             #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context2_balanced_vax1    = model_function(iso3c = "LUX", balance = "balanced", vax = 1)     #Run model in Luxembourg when contact matrix is balanced and 50% of population <15 is vaccinated
summary(context2_balanced_vax1)                                                              #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context2_balanced_vax2    = model_function(iso3c = "LUX", balance = "balanced", vax = 2)     #Run model in Luxembourg when contact matrix is balanced and 50% of population 15+ is vaccinated
summary(context2_balanced_vax2)                                                              #Summarize model output to ensure compartment values do not fall below 0 or exceed population size


#Luxembourg - imbalanced 
C = contacts_imbalanced[["LUX"]]
context2_imbalanced_novax  = model_function(iso3c = "LUX", balance = "imbalanced", vax = 0)  #Run model in Luxembourg when contact matrix is imbalanced and population is unvaccinated
summary(context2_imbalanced_novax)                                                           #Summarize model output to ensure compartment values do not fall below 0 or exceed population size
 
context2_imbalanced_vax1  = model_function(iso3c = "LUX", balance = "imbalanced", vax = 1)   #Run model in Luxembourg when contact matrix is imbalanced and 50% of population <15 is vaccinated
summary(context2_imbalanced_vax1)                                                            #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context2_imbalanced_vax2  = model_function(iso3c = "LUX", balance = "imbalanced", vax = 2)   #Run model in Luxembourg when contact matrix is imbalanced and 50% of population 15+ is vaccinated
summary(context2_imbalanced_vax2)                                                            #Summarize model output to ensure compartment values do not fall below 0 or exceed population size


## Singapore ##
# Imbalanced contacts reported by 15+ with <15 were 1.52 times balanced contacts between <15 and 15+ (underestimated), 
#  and imbalanced contacts reported by <15 with 15+ were 0.48 times balanced contacts between <15 and 15 (overestimated)

#Singapore - balanced
C = contacts_balanced[["SGP"]]
context3_balanced_novax    = model_function(iso3c = "SGP", balance = "balanced", vax = 0)    #Run model in Singapore when contact matrix is balanced and population is unvaccinated
summary(context3_balanced_novax)                                                             #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context3_balanced_vax1    = model_function(iso3c = "SGP", balance = "balanced", vax = 1)     #Run model in Singapore when contact matrix is balanced and 50% of population <15 is vaccinated
summary(context3_balanced_vax1)                                                              #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context3_balanced_vax2    = model_function(iso3c = "SGP", balance = "balanced", vax = 2)     #Run model in Singapore when contact matrix is balanced and 50% of population 15+ is vaccinated
summary(context3_balanced_vax2)                                                              #Summarize model output to ensure compartment values do not fall below 0 or exceed population size


#Singapore - imbalanced
C = contacts_imbalanced[["SGP"]]
context3_imbalanced_novax  = model_function(iso3c = "SGP", balance = "imbalanced", vax = 0)  #Run model in Singapore when contact matrix is imbalanced and population is unvaccinated
summary(context3_imbalanced_novax)                                                           #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context3_imbalanced_vax1  = model_function(iso3c = "SGP", balance = "imbalanced", vax = 1)   #Run model in Singapore when contact matrix is imbalanced and 50% of population <15 is vaccinated
summary(context3_imbalanced_vax1)                                                            #Summarize model output to ensure compartment values do not fall below 0 or exceed population size

context3_imbalanced_vax2  = model_function(iso3c = "SGP", balance = "imbalanced", vax = 2)   #Run model in Singapore when contact matrix is imbalanced and 50% of population 15+ is vaccinated
summary(context3_imbalanced_vax2)                                                            #Summarize model output to ensure compartment values do not fall below 0 or exceed population size



### Model Check - Stability in population ###

#NOTE: model assumed closed population - population size should remain fixed
#NOTE: N_stability function defined in Functions.R

#No vaccination
N_stability_context1_balanced_novax   = N_stability(context1_balanced_novax);   N_stability_context1_balanced_novax     
N_stability_context1_imbalanced_novax = N_stability(context1_imbalanced_novax); N_stability_context1_imbalanced_novax
N_stability_context2_balanced_novax   = N_stability(context2_balanced_novax);   N_stability_context2_balanced_novax
N_stability_context2_imbalanced_novax = N_stability(context2_imbalanced_novax); N_stability_context2_imbalanced_novax
N_stability_context3_balanced_novax   = N_stability(context3_balanced_novax);   N_stability_context3_balanced_novax
N_stability_context3_imbalanced_novax = N_stability(context3_imbalanced_novax); N_stability_context3_imbalanced_novax

#Vaccine prioritized to <15
N_stability_context1_balanced_vax1   = N_stability(context1_balanced_vax1);   N_stability_context1_balanced_vax1
N_stability_context1_imbalanced_vax1 = N_stability(context1_imbalanced_vax1); N_stability_context1_imbalanced_vax1
N_stability_context2_balanced_vax1   = N_stability(context2_balanced_vax1);   N_stability_context2_balanced_vax1
N_stability_context2_imbalanced_vax1 = N_stability(context2_imbalanced_vax1); N_stability_context2_imbalanced_vax1
N_stability_context3_balanced_vax1   = N_stability(context3_balanced_vax1);   N_stability_context3_balanced_vax1
N_stability_context3_imbalanced_vax1 = N_stability(context3_imbalanced_vax1); N_stability_context3_imbalanced_vax1

#Vaccine prioritized to 15+
N_stability_context1_balanced_vax2   = N_stability(context1_balanced_vax2);   N_stability_context1_balanced_vax2
N_stability_context1_imbalanced_vax2 = N_stability(context1_imbalanced_vax2); N_stability_context1_imbalanced_vax2
N_stability_context2_balanced_vax2   = N_stability(context2_balanced_vax2);   N_stability_context2_balanced_vax2
N_stability_context2_imbalanced_vax2 = N_stability(context2_imbalanced_vax2); N_stability_context2_imbalanced_vax2
N_stability_context3_balanced_vax2   = N_stability(context3_balanced_vax2);   N_stability_context3_balanced_vax2
N_stability_context3_imbalanced_vax2 = N_stability(context3_imbalanced_vax2); N_stability_context3_imbalanced_vax2


############################################ Clean and output results ################################################################

### Aim 1 - R0 ###

Impact_balancing_R0 = Impact_balancing_R0[c('iso3c', 'region', 'sub-region', 'intermediate-region',              #Reorder variables in aim 1 output
                                            'C_yo_balanced', 'C_yo_imbalanced', 'C_oy_imbalanced', 
                                            'R0_balanced', 'R0_yo_balanced', 'R0_oy_balanced','R0_imbalanced', 
                                            'R0_yo_imbalanced', 'R0_oy_imbalanced', 'C_oy_imbalance', 
                                            'C_yo_imbalance', 'R0_change')]   

save(Impact_balancing_R0, file = "./Output/Aim1_Impact_balancing_R0.RData")                                      #Save data in RData format
write_xlsx(Impact_balancing_R0, "./Output/Aim1_Impact_balancing_R0.xlsx")                                        #Save data in xlsx format
 

### Aim 2 - Epidemic Trajectory ###

ModelOutput =                                                                                                    #Merge all model output together (organized by demographic setting, age group, 
  rbind(context1_balanced_novax,                                                                                 # balance of contact matrix and vaccination scenario)
        context1_balanced_vax1,                  
        context1_balanced_vax2,
        context1_imbalanced_novax,
        context1_imbalanced_vax1,
        context1_imbalanced_vax2,
        context2_balanced_novax,
        context2_balanced_vax1,
        context2_balanced_vax2,
        context2_imbalanced_novax,
        context2_imbalanced_vax1,
        context2_imbalanced_vax2,
        context3_balanced_novax,
        context3_balanced_vax1,
        context3_balanced_vax2,
        context3_imbalanced_novax,
        context3_imbalanced_vax1,
        context3_imbalanced_vax2) %>%
  mutate(CumI = CumIncid,                                                                                         #Set CumIncid to CumI (as modell really calculates the cumulative infections, not cumulative incidence, per age group)
         CumIncid = CumI/N*100000)                                                                                #Calculate cumulative infections per capita (i.e. cumulative incidence)

ModelOutput_cleaned =                                                                                             #Summarize compartmental data by time step, demographic setting, balance of contact matrix 
  ModelOutput %>%                                                                                                 # and vaccination scenario overall (rather than by age group) 
  arrange(time, context, balance, vax) %>%   
  group_by(time, context, balance, vax) %>%
  summarise(Age.Group = "Overall",
            N = sum(N), 
            S = sum(S),
            E = sum(E),
            I = sum(I),
            R = sum(R),
            CumI = sum(CumI)) %>% 
  mutate(CumIncid = CumI/N*100000) %>%                                                                            #Calculate cumulative infections per capita (i.e. cumulative incidence) per time step      
  rbind(ModelOutput) %>%                                                                                          #Merge summarized compartment data overall, with compartment data per age group
  arrange(time, context, balance, Age.Group, vax)                                                                 #arrange data by time step, demographic setting, balance of matrix, age group and vaccination scenario


CumI_difference_novax =                                                                                           #Calculate percent difference in cumulative infections from epidemic model with balanced versus imbalanced contact matrix
  subset(ModelOutput_cleaned, time == 365 & vax == 0)[c("Age.Group", "context", "vax", "balance", "CumI")] %>%    #Subset model output after 1 year of transmission
  pivot_wider(names_from = balance, values_from = CumI) %>%                                                       #Transform data from long to wide format
  rename(CumI_bal = balanced, CumI_imbal = imbalanced) %>%      
  mutate(CumI_percent_change = (CumI_imbal - CumI_bal)/CumI_bal*100)                                              #Calculate percent difference in cumulative infections after one year of transmission when modelled with imbalanced versus balanced matrices


save(ModelOutput_cleaned, file = "./Output/Aim2_SimulationOutput.RData")                                          #save model output in RData format
write_xlsx(ModelOutput_cleaned, "./Output/Aim2_SimulationOutput.xlsx")                                            #save model output data in xlsx format 

save(CumI_difference_novax, file = "./Output/Aim2_Cum_Infections_difference.RData")                               #save percent difference in cumulative infections in RData format
write_xlsx(CumI_difference_novax, "./Output/Aim2_Cum_Infections_difference.xlsx")                                 #save percent difference in cumulative infections in xlsx format




### Aim 3 - Impact of an age-specific vaccination strategy ###

Impact_vax1  =                                                                                                    #Calculate difference in impact of vaccination strategy 1 when modeled with imbalanced versus balanced matrices
  subset(ModelOutput_cleaned, time == 365 & !(vax == 2))[c("context", "Age.Group", "balance", "vax", "CumI")] %>% #Subset model output 1 year after seeding model, when 50% of population <15 was vaccinated prior to seeding 
  
  pivot_wider(names_from = c("balance", "vax"),                                                                   #Transform data from long to wide format
              values_from = "CumI") %>%
  
  mutate(Inf_averted_bal   = balanced_0 - balanced_1,                                                             #Calculate infections averted under balanced conditions (Cumulative infections without vaccination - cumulative infections with)
         Inf_averted_imbal = imbalanced_0 - imbalanced_1,                                                         #Calculate infections averted under imbalanced conditions (Cumulative infections without vaccination - cumulative infections with)
         Diff_inf_averted = (Inf_averted_imbal-Inf_averted_bal)/Inf_averted_bal*100)                              #Calculate percent difference in infections averted 


Impact_vax2  =                                                                                                    #Calculate difference in impact of vaccination strategy 2 when modeled with imbalanced versus balanced matrices
  subset(ModelOutput_cleaned, time == 365 & !(vax == 1))[c("context", "Age.Group", "balance", "vax", "CumI")] %>% #Subset model output 1 year after seeding model, when 50% of population 15+ was vaccinated prior to seeding 
  
  pivot_wider(names_from = c("balance", "vax"),                                                                   #Transform data from long to wide format
              values_from = "CumI") %>% 
  
  mutate(Inf_averted_bal   = balanced_0 - balanced_2,                                                             #Calculate infections averted under balanced conditions (Cumulative infections without vaccination - cumulative infections with)   
         Inf_averted_imbal = imbalanced_0 - imbalanced_2,                                                         #Calculate infections averted under imbalanced conditions (Cumulative infections without vaccination - cumulative infections with)
         Diff_inf_averted = (Inf_averted_imbal-Inf_averted_bal)/Inf_averted_bal*100)                              #Calculate percent difference in infections averted 


save(Impact_vax1, file = "./Output/Aim3_VaxImpact_strat1.RData")                                                  #save percent difference in infections averted from vax strategy 1 in RData format 
write_xlsx(Impact_vax1, "./Output/Aim3_VaxImpact_strat1.xlsx")                                                    #save percent difference in infections averted from vax strategy 1 in RData format 

save(Impact_vax2, file = "./Output/Aim3_VaxImpact_strat2.RData")                                                  #save percent difference in infections averted from vax strategy 2 in RData format 
write_xlsx(Impact_vax2, "./Output/Aim3_VaxImpact_strat2.xlsx")                                                    #save percent difference in infections averted from vax strategy 2 in RData format 
