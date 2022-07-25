#################################################################### Code Summary ##################################################
### TITLE:            Contacts.R
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022/03/03
### DESCRIPTION:      Code to clean prem population and contact data including: age transformation and balancing of contacts
### DATASETS USED:    Contacts_all_prem.RData, poptotal_prem.RData
### DATASETS OUTPUT:  Pop.RData; Contacts.RData, Contacts_scenarios.RData, Contacts_scenarios.xlsx
#################################################################### Code Setup ####################################################

library(dplyr)
library(tidyr)
library(tibble)
library(writexl)

setwd("~/GitHub/Balancing_C_Impact2")           #set working directory

source("./Code/Functions.R")                    #load functions

load("./Data/Raw Prem/contacts_all_prem.RData") #load raw contact data from Prem
load("./Data/Raw Prem/poptotal_prem.RData")     #load population data from Prem


#################################################################### Transform population age structure ##############################

pop_subset = 
  subset(poptotal, iso3c %in% names(contact_all)) %>%         #remove country population data if contact rates were not produced by Prem
  arrange(iso3c); rm(poptotal)

pop_list =                                                    #Generate list of population vectors from prem pop data
  pop_subset[4:24] %>%                                        #remove unneccesary columns from dataframe
  split(1:nrow(pop_subset)) %>%                               #split rows of dataframe into list of vectors (per country)
  lapply(function(x){as.numeric(x)})                          #transform data frame to numeric vector class type

names(pop_list) = pop_subset$iso3c; rm(pop_subset)            #assign iso3c country codes to each pop vector

N_list = lapply(pop_list, function(x){transform_age(x)})      #Transform age stratification to <15, 15+ using transform_age function (in Functions.R)

save(N_list, file = "./Data/Pop.RData")                       #Save data


#################################################################### Transform contact age structure ####################################

pop_list_16 = lapply(pop_list, function(x){   #iterate through list of population vectors from Prem and clean to match raw contact data (i.e. need to sum population aged 75+)                                    
  
  output = vector(, length = 16)              #create empty vector to save new transformed population data in 
  
  i = 1
  
  while(i <= 15){                             #iterate through raw population vector from Prem
    
    output[i] = x[i]                          #for all vector elements based on age groups <75 output original value                                         
    i = i + 1
  }
  
  output[16] = sum(x[16:21])                  #sum vector elements for age groups 75+ and save in last vector element
  return(output)
  
  }); rm(pop_list)                            #remove raw Prem population data (no longer needed in analysis)


pop_contacts_imbalanced_temp = intensive_to_extensive(c_list   = contact_all,                      #multiply contacts per person per day by age group population size to obtain population contacts   
                                                      pop_list = pop_list_16)                      #    using intensive_to_extensive function (in Functions.R code)
rm(list = c("pop_list_16", "contact_all"))                                                         #Remove unnecesary data

pop_contacts_imbalanced      = lapply(pop_contacts_imbalanced_temp, function(x){transform_age(x)}) #Transform age stratification of contact rates to <15, 15+ using transform_age function (in Functions.R)
rm(pop_contacts_imbalanced_temp)                                                                   #Remove unnecessary data

contacts_imbalanced          = extensive_to_intensive(pop_c_list = pop_contacts_imbalanced,        #Transform population contacts per person per day using extensive_to_intensive function (in Functions.R)
                                                      pop_list   = N_list)


#################################################################### Balance Contact Matrices #############################################

pop_contacts_balanced = lapply(pop_contacts_imbalanced, function(x){balance_pop_c_matrix(x)})                                                   #Balance population level contact matrix using balance_pop_c_matrix function (in Functions.R)
contacts_balanced     = extensive_to_intensive(pop_contacts_balanced, N_list)                                                                   #Transform balanced population level matrix to contacts per person per day using extensive_to_intensive function 

save(list = c("contacts_balanced", "pop_contacts_balanced", "contacts_imbalanced", "pop_contacts_imbalanced"), file = "./Data/Contacts.RData")  #Save balanced and imbalanced lists of contact matrices in Contacts.RData


############################################################## Generate C data frame for 3 scenarios #######################################

#Code to create a data frame of balanced and imbalanced contact rates in three demographic settings where contacts 15+ reported with <15 were: 1) larger than (Singapore), 
# 2) equal to (Luxembourg), or 3) less than (Gambia) balanced contacts between 15+ and <15.  This output is used to plot Figure 3 and Figure 4.

### Label age groups ###
i = 1

for(i in seq_along(pop_contacts_balanced)) {                #Iterate through list of balanced population contacts from Prem and label rows and columns with age group
  colnames(pop_contacts_balanced[[i]]) = c("<15", "15+")
  rownames(pop_contacts_balanced[[i]]) = c("<15", "15+")
}

i = 1

for(i in seq_along(pop_contacts_imbalanced)) {
  colnames(pop_contacts_imbalanced[[i]]) = c("<15", "15+")  #Iterate through list of imbalanced population contacts from Prem and label rows and columns with age group
  rownames(pop_contacts_imbalanced[[i]]) = c("<15", "15+")
}

#### Create single df of balanced and imbalanced C per country ###

## Gambia ##

C_GMB_imbal = 
  data.frame(pop_contacts_imbalanced[["GMB"]]) %>%                         #Transform imbalanced contact matrix from Gambia to dataframe 
  rownames_to_column("age.ind") %>%                                        #Create column according to age of individual
  pivot_longer(cols=2:3,                                                   #Transform data frame to long format with additional column outlining age of contact
               names_to = "age.contact",                                            
               values_to = "C.imbal") %>%
  mutate(setting = "Gambia",                                               #Add additional variable describing demographic setting  
         age.contact = ifelse(age.contact == "X.15", "<15", "15+"))        #Fix names of contact age groups

C_GMB = 
  data.frame(pop_contacts_balanced[["GMB"]]) %>%                           #Transform balanced contact matrix from Gambia to dataframe 
  rownames_to_column("age.ind") %>%                                        #Create column according to age of individual
  pivot_longer(cols=2:3,
               names_to = "age.contact",                                   #Transform data frame to long format with additional column outlining age of contact
               values_to = "C.bal") %>%
  mutate(setting = "Gambia",                                               #Add additional variable describing demographic setting
         age.contact = ifelse(age.contact == "X.15", "<15", "15+")) %>%    #Fix names of contact age groups
  merge(C_GMB_imbal, all = T) %>%                                          #Merge data frame of balanced contacts from Gambia with data frame of imbalanced contacts from Gambia                                                   
  mutate(C_ratio = C.imbal/C.bal)                                          #Calculate ratio of imbalanced to balanced contacts per matrix element (<15 with 15+, 15+ with <15 and so on)


## Luxembourg ##
C_LUX_imbal = 
  data.frame(pop_contacts_imbalanced[["LUX"]]) %>%                          #Transform imbalanced contact matrix from Luxembourg to dataframe 
  rownames_to_column("age.ind") %>%                                         #Create column according to age of individual
  pivot_longer(cols=2:3,                                                            
               names_to = "age.contact",                                    #Transform data frame to long format with additional column outlining age of contact
               values_to = "C.imbal") %>%                                           
  mutate(setting = "Luxembourg",                                            #Add additional variable describing demographic setting
         age.contact = ifelse(age.contact == "X.15", "<15", "15+"))         #Fix names of contact age groups

C_LUX = 
  data.frame(pop_contacts_balanced[["LUX"]]) %>%                            #Transform balanced contact matrix from Luxembourg to dataframe 
  rownames_to_column("age.ind") %>%                                         #Create column according to age of individual
  pivot_longer(cols=2:3, 
               names_to = "age.contact",                                    #Transform data frame to long format with additional column outlining age of contact
               values_to = "C.bal") %>%
  mutate(setting = "Luxembourg",                                            #Add additional variable describing demographic setting
         age.contact = ifelse(age.contact == "X.15", "<15", "15+")) %>%     #Fix names of contact age groups
  merge(C_LUX_imbal, all = T) %>%                                             #Merge data frame of balanced contacts from Gambia with data frame of imbalanced contacts from Gambia      
  mutate(C_ratio = C.imbal/C.bal)                                           #Calculate ratio of imbalanced to balanced contacts per matrix element (<15 with 15+, 15+ with <15 and so on)

 
## Singapore ##
C_SGP_imbal = 
  data.frame(pop_contacts_imbalanced[["SGP"]]) %>%                          #Transform imbalanced contact matrix from Singapore to dataframe 
  rownames_to_column("age.ind") %>%                                         #Create column according to age of individual
  pivot_longer(cols=2:3,
               names_to = "age.contact",                                    #Transform data frame to long format with additional column outlining age of contact
               values_to = "C.imbal") %>%
  mutate(setting = "Singapore",                                             #Add additional variable describing demographic setting
         age.contact = ifelse(age.contact == "X.15", "<15", "15+"))         #Fix names of contact age group


C_SGP = 
  data.frame(pop_contacts_balanced[["SGP"]]) %>%                            #Transform balanced contact matrix from Singapore to dataframe 
  rownames_to_column("age.ind") %>%                                         #Create column according to age of individual
  pivot_longer(cols=2:3,
               names_to = "age.contact",                                    #Transform data frame to long format with additional column outlining age of contact
               values_to = "C.bal") %>%
  mutate(setting = "Singapore",
         age.contact = ifelse(age.contact == "X.15", "<15", "15+")) %>%     #Fix names of contact age group
  merge(C_SGP_imbal, all = T) %>%                                           #Add additional variable describing demographic setting
  mutate(C_ratio = C.imbal/C.bal)                                           #Calculate ratio of imbalanced to balanced contacts per matrix element (<15 with 15+, 15+ with <15 and so on)



### Merge country-specific dataframes ###

C_sub_df =                                               #Merge dataframes of imbalanced and balanced matrix elements from Gambia, Luxembourg and Singapore
  rbind(C_GMB,
        C_LUX,
        C_SGP)

write_xlsx(C_sub_df, "./Data/Contacts_scenarios.xlsx")   #save dataframe of imbalanced and balanced matrix elements from Gambia, Luxembourg and Singapore to plot Figures 3 and 4
save(C_sub_df, file = "./Data/Contacts_scenarios.RData")


