#################################################################### Code Summary ##################################################
### TITLE:            Validation - BiasDirection
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022-04-07
### DESCRIPTION:      Code to assess the impact of imbalanced contact rates (imbalanced opposite to trends observed in Prem et al.) 
###                     on basic reproduction number
### DATASETS USED:    Contacts.RData, Pop.RData, Aim1_Impact_balancing_R0.RData, ISO3C_byRegion.xlsx, FigureS3a.RData
### DATASETS OUTPUT:  Validity_BiasDirection_R0.RData, Validity_BiasDirection_R0.xlsx, FigureS3b.RData, FigureS3.png
#################################################################### Code Setup ####################################################

library(dplyr)
library(tidyr)
library(tibble)
library(writexl)
library(ggplot2)
library(ggpubr)
library(colorspace)

setwd("~/GitHub/imbalanced-contact-matrices")                 #Set working directory

source("./Code/Functions.R")                                  #Load general functions
source("./Code/Model.R")                                      #Load SEIR model function and model parameters

iso3c_continent = read_xlsx("./Data/ISO3C_byRegion.xlsx")     #Load ISO3C codes per country across the world
load("./Data/Pop.RData")                                      #Load Prem 2021 population data from 177 demographic settings (raw data cleaned in Contacts.R)        
load("./Data/Contacts.RData")                                 #Load imbalanced and balanced contact matrices for populations stratified into two age groups: <15 and 15+ across 177 
                                                              # demographic  settings from Prem 2021 (matrices cleaned from raw data in Contacts.R)

load("./Output/Aim1_Impact_balancing_R0.RData")               #Load data output for Aim 1 evaluating relationship between imbalance in raw Prem contact matrices and underestimation of R0 
load("./Figures/FigureS3a.RData")                             #Load data for FigureS3a (other validation analysis) to be merged with FigureS3b at end of code


#################################################################### Change directionality of bias in C #################################

pop_contacts_imbalanced_2 = lapply(pop_contacts_imbalanced, function(x){change_bias_direction_c(x)}) #Apply change_bias_direction_c function (written in Functions.R) to invert bias in population 
                                                                                                     #  contacts between <15 and 15+ in imbalanced matrices from 177 demographic settings in Prem
contacts_imbalanced_2 = extensive_to_intensive(pop_contacts_imbalanced_2, N_list)                    #Transform population contacts to contacts per person per day using extensive_to_intensive
                                                                                                     #  function (written in Functions.R)
pop_contacts_imbalanced[[1]]
pop_contacts_imbalanced_2[[1]]                                                                       #Check to ensure population contacts were inverted correctly
pop_contacts_balanced[[1]]                                                                                     

###############################  Validation 2: Impact Basic Reproduction Number (inverse directionality in bias of contacts) #############


### Imbalanced Reproduction numbers (with new direction of bias) ###

R0_matrix_imbalanced_2 = R0_matrix(contacts_imbalanced_2, N_list, beta, gamma)       #Calculate baby R0s (i.e. R0 per age group and mixing pattern) per demographic setting using imbalanced matrices (with inverse bias direction) and population data from Prem
R0_yo_imbalanced_2     = lapply(R0_matrix_imbalanced_2, function(x){x[1,2]})         #Save baby R0 among <15 from contact with 15+ under imbalanced (inverse bias) conditions
R0_oy_imbalanced_2     = lapply(R0_matrix_imbalanced_2, function(x){x[2,1]})         #Save baby R0 among 15+ from contact with <15 under imbalanced (inverse bias) conditions
R0_imbalanced_2        = R0_matrix_determinant(R0_matrix_imbalanced_2)               #Calculate overall R0 (i.e. dominant eigenvalue of baby R0 matrix) per demographic setting studied in Prem

### Balanced Reproduction numbers ###

R0_matrix_balanced = R0_matrix(contacts_balanced, N_list, beta, gamma)               #Calculate baby R0s (i.e. R0 per age group and mixing pattern) per demographic setting using balanced matrices and population data from Prem
R0_yo_balanced     = lapply(R0_matrix_balanced, function(x){x[1,2]})                 #Save baby R0 among <15 from contact with 15+ under balanced conditions
R0_oy_balanced     = lapply(R0_matrix_balanced, function(x){x[2,1]})                 #Save baby R0 among 15+ from contact with <15 under balanced conditions
R0_balanced        = R0_matrix_determinant(R0_matrix_balanced)                       #Calculate overall R0 (i.e. dominant eigenvalue of baby R0 matrix) per demographic setting studied in Prem

### Direction and magnitude of imbalanced contacts (y-o) ###

C_yo_balanced     = lapply(pop_contacts_balanced, function(x){x[1,2]})               #Create list of balanced population contacts between <15 and 15+ per demographic setting in Prem
C_yo_imbalanced_2 = lapply(pop_contacts_imbalanced_2, function(x){x[1,2]})           #Create list of population contacts <15 reported with 15+ per demographic setting in Prem (imbalanced)  
C_oy_imbalanced_2 = lapply(pop_contacts_imbalanced_2, function(x){x[2,1]})           #Create list of population contacts 15+ reported with <15 per demographic setting in Prem (imbalanced)


#Merge data into single dataframe

C_yo_balanced_df = 
  as.data.frame(C_yo_balanced) %>%                                                   #Transform list of balanced contacts between <15 and 15+ into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_balanced")        #Transform dataframe from wide to long format according to country ISO3C code

C_yo_imbalanced_df = 
  as.data.frame(C_yo_imbalanced_2) %>%                                               #Transform list of imbalanced contacts <15 reported with 15+ into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_yo_imbalanced_2")    #Transform dataframe from wide to long format according to country ISO3C code

C_oy_imbalanced_df = 
  as.data.frame(C_oy_imbalanced_2) %>%                                               #Transform list of imbalanced contacts 15+ reported with <15 into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "C_oy_imbalanced_2")    #Transform dataframe from wide to long format according to country ISO3C code

R0_balanced_df =  
  as.data.frame(R0_balanced) %>%                                                     #Transform list of R0s calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_balanced")          #Transform dataframe from wide to long format according to country ISO3C code

R0_yo_balanced_df = 
  as.data.frame(R0_yo_balanced) %>%                                                  #Transform list of baby R0s among <15 from contact with 15+ calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_balanced")       #Transform dataframe from wide to long format according to country ISO3C code

R0_oy_balanced_df = 
  as.data.frame(R0_oy_balanced) %>%                                                  #Transform list of baby R0s among 15+ from contact with <15 calculated with balanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_balanced")       #Transform dataframe from wide to long format according to country ISO3C code

R0_imbalanced_df = 
  as.data.frame(R0_imbalanced_2) %>%                                                 #Transform list of R0s calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_imbalanced_2")      #Transform dataframe from wide to long format according to country ISO3C code

R0_yo_imbalanced_df = 
  as.data.frame(R0_yo_imbalanced_2) %>%                                              #Transform list of baby R0s among <15 from contact with 15+ calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_yo_imbalanced_2")   #Transform dataframe from wide to long format according to country ISO3C code

R0_oy_imbalanced_df = 
  as.data.frame(R0_oy_imbalanced_2) %>%                                              #Transform list of baby R0s among 15+ from contact with <15 calculated with imbalanced contact matrices into a dataframe
  pivot_longer(cols = 1:177, names_to = "iso3c", values_to = "R0_oy_imbalanced_2")   #Transform dataframe from wide to long format according to country ISO3C code

Impact_balancing_R0_valid2 =                                                         #Merge all dataframes together
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
  mutate(C_oy_imbalance_ratio_2  = C_oy_imbalanced_2/C_yo_balanced,                  #Calculate ratio of imbalanced contacts 15+ reported with <15 to balanced contacts between 15+ and <15
         C_yo_imbalance_ratio_2  = C_yo_imbalanced_2/C_yo_balanced,                  #Calculate ratio of imbalanced contacts <15 reported with 15+ to balanced contacts between 15+ and <15
         R0_oy_imbalance_ratio_2 = R0_oy_imbalanced_2/R0_oy_balanced,                #Calculate ratio of imbalanced to balanced baby R0 among 15+ from contact with <15
         R0_yo_imbalance_ratio_2 = R0_yo_imbalanced_2/R0_yo_balanced,                #Calculate ratio of imbalanced to balanced baby R0 among <15 from contact with 15+ 
         R0_change_2 = (R0_imbalanced_2 - R0_balanced)/R0_balanced*100) %>%          #Calculate percent change in R0 when calculated with imabalanced versus balanced contact matrices
  select(iso3c, region, `sub-region`, `intermediate-region`, C_yo_balanced,          #Reorder columns of dataframe
         C_yo_imbalanced_2, C_oy_imbalanced_2,  C_yo_imbalance_ratio_2,  
         C_oy_imbalance_ratio_2, R0_balanced, R0_yo_balanced, R0_oy_balanced,
         R0_imbalanced_2, R0_yo_imbalanced_2, R0_oy_imbalanced_2, 
         R0_yo_imbalance_ratio_2, R0_oy_imbalance_ratio_2, R0_change_2)

save(Impact_balancing_R0_valid2, file = "./Output/Validity_BiasDirection_R0.RData")     #Save data in RData format
write_xlsx(Impact_balancing_R0_valid2, "./Output/Validity_BiasDirection_R0.xlsx") #Save data in xlsx format



############################################ Plot bias in basic reproduction number ################################################

### Supplementary Figure 3b: Underestimation of R0 is robust to changes in direction of imbalance in synthetic contact matrices  ###

#Assign ratios of Coy (population contacts 15+ reported with <15) and R0 (imbalanced/balanced) to variables for easy annotations in ggplot FigureS3b code
C_oy_ratio_GMB_plotb = subset(Impact_balancing_R0_valid2, iso3c =="GMB")[[c("C_oy_imbalance_ratio_2")]]  #Assign Gambia C_oy_ratio (imbalanced/balanced) to variable
R0_ratio_GMB_plotb   = subset(Impact_balancing_R0_valid2, iso3c =="GMB")[[c("R0_imbalanced_2")]]/        #Assign Gambia R0 ratio (imbalanced/balanced) to variable 
  subset(Impact_balancing_R0_valid2, iso3c =="GMB")[[c("R0_balanced")]]
C_oy_ratio_LUX_plotb = subset(Impact_balancing_R0_valid2, iso3c =="LUX")[[c("C_oy_imbalance_ratio_2")]]  #Assign Luxembourg C_oy_ratio (imbalanced/balanced) to variable
R0_ratio_LUX_plotb   = subset(Impact_balancing_R0_valid2, iso3c =="LUX")[[c("R0_imbalanced_2")]]/        #Assign Luxembourg R0 ratio (imbalanced/balanced) to variable
  subset(Impact_balancing_R0_valid2, iso3c =="LUX")[[c("R0_balanced")]]
C_oy_ratio_SGP_plotb = subset(Impact_balancing_R0_valid2, iso3c =="SGP")[[c("C_oy_imbalance_ratio_2")]]  #Assign Singapore C_oy_ratio (imbalanced/balanced) to variable
R0_ratio_SGP_plotb   = subset(Impact_balancing_R0_valid2, iso3c =="SGP")[[c("R0_imbalanced_2")]]/        #Assign Singapore R0 ratio (imbalanced/balanced) to variable
  subset(Impact_balancing_R0_valid2, iso3c =="SGP")[[c("R0_balanced")]]

summary(Impact_balancing_R0_valid2$C_oy_imbalance_2)                                                     #Evaluate descriptive stats of C_oy_ratio to determine x axis range

FigureS3b =                                                                                              #Plot FigureS3b (relationship between imbalance in Coy and R0 with inverse bias 
  ggplot(Impact_balancing_R0_valid2,                                                                     # in imbalanced synthetic contact matrices)
         aes(x = C_oy_imbalance_2,                                                                       #Plot imbalanced in contacts 15+ reported with <15 on x axis
             y = R0_imbalanced_2/R0_balanced)) +                                                         #Plot imbalanced to balanced R0 ratio on y axis
  geom_point(size= 1.4, aes(colour = C_oy_imbalance_2)) +                                                #Colour data points according to imbalance in C_oy
  geom_point(aes(x=C_oy_ratio_GMB_plotb, y=R0_ratio_GMB_plotb), colour="black", size = 2.2) +            #Highlight Gambia data point
  annotate("text", x = 1.42, y = 0.9425, label = "Gambia", colour = "black", size = 3.6) +               #Annotate Gambia data point
  geom_point(aes(x=C_oy_ratio_LUX_plotb, y=R0_ratio_LUX_plotb), colour="black", size = 2.2) +            #Highlight Luxembourg data point
  annotate("text", x = 1.0, y = 0.993, label = "Luxembourg", colour = "black", size = 3.6) +             #Annotate Luxembourg data point
  geom_point(aes(x=C_oy_ratio_SGP_plotb, y=R0_ratio_SGP_plotb), colour="black", size = 2.2) +            #Highlight Singapore data point
  annotate("text", x = 0.7, y = 0.97, label = "Singapore", colour = "black", size = 3.6) +               #Annotate Singapore data point
  theme_bw() +
  theme(legend.position = "right") +                                                                     #Position colour legend on right side of plot
  xlab(bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                        #Label x axis
  ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +                                                          #Label y axis
  labs(color = bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                #Label legend
  scale_x_continuous(limits = c(0.4, 1.6),                                                               #Fix x axis range and break points
                     breaks = seq(0.4, 1.6, 0.4)) +
  scale_y_continuous(limits = c(0.94, 1.00),                                                             #Fix y axis range and break points
                     breaks = seq(0.94, 1.00, 0.02)) + 
  scale_color_continuous_diverging("Blue-Red 3", trans = "log",                                          #Manually colour data points according to extent of imbalance in C_oy (according to fixed colour scale)
                                   limits = c(0.35, 2.9), breaks = c(0.5, 1, 2))

FigureS3b

save(FigureS3b, file = "./Figures/FigureS3b.RData")                                                      #Save FigureS3b as RData file


############################################ Merge FigureS3a and FigureS3b ################################################

FigureS3 = ggarrange(FigureS3a,                                  #Merge FigureS3a and S3b
                     FigureS3b + rremove("ylab"),
                     align = "hv",
                     nrow = 1, 
                     ncol = 2,
                     labels = "AUTO",
                     common.legend = TRUE,
                     legend = "right")
FigureS3

ggsave(file= paste0("./Figures/FigureS3.png"),                 
       width = 10, height = 3.7, limitsize = FALSE, FigureS3)    #Save figure as .png file with fixed dimensions
