########################################### Code Summary ##############################################
### TITLE:            Results.R
### BY:               Mackenzie A. Hamilton 
### DATE CREATED:     2022/03/04
### DESCRIPTION:      Code to plot results from each aim (i.e. output from Main.R)
### DATASETS USED:    Aim1_Impact_balancing_R0.RData, Aim2_SimulationOutput.RData, 
###                     Aim2_Cum_Infections_difference.RData, Aim3_VaxImpact_strat1.RData, 
###                     Aim3_VaxImpact_strat2.RData
### DATASETS OUTPUT:  Figure1a.png, Figure1b.png, Figure2.png, Figure3.png, Figure4.png, FigureS1a.png, 
###                     FigureS1b.png, FigureS2.png
############################################ Code Setup ###############################################

library(tibble)
library(scales)
library(dplyr)
library(tidyr)
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

setwd("~/GitHub/Balancing_C_Impact2")                       #Set working directory

load("./Data/Contacts_scenarios.RData")                     #Load contact matrix dataframe for 3 countries: Gambia, Luxembourg and Singapore
load("./Output/Aim1_Impact_balancing_R0.RData")             #Load aim 1 output (impact of imbalanced contact matrix on R0)
load("./Output/Aim2_SimulationOutput.RData")                #Load aim 2 model output (impact of imbalanced contact matrix on epidemic trajectory)
load("./Output/Aim2_Cum_Infections_difference.RData")       #Load aim 2 cumulative infections (impact of imbalanced contact matrix on cumulative infections over 1 year)
load("./Output/Aim3_VaxImpact_strat1.RData")                #Load aim 3 vax strategy 1 output (influence of imbalanced contact matrix on impact of an age-specific vaccination strategy)
load("./Output/Aim3_VaxImpact_strat2.RData")                #Load aim 3 vax strategy 2 output (influence of imbalanced contact matrix on impact of an age-specific vaccination strategy)


############################################ Aim 1: Impact Basic Reproduction Number ###################

### Figure 1: Models with imbalanced contact matrices underestimate R0 ### 

## Figure 1b ##
#Note: Plot Figure 1b first because colour scheme from Figure1b is used to colour Figure 1a

#Assign ratios of Coy (population contacts 15+ reported with <15) and R0 (imbalanced/balanced) to variables for ease of use in ggpplot Figure 1 code 
C_oy_ratio_GMB = subset(Impact_balancing_R0, iso3c =="GMB")[[c("C_oy_imbalance")]]                                                                #Assign Gambia C_oy ratio (imbalanced/balanced) to variable                                                      
R0_ratio_GMB = subset(Impact_balancing_R0, iso3c =="GMB")[[c("R0_imbalanced")]]/subset(Impact_balancing_R0, iso3c =="GMB")[[c("R0_balanced")]]    #Assign Gambia R0 ratio (imbalanced/balanced) to variable

C_oy_ratio_LUX = subset(Impact_balancing_R0, iso3c =="LUX")[[c("C_oy_imbalance")]]                                                                #Assign Luxembourg C_oy ratio (imbalanced/balanced) to variable           
R0_ratio_LUX = subset(Impact_balancing_R0, iso3c =="LUX")[[c("R0_imbalanced")]]/subset(Impact_balancing_R0, iso3c =="LUX")[[c("R0_balanced")]]    #Assign Luxembourg R0 ratio (imbalanced/balanced) to variable           

C_oy_ratio_SGP = subset(Impact_balancing_R0, iso3c =="SGP")[[c("C_oy_imbalance")]]                                                                #Assign Singapore C_oy ratio (imbalanced/balanced) to variable
R0_ratio_SGP = subset(Impact_balancing_R0, iso3c =="SGP")[[c("R0_imbalanced")]]/subset(Impact_balancing_R0, iso3c =="SGP")[[c("R0_balanced")]]    #Assign Singapore R0 ratio (imbalanced/balanced) to variable

summary(Impact_balancing_R0$C_oy_imbalance)                                                                                                       #Evaluate descriptive stats of C_oy ratio to determine x axis of Figure 1b  

Figure1b=                                                                                                                                         #Plot Figure 1b (relationship between imbalance in Coy and R0)
  ggplot(Impact_balancing_R0,
         aes(x = C_oy_imbalanced/C_yo_balanced,
             y = R0_imbalanced/R0_balanced)) +
  geom_point(size= 1.4, aes(colour = C_oy_imbalance)) +
  geom_point(aes(x=C_oy_ratio_GMB, y=R0_ratio_GMB), colour="black", size = 2.2) +                             #Highlight Gambia
  annotate("text", x = 0.57, y = 0.9425, label = "Gambia", colour = "black", size = 3.6) +                    #Annotate Gambia
  geom_point(aes(x=C_oy_ratio_LUX, y=R0_ratio_LUX), colour="black", size = 2.2) +                             #Highlight Luxembourg
  annotate("text", x = 1.0, y = 0.995, label = "Luxembourg", colour = "black", size = 3.6) +                  #Annotate Luxembourg
  geom_point(aes(x=C_oy_ratio_SGP, y=R0_ratio_SGP), colour="black", size = 2.2) +                             #Highlight Singapore
  annotate("text", x = 1.3, y = 0.97, label = "Singapore", colour = "black", size = 3.6) +                    #Annotate Singapore
  theme_bw() +
  xlab(bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                             #Label x axis
  ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +                                                               #Label y axis
  labs(color = bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                     #Label legend
  scale_color_continuous_diverging("Blue-Red 3", trans = "log", limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))  #Fill colour of points based on extent of imbalance in Coy
  
Figure1b
 
ggsave(file= paste0("./Figures/Figure1b.png"), width = 6.5, height = 4, limitsize = FALSE, Figure1b)          #Save Figure1b as .png file


## Figure1a ##

Figure1_colours = ggplot_build(Figure1b)$data[[1]] %>%                                                        #save Figure1b data point colours in variable for Figure1a 
  arrange(x)

Figure1a_spatial_df = joinCountryData2Map(Impact_balancing_R0,                                                #Create a spatial polygon dataframe of countries from Prem                                      
                                          joinCode = "ISO3",
                                          nameJoinColumn = "iso3c")

mapCountryData(Figure1a_spatial_df,                                                                           #Plot distribution of Coy imbalance globally using colourscheme from Figure1b 
               nameColumnToPlot = 'C_oy_imbalance',                                                           #NOTE: manually saved plot and merged with Figure 1b outside of R workspace
               catMethod = "fixedWidth",
               numCats = 177,
               colourPalette = Figure1_colours$colour,                                                        #Colour countries according to colour palette from Figure1b
               addLegend = F,
               mapTitle = "",
               missingCountryCol = "Grey")                                                                    #If country is missing data, fill with grey


### Supplemental Figure 2: Influence of imbalanced contacts reported by <15 with 15+ on R0 ###

## FigureS2b ##

#Assign ratios of Cyo (Population contacts reported by <15 with 15+; imbalanced/balanced) to variables for ease of use in ggpplot Figure S1 code 
C_yo_ratio_GMB = subset(Impact_balancing_R0, iso3c =="GMB")[[c("C_yo_imbalance")]]                            #Assign Gambia C_yo ratio (imbalanced/balanced) to variable 
C_yo_ratio_LUX = subset(Impact_balancing_R0, iso3c =="LUX")[[c("C_yo_imbalance")]]                            #Assign Luxembourg C_yo ratio (imbalanced/balanced) to variable 
C_yo_ratio_SGP = subset(Impact_balancing_R0, iso3c =="SGP")[[c("C_yo_imbalance")]]                            #Assign Singapore C_yo ratio (imbalanced/balanced) to variable


#Plot figureS2b first because colour scheme are used to colour FigureS1a
FigureS2b = 
  ggplot(Impact_balancing_R0,
         aes(x = C_yo_imbalanced/C_yo_balanced,
             y = R0_imbalanced/R0_balanced)) +
  geom_point(size= 1.4, aes(colour = C_yo_imbalance)) +
  geom_point(aes(x=C_yo_ratio_GMB, y=R0_ratio_GMB), colour="black", size = 2.2) +                             #Highlight Gambia data point
  annotate("text", x = 1.4, y = 0.9425, label = "Gambia", colour = "black", size = 3.6) +                     #Annotate Gambia data point
  geom_point(aes(x=C_yo_ratio_LUX, y=R0_ratio_LUX), colour="black", size = 2.2) +                             #Highlight Luxembourg data point
  annotate("text", x = 1.0, y = 0.995, label = "Luxembourg", colour = "black", size = 3.6) +                  #Annotate Luxembourg data point
  geom_point(aes(x=C_yo_ratio_SGP, y=R0_ratio_SGP), colour="black", size = 2.2) +                             #Highlight Singapore data point
  annotate("text", x = 0.725, y = 0.97, label = "Singapore", colour = "black", size = 3.6) +                  #Annotate Singapore data point
  theme_bw() +
  xlab(bquote(C['yo, imbal']~'/'~C['yo, bal'])) +                                                             #Label x axis
  ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +                                                               #Label y axis
  labs(color = bquote(C['yo, imbal']~'/'~C['yo, bal'])) +                                                     #Label legend
  scale_x_continuous(limits = c(0.4, 1.6),                                                                    #fix x-axis scale limits and break points
                     breaks = seq(0.4, 1.6, 0.4)) +                        
  scale_y_continuous(limits = c(0.94, 1.00),                                                                  #fix y-axis scale limits and break points
                     breaks = seq(0.94, 1.00, 0.02)) +
  scale_color_continuous_diverging("Blue-Red 3", trans = "log", limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))  #colour data points according to imbalance in contacts <15 reported with 15+ (Cyo)

FigureS2b

ggsave(file= paste0("./Figures/FigureS2b.png"), 
       width = 6.5, height = 4, limitsize = FALSE, FigureS2b)                                                 #Save FigureS1b as .png file

## FigureS2a ##

FigureS2_colours = ggplot_build(FigureS2b)$data[[1]] %>%                                                      #save FigureS1b data point colours in variable for FigureS1a 
  arrange(x)

mapCountryData(Figure1a_spatial_df,                                                                           #Plot distribution of Cyo imbalance globally using colourscheme from FigureS1b
               nameColumnToPlot = 'C_yo_imbalance',                                                           #NOTE: uses spatial data frame created for Figure 1 must manually save this figure and merge with 1a outside of R
               catMethod = "fixedWidth",                                                                      #NOTE:manually saved plot and merged with Figure1b outside of R workspace
               numCats = 177,                                                                                
               colourPalette = FigureS2_colours$colour,                                                       #Colour countries according to colour palette from FigureS1b
               addLegend = F,                                    
               mapTitle = "",                                                                                 
               missingCountryCol = "Grey")                                                                    #If countries not included in Prem analysis, colour in grey


############################################ Aim 2: Impact Transmission Dynamics ###############################

### Clean labels from model output ###

ModelOutput_cleaned$Age.Group = factor(ModelOutput_cleaned$Age.Group,                     #Re-order age group labels for Figure2 plot                            
                                       levels = c("Overall", "<15", "15+"))
ModelOutput_cleaned$context   = factor(ModelOutput_cleaned$context,                       #Re-label and reorder demographic setting labels
                                       levels = c("GMB", "LUX", "SGP"),      
                                       labels = c("Gambia", "Luxembourg", "Singapore")) 
ModelOutput_cleaned$balance   = factor(ModelOutput_cleaned$balance,                       #Re-label (capitalize) and reorder balance labels 
                                       levels = c("balanced", "imbalanced"),  
                                       labels = c("Balanced", "Imbalanced"))

### Figure 2: Bias in SARS-CoV-2 epidemic trajectory overall and among age groups according to imbalance in synthetic contact matrix ###

Figure2 =                                                                                                     
  ggplot(subset(ModelOutput_cleaned, vax == 0),                                   #Plot epidemic curve up to 365 days post seeding with no vaccination
         aes(x = time,                                                            #Plot time as x variable
             y = I/N*100000,                                                      #plot incidence per 100,000 as y variable
             group = factor(balance),                                             #Factor plots according to balance of model
             color= factor(balance))) +                                           #Colour line according to balance of model
  geom_line(size= 0.9) +
  xlab(label= "Time (days)") +                                                    #Label x-axis
  ylab(label= "Incidence (per 100,000)") +                                        #Label y-axis
  labs(color = "Contact Matrix") +                                                #Label legend
  theme_bw() +
  scale_x_continuous(limits = c(0, 365),                                          #Fix x-axis scale limits and break points
                     breaks = seq(0, 365, 100)) +
  scale_color_manual(values = c("#696969", "#B54B4B")) +                          #Fix colour scheme of lines according to balance of model
  facet_grid(rows = vars(Age.Group),                                              #Facet wrap by age group (Overall, <15 and 15+) and demographic setting (Gambia, Luxembourg and Singapore)
             cols = vars(context))

Figure2

ggsave(file= paste0("./Figures/Figure2.png"),                                     #save Figure 2 as a .png file
       width = 8, height = 6, limitsize = FALSE, Figure2)



### Figure 3: Imbalanced contact matrices bias estimates of cumulative SARS-CoV-2 infections overall and among subgroups ###

## Figure3a ##

Figure3a =                                                                        #Plot heat map of contact matrices per demographic setting to visually demonstrate imbalances 
  ggplot(C_sub_df,
         aes(x = age.ind,                                                         #Age of individual on x-axis 
             y = age.contact)) +                                                  #Age of contact on y-axis 
  geom_raster(aes(fill = C_ratio)) +                                              #Fill heat map according to population contacts age group i reported with age group j vs. balanced 
  theme_bw() +                                                                    #   population contacts between i and j
  ylab(label= "Age (contact)") +                                                  #Label y axis
  xlab(label= "Age (individual)") +                                               #Label x axis 
  labs(fill = bquote(C['imbal']~'/'~C['bal'])) +                                  #Label legend
  facet_wrap(~setting, nrow = 1) +                                                #Facet by demographic setting
  scale_fill_continuous_diverging("Blue-Red 3", trans = "log", 
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))    #Fix fill colour, limits and breaks of heatmap
Figure3a


#Figure 3b: Percent change in final epidemic size from imbalanced to balanced conditions 

## Figure 3b ##

#Calculate ratio of imbalanced population contacts to balanced population contacts per demographic setting overall (for plotting Figure 3b)

#Create subset of contact matrix dataframe that only includes contacts that were imbalanced 
C_sub_df_imbalance =                                                                                           
  subset(C_sub_df, !(age.ind == "<15" & age.contact == "<15") &                   #subset contact matrix data frame
           !(age.ind == "15+" & age.contact == "15+")) %>% 
  select(age.ind, setting, C_ratio) %>%                                           #Select variables of interest
  rename(Age.Group = age.ind,                                                     #Rename variables to align with data for Figure 3b 
         context = setting)

CumI_difference$context = factor(CumI_difference$context,                         #Spell out demographic contexts for Figure 3b 
                                 levels = c("GMB", "LUX", "SGP"), 
                                 labels = c("Gambia", "Luxembourg", "Singapore"))

#Sum population contacts per demographic setting when imbalanced and balanced and merge with Figure 3b data (percent difference in cumulative infections per demographic setting)
CumI_difference_plots =                                                                                        
  C_sub_df %>%
  arrange(setting) %>%                      #Group all heterogenous contacts from contact dataframe by demographic setting
  group_by(setting) %>%
  summarise(Age.Group = "Overall",          #Name age group as "Overall"
            C.imbal = sum(C.imbal),         #Sum population level contacts from imbalanced contact matrix
            C.bal = sum(C.bal)) %>%         #Sum population level contacts from balanced contact matrix 
  mutate(C_ratio = C.imbal/C.bal) %>%       #Calculate ratio of population contacts from imbalanced versus balanced contact matrix
  rename(context = setting) %>%             #Rename "setting" variable to context to match Figure3 data
  select(context, Age.Group, C_ratio) %>%
  rbind(C_sub_df_imbalance) %>%             #Merge with population contact ratios (imbalanced to balanced) per age-group, per demographic setting 
  merge(CumI_difference)                                                                                       

 
CumI_difference_plots$Age.Group = factor(CumI_difference_plots$Age.Group,            #Reorder age groups for plots
                                         levels = c("Overall", "<15", "15+"))

summary(CumI_difference_plots$CumI_percent_change)                                   #Obtain range of percent change in cumulative infections for plots
  
Figure3b =                                                                           #Plot Figure 3b
  ggplot(CumI_difference_plots,
         aes(x = Age.Group,                                                          #Plot age groups on x axis
             y = CumI_percent_change)) +                                             #Plot percent change in cumulative infections per age group on y axis
  geom_col(width = 0.3, position = "dodge", aes(fill = C_ratio), color = "black") +  #Plot a box plot, colored by the ratio of imbalance in contacts per age group and demographic setting 
  geom_hline(yintercept = 0, linetype = "dashed") +                                  #Draw horizontal line at 0 where there is no difference in cumulative infections
  scale_y_continuous(limits = c(-12, 4),                                             #Set y axis limits
                     breaks = seq(-12, 4, 2)) + 
  xlab(label= "Age Group") +                                                         #Label x axis
  ylab(label= "% Difference in Cumulative Infections \n (imbal vs. bal)") +           #Label y axis                                                                    
  labs(fill = bquote(C['imbal']~'/'~C['bal'])) +                                     #Label legend
  theme_bw() +
  facet_wrap(~context, nrow = 1) +                                                   #stratify by demographic context
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +          
  scale_fill_continuous_diverging(palette = "Blue-Red 3", trans = "log",             #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))
Figure3b

    
Figure3 = ggarrange(Figure3a,                                                        #Merge Figure 3a and 3b
                    Figure3b,
                    align = "hv",
                    nrow = 2,
                    ncol = 1,
                    common.legend = T,
                    legend = "right",
                    heights = c(1,2),
                    labels = "AUTO")

Figure3
 
ggsave(file= paste0("./Figures/Figure3.png"),                                        #Save Figure 3 as a .png file
       width = 7, height = 7, limitsize = FALSE, Figure3)


############################################ Aim 3: Impact of Age-specific Vaccination Strategies ###############################

### Vaccination strategy 1 - vaccinate 50% of individuals <15 ###

Impact_vax1$context = factor(Impact_vax1$context,                                   #Spell out demographic contexts for Figure 4a 
                             levels = c("GMB", "LUX", "SGP"), 
                             labels = c("Gambia", "Luxembourg", "Singapore"))

Impact_vax1_wC =                                                                    #Merge data on percent difference in infections averted from age-specific vaccination 
  CumI_difference_plots %>%                                                         #  strategy 1 with imbalances in population contacts per age group and demographic setting
  select(context, Age.Group, C_ratio) %>%
  merge(Impact_vax1)

Impact_vax1_wC$Age.Group = factor(Impact_vax1_wC$Age.Group,                         #Reorder age groups for plots 
                                  levels = c("Overall", "<15", "15+"))

summary(Impact_vax1_wC$Diff_inf_averted)                                            #Obtain range of percent change in infections averted for Figure 4a

Figure4a =                                                                          #Plot Figure 4a
  ggplot(Impact_vax1_wC,
         aes(x = Age.Group,                                                         #Plot age group on x axis
             y = Diff_inf_averted)) +                                               #Plot percent difference in infections averted from age-specifc vaccination strategy 1 on y axis 
  geom_col(width = 0.3, position = "dodge", aes(fill = C_ratio), color = "black") + #Plot a box plot, colored by the ratio of imbalance in contacts per age group and demographic setting 
  geom_hline(yintercept = 0, linetype = "dashed") +                                 #Draw horizontal line at 0 where there is no difference in infections averted from imbalanced to balanced models
  scale_y_continuous(limits = c(-25, 40),                                           #Set y axis limits
                     breaks = seq(-24, 40, 8)) + 
  xlab(label= "Age Group") +                                                        #Label x axis
  ylab(label= "% Difference in Infections Averted \n (imbal vs. bal)") +            #Label y axis                                           
  labs(fill = bquote(C['imbal']~'/'~C['bal'])) +                                    #Label legend
  theme_bw() + 
  facet_wrap(~context, nrow = 1) +                                                  #Stratify by demographic context
  scale_fill_continuous_diverging(palette = "Blue-Red 3", trans = "log",            #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))

Figure4a

### Vaccination strategy 2 - vaccinate 50% of individuals 15+ ###

Impact_vax2$context = factor(Impact_vax2$context,                                   #Spell out demographic context for Figure 4b
                             levels = c("GMB", "LUX", "SGP"), 
                             labels = c("Gambia", "Luxembourg", "Singapore"))

Impact_vax2_wC =                                                                    #Merge data on percent difference in infections averted from age-specific vaccination 
  CumI_difference_plots %>%                                                         #  strategy 2 with imbalances in population contacts per age group and demographic setting                                                     
  select(context, Age.Group, C_ratio) %>%
  merge(Impact_vax2)

Impact_vax2_wC$Age.Group = factor(Impact_vax1_wC$Age.Group,                         #Reorder age groups for plots
                                  levels = c("Overall", "<15", "15+"))


summary(Impact_vax2_wC$Diff_inf_averted)                                            #Obtain range of percent change in infections averted for Figure 4b
  
Figure4b =                                                                          #Plot Figure 4b 
  ggplot(Impact_vax2_wC,
         aes(x = Age.Group,                                                         #Plot age group on x axis
             y = Diff_inf_averted)) +                                               #Plot percent difference in infections averted from age-specific vaccination strategy 2 on y axis 
  geom_col(width = 0.3, position = "dodge", aes(fill = C_ratio), color = "black") + #Plot a box plot, colored by the ratio of imbalance in contacts per age group and demographic setting 
  geom_hline(yintercept = 0, linetype = "dashed") +                                 #Draw horizontal line at 0 where there is no difference in infections averted from imbalanced to balanced models
  scale_y_continuous(limits = c(-25, 40),                                           #Set y axis limits
                     breaks = seq(-24, 40, 8)) + 
  xlab(label= "Age Group") +                                                        #Label x axis
  ylab(label= "% Difference in Infections Averted \n (imbal vs. bal)") +            #Label y axis
  labs(fill = bquote(C['imbal']~'/'~C['bal'])) +                                    #Label legend
  theme_bw() +                                       
  facet_wrap(~context, nrow = 1) +                                                  #Stratify by demographic context
  scale_fill_continuous_diverging(palette = "Blue-Red 3", trans = "log",            #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))

Figure4b


Figure4 = ggarrange(Figure4a,                                                       #Merge Figure 4a and 4b
                    Figure4b + rremove("y.title"),
                    align = "hv",
                    nrow = 1,
                    ncol = 2,
                    common.legend = T,
                    legend = "right")

Figure4

ggsave(file= paste0("./Figures/Figure4.png"),                                       #Save Figure 4 as .png file
       width = 10, height = 5, limitsize = FALSE, Figure4)

