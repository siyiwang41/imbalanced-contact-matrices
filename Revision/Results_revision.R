########################################### Code Summary ##############################################
### TITLE:            Results.R
### BY:               Mackenzie A. Hamilton 
### DATE CREATED:     2022/03/04
### DESCRIPTION:      Code to plot results from each aim (i.e. output from Main.R)
### DATASETS USED:    Aim1_Impact_balancing_R0.RData, Aim2_SimulationOutput.RData, 
###                     Aim2_Cum_Infections_difference.RData, Aim3_VaxImpact_strat1.RData, 
###                     Aim3_VaxImpact_strat2.RData
### DATASETS OUTPUT:  Figure1a.png, Figure1b.png, Figure2.png, Figure3.png, Figure4.png, FigureS2a.png, 
###                     FigureS2b.png, FigureS3.png
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
library(tidyverse)

setwd("~/GitHub/imbalanced-contact-matrices/imbalanced-contact-matrices")               #Set working directory
source("./Code/Functions.R")  
source("./Code/Model.R") 
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

# dot=others, diamond=Gambia, rectangle=Luxembourg, triangle=Singapore
shape = c( "Others" =  16,"Gambia" = 23, "Luxembourg" = 22, "Singapore" = 24)



# New version
Figure1b=                                                                                                                                         #Plot Figure 1b (relationship between imbalance in Coy and R0)
  ggplot(Impact_balancing_R0,
         aes(x = C_oy_imbalanced/C_yo_balanced,
             y = R0_imbalanced/R0_balanced)) +
  geom_point(aes(x = C_oy_imbalanced/C_yo_balanced,
                 y = R0_imbalanced/R0_balanced,
                 colour = C_oy_imbalance,
                 shape = "Others"), size= 8/.pt) +
  geom_point(aes(x=C_oy_ratio_GMB, y=R0_ratio_GMB, shape = "Gambia"), colour="black", fill = 'black',  size = 9/.pt) +                             #Highlight Gambia
  # annotate("text", x = 0.57, y = 0.9425, label = "Gambia", colour = "black", size = 3.6) +                    #Annotate Gambia
  geom_point(aes(x=C_oy_ratio_LUX, y=R0_ratio_LUX, shape = "Luxembourg"), colour="black", fill = 'black', size = 9/.pt) +                             #Highlight Luxembourg
  # annotate("text", x = 1.0, y = 0.995, label = "Luxembourg", colour = "black", size = 3.6) +                  #Annotate Luxembourg
  geom_point(aes(x=C_oy_ratio_SGP, y=R0_ratio_SGP, shape = "Singapore"), colour="black", fill = 'black', size = 9/.pt) +                             #Highlight Singapore
  # annotate("text", x = 1.3, y = 0.97, label = "Singapore", colour = "black", size = 3.6) +                    #Annotate Singapore
  theme_classic() +
  xlab(bquote(italic(C)['oy, imbal']~'/'~italic(C)['oy, bal'])) +                                                             #Label x axis
  ylab(bquote(italic(R)['0, imbal']~'/'~italic(R)['0, bal'])) +                                                               #Label y axis
  labs(color = bquote(underline(italic(C)['oy, imbal']~'/'~italic(C)['oy, bal']))) +                                                     #Label legend
  scale_color_continuous_diverging("Blue-Red", trans = "log", limits = c(0.4, 2.5), breaks = c(0.5, 1, 2)) +  #Fill colour of points based on extent of imbalance in Coy
  scale_shape_manual(name = bquote(underline(Country)),
                     breaks = c( "Others", "Gambia" , "Luxembourg", "Singapore"),
                     # label = c( "Others", "Gambia" , "Luxembourg", "Singapore"),
                     values = shape) +
  theme(text = element_text(family = 'sans'),
        axis.line = element_line(colour = 'black', linewidth = 1/.pt),
        legend.position = c(0.5, 0.36),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
        legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        axis.text = element_text(colour = 'black', size = 11),
        axis.title = element_text(size = 11))


# Align_legend function from online resource
Figure1b = ggdraw(align_legend(Figure1b, hjust = 0.5)) 
Figure1b
 

ggsave(file= paste0("./Figures/Figures_revision/Figure1b.pdf"), width = 6, height = 5, limitsize = FALSE, Figure1b)          #Save Figure1b as .png file


## Figure1a ##


# Figure1b=                                                                                                                                         #Plot Figure 1b (relationship between imbalance in Coy and R0)
#   ggplot(Impact_balancing_R0,
#          aes(x = C_oy_imbalanced/C_yo_balanced,
#              y = R0_imbalanced/R0_balanced)) +
#   geom_point(size= 1.4, aes(colour = C_oy_imbalance)) +
#   geom_point(aes(x=C_oy_ratio_GMB, y=R0_ratio_GMB), colour="black", size = 2.2) +                             #Highlight Gambia
#   annotate("text", x = 0.57, y = 0.9425, label = "Gambia", colour = "black", size = 3.6) +                    #Annotate Gambia
#   geom_point(aes(x=C_oy_ratio_LUX, y=R0_ratio_LUX), colour="black", size = 2.2) +                             #Highlight Luxembourg
#   annotate("text", x = 1.0, y = 0.995, label = "Luxembourg", colour = "black", size = 3.6) +                  #Annotate Luxembourg
#   geom_point(aes(x=C_oy_ratio_SGP, y=R0_ratio_SGP), colour="black", size = 2.2) +                             #Highlight Singapore
#   annotate("text", x = 1.3, y = 0.97, label = "Singapore", colour = "black", size = 3.6) +                    #Annotate Singapore
#   theme_bw() +
#   xlab(bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                             #Label x axis
#   ylab(bquote(R['0, imbal']~'/'~R['0, bal'])) +                                                               #Label y axis
#   labs(color = bquote(C['oy, imbal']~'/'~C['oy, bal'])) +                                                     #Label legend
#   scale_color_continuous_diverging("Blue-Red 3", trans = "log", limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))  #Fill colour of points based on extent of imbalance in Coy
# 


# Figure1a

Figure1_colours = ggplot_build(Figure1b)$data[[1]] %>%                                                        #save Figure1b data point colours in variable for Figure1a 
  arrange(x)

Figure1a_spatial_df = joinCountryData2Map(Impact_balancing_R0,                                                #Create a spatial polygon dataframe of countries from Prem                                      
                                          joinCode = "ISO3",
                                          nameJoinColumn = "iso3c")
# Figure1a_spatial_df <- fortify(Figure1a_spatial_df)

library(ggsn)
library(prettymapr)
Figure1a <- mapCountryData(Figure1a_spatial_df,                                                                           #Plot distribution of Coy imbalance globally using colourscheme from Figure1b 
               nameColumnToPlot = 'C_oy_imbalance',                                                           #NOTE: manually saved plot and merged with Figure 1b outside of R workspace
               catMethod = "fixedWidth",
               numCats = 177,
               colourPalette = Figure1_colours$colour,                                                        #Colour countries according to colour palette from Figure1b
               addLegend = F,
               borderCol = "black",
               mapTitle = "",
               missingCountryCol = "grey50",
               lwd = 1)
addnortharrow(pos = 'topright', 
              scale = 0.3,
              padin = c(0.2, 1.8))

# addscalebar(pos = 'topright', 
#             style='ticks',
#             plotunit = 'km',
#             padin = c(0.2, 1.7),
#             labelpadin = 0.2) # can control the position by margin
Figure1a <- recordPlot()
dev.off()


pdf("./Figures/Figures_revision/Figure1a.pdf",6,4)

Figure1a
dev.off()


# Figure 1 Panels
library(gridGraphics)



pdf("./Figures/Figures_revision/Figure1.pdf",6,7.8)
Figure1 = list(Figure1a, Figure1b)
ggpubr::ggarrange(plotlist = Figure1, nrow = 2, widths = c(1.2,1), heights = c(1,1))
dev.off()

# mtext('abs')

# Figure1a <- ggplot(data = Figure1a_spatial_df) +
#   geom_sf(data = Figure1a_spatial_df,aes(fill = C_oy_imbalance)) +
#   scale_fill_viridis(name = 'c') +
#   theme(legend.position = c(0.8, 0.2))+
#   theme_void()
# Figure1a
# 
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl)
# addnortharrow()
# addscalebar()
# plot(1:5, 1:5, asp=1)
# addnortharrow()
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl, xlim=c(-66.86, -59.75), ylim=c(43, 47.3)) #Nova Scotia
# addscalebar()
# 
# library(ggmap)
# library(maps)
# 
# ggplot(Figure1a_spatial_df, aes(fill = Figure1a_spatial_df$C_oy_imbalance)) +
#   geom_sf() +
#   scalebar(Figure1a_spatial_df, dist = 4, dist_unit = "km",
#            transform = FALSE, model = "WGS84") +
#   blank() +
#   scale_fill_continuous(low = "#fff7ec", high = "#7F0000")
# 
# ggplot(data=Figure1a_spatial_df,aes(x=long, y=lat, group=group)) +
#   geom_polygon(aes(fill=Province),alpha=0.5)+
#   geom_point(data = full, aes(x=Long, y=Lat,group=Source,color=Source, shape=Source),size =1.5,alpha=0.8,position=position_jitter(width=0.025, height=0.03)) +
#   scale_color_manual(values =c("#253582ff","#de7065ff"))+
#   scale_fill_viridis(discrete=T, option = "E", begin = 0.7, end=0.2) +
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   scalebar(data=SLdat,border.size = 0.2,model = 'WGS84', dist = 50, transform = T, dist_unit="km", st.size=2)+
#   xlab("Longitude")+
#   ylab("Latitude")+
#   north(data=Figure1a_spatial_df, symbol=4, location="bottomleft", scale=.1)



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
  scale_color_continuous_diverging("Blue-Red", trans = "log", limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))  #colour data points according to imbalance in contacts <15 reported with 15+ (Cyo)

FigureS2b

ggsave(file= paste0("./Figures/Figures_revision/FigureS2b.pdf"), 
       width = 6.5, height = 4, limitsize = FALSE, FigureS2b)                                                 #Save FigureS1b as .png file

## FigureS2a ##

FigureS2_colours = ggplot_build(FigureS2b)$data[[1]] %>%                                                      #save FigureS1b data point colours in variable for FigureS1a 
  arrange(x)

FigureS2a <- mapCountryData(Figure1a_spatial_df,                                                                           #Plot distribution of Cyo imbalance globally using colourscheme from FigureS1b
               nameColumnToPlot = 'C_yo_imbalance',                                                           #NOTE: uses spatial data frame created for Figure 1 must manually save this figure and merge with 1a outside of R
               catMethod = "fixedWidth",                                                                      #NOTE:manually saved plot and merged with Figure1b outside of R workspace
               numCats = 177,                                                                                
               colourPalette = FigureS2_colours$colour,                                                       #Colour countries according to colour palette from FigureS1b
               addLegend = F,
               borderCol = "black",
               mapTitle = "",
               missingCountryCol = "grey50",
               lwd = 1)                                                             #If countries not included in Prem analysis, colour in grey
addnortharrow(pos = 'topright', 
              scale = 0.3,
              padin = c(0.2, 1.8))

FigureS2a <- recordPlot()
dev.off()


pdf("./Figures/Figures_revision/FigureS2a.pdf",6,4)

FigureS2a
dev.off()

pdf("./Figures/Figures_revision/FigureS2.pdf",6,7.8)
FigureS2 = list(FigureS2a, FigureS2b)
ggpubr::ggarrange(plotlist = Figure1, nrow = 2, widths = c(1.2,1), heights = c(1,1))
dev.off()
############################################ Aim 2: Impact Transmission Dynamics ###############################
library(extrafont)
font_import()
loadfonts(device = "win")
### Clean labels from model output ###
load("./Output/Aim2_SimulationOutput.RData")
geq <- "\u2265"
less <- '\u003C'

ModelOutput_cleaned$Age.Group = factor(ModelOutput_cleaned$Age.Group,                     #Re-order age group labels for Figure2 plot                            
                                       levels = c("Overall", "<15","15+"))
ModelOutput_cleaned$context   = factor(ModelOutput_cleaned$context,                       #Re-label and reorder demographic setting labels
                                       levels = c("GMB", "LUX", "SGP"),      
                                       labels = c("Gambia", "Luxembourg", "Singapore")) 
ModelOutput_cleaned$balance   = factor(ModelOutput_cleaned$balance,                       #Re-label (capitalize) and reorder balance labels 
                                       levels = c("balanced", "imbalanced"),  
                                       labels = c("Balanced", "Imbalanced"))

### Figure 2: Bias in SARS-CoV-2 epidemic trajectory overall and among age groups according to imbalance in synthetic contact matrix ###



strip_labels <- expression(
  'Overall' = Overall,
  '<15' =  paste('\u003C','15'),
  '15+' =  paste('\u2265','15')
)

library('ggthemes')



# Figure2 =
#   ggplot(subset(ModelOutput_cleaned, vax == 0),                                   #Plot epidemic curve up to 365 days post seeding with no vaccination
#          aes(x = time,                                                            #Plot time as x variable
#              y = I/N*100000,                                                      #plot incidence per 100,000 as y variable
#              group = factor(balance),                                             #Factor plots according to balance of model
#              color= factor(balance))) +                                           #Colour line according to balance of model
#   geom_line(size= 0.9) +
#   xlab(label= "Time, days") +                                                    #Label x-axis
#   ylab(label= "Incidence per 100,000") +                                        #Label y-axis
#   labs(color = "Contact Matrix") +                                               #Label legend
#   theme_tufte() + 
#   scale_x_continuous(limits = c(0, 365),                                          #Fix x-axis scale limits and break points
#                      breaks = c(0,100,200,300,365)) +
#   scale_y_continuous(limits = c(0, 18000),                                          #Fix y-axis scale limits and break points
#                      breaks = c(0,5000,10000,15000,18000),
#                      label = c('0','5,000','10,000','15,000','18,000')) +
#   scale_color_manual(values = c("#696969", "#B54B4B")) +                          #Fix colour scheme of lines according to balance of model
#   facet_wrap(~ context + Age.Group,                                              #Facet wrap by age group (Overall, <15 and 15+) and demographic setting (Gambia, Luxembourg and Singapore)
#              scales = "free",
#              labeller = labeller(Age.Group = label_expressions(strip_labels))) +
#   labs(color = bquote(underline("Contact Matrix"))) + 
#   theme(text = element_text(family = 'sans'),
#         strip.background = element_blank(),
#         # strip.text.x = element_blank(),
#         # strip.text.y = element_blank(),
#         axis.line = element_line(colour = 'black', linewidth = 1/.pt),
#         # legend.position = c(0.8, 0.22),
#         legend.position = 'none',
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
#         legend.margin = margin(t= 0, b = 0, r = 0, l = 0, unit = 'pt'),
#         legend.title.align = 0.5,
#         legend.text = element_text(size = 11),
#         legend.title = element_text(size = 11),
#         axis.text = element_text(colour = 'black', size = 11),
#         axis.title = element_text(size = 11))
# 
# 
# Figure2

FigureS3 =                                                                                                     
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

FigureS3

ggsave(file= paste0("./Figures/Figures_revision/FigureS3.pdf"),                                     #save Figure 2 as a .png file
       width = 8, height = 6, limitsize = FALSE, FigureS3)



# ggsave(file= paste0("./Figures/Figures_revision/Figure2.pdf"),                                     #save Figure 2 as a .png file
#        width = 7, height = 6, limitsize = FALSE, Figure2)



### Figure 3: Imbalanced contact matrices bias estimates of cumulative SARS-CoV-2 infections overall and among subgroups ###

## Figure3a ##
library(ggforce)

Figure3a =                                                                        #Plot heat map of contact matrices per demographic setting to visually demonstrate imbalances 
  ggplot(C_sub_df,
         aes(x = age.ind,                                                         #Age of individual on x-axis 
             y = age.contact)) +                                                  #Age of contact on y-axis 
  geom_raster(aes(fill = C_ratio)) +                                              #Fill heat map according to population contacts age group i reported with age group j vs. balanced                                                                    #   population contacts between i and j
  ylab(label= "Contact Age") +                                                  #Label y axis
  xlab(label= "Individual Age") +                                               #Label x axis 
  labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                  #Label legend
  facet_wrap_paginate(~setting, nrow = 1,scales = "free",page=3)+
  # facet_wrap(~setting, nrow = 1,scales = "free") +                                                #Facet by demographic setting
  scale_fill_continuous_diverging("Blue-Red", trans = "log", 
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))+    #Fix fill colour, limits and breaks of heatmap
  scale_x_discrete(labels = c("<15",expression("">= 15))) +
  scale_y_discrete(labels = c("<15",expression("">= 15))) +
  theme_tufte() +
  theme(text = element_text(family = 'sans'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.line = element_line(colour = 'black', linewidth = 1/.pt),
        # legend.position = c(0.5,0.5),
        legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
        legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        axis.text = element_text(colour = 'black', size = 11),
        axis.title = element_text(size = 11))
Figure3a

pdf("./Figures/Figures_revision/Figure3a.pdf",6,6)

Figure3a
dev.off()

# Create a separate plot for each value of cyl, and store each plot in a list
Fig3.list = lapply(sort(unique(mtcars$cyl)), function(i) {
  ggplot(mtcars[mtcars$cyl==i,], aes(mpg, disp, colour=factor(cyl))) +
    geom_point(show.legend=FALSE) +
    facet_wrap(~cyl) +
    scale_colour_manual(values=hcl(seq(15,365,length.out=4)[match(i, sort(unique(mtcars$cyl)))], 100, 65))
})

ggsave(file= paste0("./Figures/Figures_revision/Figure3a.pdf"),                                     #save Figure 2 as a .png file
       width = 6, height = 6, limitsize = FALSE, Figure3a)
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
  labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                     #Label legend
  scale_x_discrete(labels = c('Overall',"<15",expression("">= 15))) +
  facet_wrap(~context, nrow = 1,scales = "free") + 
  theme_tufte() +
  theme(text = element_text(family = 'sans'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.line = element_line(colour = 'black', linewidth = 1/.pt),
        legend.position = c(0.5,0.4),
        # legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
        legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        axis.text = element_text(colour = 'black', size = 11),
        axis.title = element_text(size = 11)) +
  scale_fill_continuous_diverging(palette = "Blue-Red", trans = "log",             #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))
Figure3b

ggsave(file= paste0("./Figures/Figures_revision/Figure3b.pdf"),                                     #save Figure 2 as a .png file
       width = 6, height = 6, limitsize = FALSE, Figure3b)

Figure3 = ggarrange(Figure3a,                                                        #Merge Figure 3a and 3b
                    Figure3b,
                    align = "hv",
                    nrow = 2,
                    ncol = 1,
                    # labels = "AUTO",
                    heights = c(1,2)
                    )

Figure3
 
ggsave(file= paste0("./Figures/Figures_revision/Figure3.pdf"),                                        #Save Figure 3 as a .png file
       width = 6, height = 6, limitsize = FALSE, Figure3)


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
  labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                    #Label legend
  facet_wrap(~context, nrow = 1, scales = 'free') + 
  scale_x_discrete(labels = c('Overall',"<15",expression("">= 15))) +
  theme_tufte() +
  theme(text = element_text(family = 'sans'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.line = element_line(colour = 'black', linewidth = 1/.pt),
        legend.position = c(0.5,0.75),
        # legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
        legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        axis.text = element_text(colour = 'black', size = 11),
        axis.title = element_text(size = 11)) +                              #Stratify by demographic context
  scale_fill_continuous_diverging(palette = "Blue-Red", trans = "log",            #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))

Figure4a

ggsave(file= paste0("./Figures/Figures_revision/Figure4a.pdf"),                                        #Save Figure 3 as a .png file
       width = 6, height = 6, limitsize = FALSE, Figure4a)

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
  labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                    #Label legend
  facet_wrap(~context, nrow = 1, scales = 'free') +                                                  #Stratify by demographic context
  scale_fill_continuous_diverging(palette = "Blue-Red", trans = "log",            #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                  limits = c(0.4, 2.5), breaks = c(0.5, 1, 2)) +
  scale_x_discrete(labels = c('Overall',"<15",expression("">= 15))) +
  theme_tufte() +
  theme(text = element_text(family = 'sans'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.line = element_line(colour = 'black', linewidth = 1/.pt),
        # legend.position = c(0.5,0.6),
        legend.position = 'none',
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
        legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        axis.text = element_text(colour = 'black', size = 11),
        axis.title = element_text(size = 11))          

Figure4b

ggsave(file= paste0("./Figures/Figures_revision/Figure4b.pdf"),                                        #Save Figure 3 as a .png file
       width = 6, height = 6, limitsize = FALSE, Figure4b)

Figure4 = ggarrange(Figure4a + rremove("x.title"),                                                       #Merge Figure 4a and 4b
                    Figure4b,
                    align = "hv",
                    nrow = 2,
                    ncol = 1,
                    common.legend = F)

Figure4

ggsave(file= paste0("./Figures/Figures_revision/Figure4.pdf"),                                       #Save Figure 4 as .png file
       width = 6.5, height = 7.6, limitsize = FALSE, Figure4)

################################
# Save facet panel individually


Fig2a.list = lapply(seq_along(unique(C_sub_df$setting)), function(page) {
  ggplot(C_sub_df,
         aes(x = age.ind,                                                         #Age of individual on x-axis 
             y = age.contact)) +                                                  #Age of contact on y-axis 
    geom_raster(aes(fill = C_ratio)) +                                              #Fill heat map according to population contacts age group i reported with age group j vs. balanced                                                                    #   population contacts between i and j
    ylab(label= "Contact Age") +                                                  #Label y axis
    xlab(label= "Individual Age") +                                               #Label x axis 
    labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                  #Label legend
    facet_wrap_paginate(~setting, nrow = 1, ncol = 1,scales = "free",page=page)+
    # facet_wrap(~setting, nrow = 1,scales = "free") +                                                #Facet by demographic setting
    scale_fill_continuous_diverging("Blue-Red", trans = "log", 
                                    limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))+    #Fix fill colour, limits and breaks of heatmap
    scale_x_discrete(labels = c("<15",expression("">= 15))) +
    scale_y_discrete(labels = c("<15",expression("">= 15))) +
    theme_tufte() +
    theme(text = element_text(family = 'sans'),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.line = element_line(colour = 'black', linewidth = 1/.pt),
          # legend.position = c(0.5,0.5),
          legend.position = 'none',
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
          legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
          legend.title.align = 0.5,
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
          axis.text = element_text(colour = 'black', size = 11),
          axis.title = element_text(size = 11))
})

ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure2A.pdf"),                                       #Save Figure 4 as .png file
       width = 2.2, height = 1.75, limitsize = FALSE, Fig2a.list[[1]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure2B.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 1.75, limitsize = FALSE, Fig2a.list[[2]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure2C.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 1.75, limitsize = FALSE, Fig2a.list[[3]])




Fig2b.list = lapply(seq_along(unique(CumI_difference_plots$context)), function(page) {
  ggplot(CumI_difference_plots,
         aes(x = Age.Group,                                                          #Plot age groups on x axis
             y = CumI_percent_change)) +                                             #Plot percent change in cumulative infections per age group on y axis
    geom_col(width = 0.3, position = "dodge", aes(fill = C_ratio), color = "black") +  #Plot a box plot, colored by the ratio of imbalance in contacts per age group and demographic setting 
    geom_hline(yintercept = 0, linetype = "dashed") +                                  #Draw horizontal line at 0 where there is no difference in cumulative infections
    scale_y_continuous(limits = c(-12, 4),                                             #Set y axis limits
                       breaks = seq(-12, 4, 2)) + 
    xlab(label= "Age Group") +                                                         #Label x axis
    ylab(label= "% Difference in Cumulative Infections \n (imbal vs. bal)") +           #Label y axis                                                                    
    labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                     #Label legend
    scale_x_discrete(labels = c('Overall',"<15",expression("">= 15))) +
    facet_wrap_paginate(~context, nrow = 1, ncol = 1,scales = "free",page=page)+
    theme_tufte() +
    theme(text = element_text(family = 'sans'),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.line = element_line(colour = 'black', linewidth = 1/.pt),
          legend.position = c(0.5,0.4),
          # legend.position = 'none',
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
          legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
          legend.title.align = 0.5,
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          # axis.title.x = element_blank(),
           axis.title.y = element_blank(),
          axis.text = element_text(colour = 'black', size = 11),
          axis.title = element_text(size = 11)) +
    scale_fill_continuous_diverging(palette = "Blue-Red", trans = "log",             #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                    limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))
})

ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure2D.pdf"),                                       #Save Figure 4 as .png file
       width = 2.2, height = 4.5, limitsize = FALSE, Fig2b.list[[1]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure2E.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 4.5, limitsize = FALSE, Fig2b.list[[2]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure2F.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 4.5, limitsize = FALSE, Fig2b.list[[3]])



Fig3a.list = lapply(seq_along(unique(Impact_vax1_wC$context)), function(page) {
  ggplot(Impact_vax1_wC,
         aes(x = Age.Group,                                                         #Plot age group on x axis
             y = Diff_inf_averted)) +                                               #Plot percent difference in infections averted from age-specifc vaccination strategy 1 on y axis 
    geom_col(width = 0.3, position = "dodge", aes(fill = C_ratio), color = "black") + #Plot a box plot, colored by the ratio of imbalance in contacts per age group and demographic setting 
    geom_hline(yintercept = 0, linetype = "dashed") +                                 #Draw horizontal line at 0 where there is no difference in infections averted from imbalanced to balanced models
    scale_y_continuous(limits = c(-25, 40),                                           #Set y axis limits
                       breaks = seq(-24, 40, 8)) + 
    xlab(label= "Age Group") +                                                        #Label x axis
    ylab(label= "% Difference in Infections Averted \n (imbal vs. bal)") +            #Label y axis                                           
    labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                    #Label legend
    facet_wrap_paginate(~context, nrow = 1, ncol = 1,scales = "free",page=page)+
    scale_x_discrete(labels = c('Overall',"<15",expression("">= 15))) +
    theme_tufte() +
    theme(text = element_text(family = 'sans'),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.line = element_line(colour = 'black', linewidth = 1/.pt),
          legend.position = c(0.5,0.75),
          #legend.position = 'none',
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
          legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
          legend.title.align = 0.5,
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(colour = 'black', size = 11),
          axis.title = element_text(size = 11)) +                              #Stratify by demographic context
    scale_fill_continuous_diverging(palette = "Blue-Red", trans = "log",            #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                    limits = c(0.4, 2.5), breaks = c(0.5, 1, 2))
})

ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure3A.pdf"),                                       #Save Figure 4 as .png file
       width = 2.2, height = 3.8, limitsize = FALSE, Fig3a.list[[1]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure3B.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 3.8, limitsize = FALSE, Fig3a.list[[2]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure3C.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 3.8, limitsize = FALSE, Fig3a.list[[3]])



Fig3b.list = lapply(seq_along(unique(Impact_vax2_wC$context)), function(page) {
  ggplot(Impact_vax2_wC,
         aes(x = Age.Group,                                                         #Plot age group on x axis
             y = Diff_inf_averted)) +                                               #Plot percent difference in infections averted from age-specific vaccination strategy 2 on y axis 
    geom_col(width = 0.3, position = "dodge", aes(fill = C_ratio), color = "black") + #Plot a box plot, colored by the ratio of imbalance in contacts per age group and demographic setting 
    geom_hline(yintercept = 0, linetype = "dashed") +                                 #Draw horizontal line at 0 where there is no difference in infections averted from imbalanced to balanced models
    scale_y_continuous(limits = c(-25, 40),                                           #Set y axis limits
                       breaks = seq(-24, 40, 8)) + 
    xlab(label= "Age Group") +                                                        #Label x axis
    ylab(label= "% Difference in Infections Averted \n (imbal vs. bal)") +            #Label y axis
    labs(fill = bquote(underline(italic(C)['imbal']~'/'~italic(C)['bal']))) +                                    #Label legend
    facet_wrap_paginate(~context, nrow = 1, ncol = 1,scales = "free",page=page)+                                                  #Stratify by demographic context
    scale_fill_continuous_diverging(palette = "Blue-Red", trans = "log",            #Define colour palette to fill plot according to limits of Cimbal/Cbal
                                    limits = c(0.4, 2.5), breaks = c(0.5, 1, 2)) +
    scale_x_discrete(labels = c('Overall',"<15",expression("">= 15))) +
    theme_tufte() +
    theme(text = element_text(family = 'sans'),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.line = element_line(colour = 'black', linewidth = 1/.pt),
          
          legend.position = 'none',
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = 'black',linewidth = 1/.pt),
          legend.margin = margin(t= 5, b = 5, r = 10, l = 10, unit = 'pt'),
          legend.title.align = 0.5,
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(colour = 'black', size = 11),
          axis.title = element_text(size = 11))     
})

ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure3D.pdf"),                                       #Save Figure 4 as .png file
       width = 2.2, height = 3.8, limitsize = FALSE, Fig3b.list[[1]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure3E.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 3.8, limitsize = FALSE, Fig3b.list[[2]])
ggsave(file= paste0("./Figures/Figures_revision/panel_individual/Figure3F.pdf"),                                       #Save Figure 4 as .png file
       width = 1.9, height = 3.8, limitsize = FALSE, Fig3b.list[[3]])


     
