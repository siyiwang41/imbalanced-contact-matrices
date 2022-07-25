########################################### Code Summary ##############################################
### TITLE:            Model.R
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022/02/14
### DESCRIPTION:      Code to set up basic SEIR model with parameterization of fixed 
###                   parameters (initialization and parameterization of  contact rates is 
###                   found in other code)
############################################ Code Setup ###############################################

library(readxl)
require(deSolve)
require(tidyverse)
require(ggplot2)
require(socialmixr)

######################################## Model Information #############################################

# Model Type:     Dynamic infectious disease transmission model (SEIR) 

# Stratified by:  2 age groups (0-39, 40+)

# Compartments:   1	  S           Susceptible
#                 2   E           Exposed 
#                 3   I           Infectious
#                 4   R           Recovered 

#                 5  CumIncid     Cumulative incidence

# Parameters:     beta      probability of transmission upon contact
#                 C         contacts per day
#                 omega     rate of infectiousness (1/latent period)
#                 gamma     rate of recovery (1/infectious period)

# Other Notes for Use: x is a vector of length (number of model compartment types)*(number of risk strata), representing the total 
#   number of state variables, and thus total number of coupled ODEs (ordinary differential equations)
#   For the SEIR model, there are 7 model compartments (SEIR). At the start of the function, we fill in the 
#   activity classes for each model compartment type in turn

######################################## Model Code #####################################################

seir.5 <- function(t, x, parms){      #Model Function (input t = time steps, parms = model parameters, x = vector defined above)         
  
  n.compartment <- 5                  #Assign number of compartments: 4 ID compartments + 1 cumulative tracker
  n.group <- length(x)/n.compartment  #Assign number of age groups
  
  #Iterate through x and assign vectors of initialization values to compartments
  S          <- as.matrix(x[1:n.group])
  E          <- as.matrix(x[(n.group+1):(2*n.group)])
  I          <- as.matrix(x[(2*n.group+1):(3*n.group)])
  R          <- as.matrix(x[(3*n.group+1):(4*n.group)])
  
  CumIncid   <- as.matrix(x[(4*n.group+1):(5*n.group)])
  
  N = S+E+I+R
  
  with(as.list(parms),{ 
    
    #Ordinary differential equations
    dS         = -as.vector(beta*S)*as.matrix(C)%*%as.matrix(I/N)                       #Change in susceptible per unit time
    dE         = as.vector(beta*S)*as.matrix(C)%*%as.matrix(I/N) - omega*as.vector(E)   #Change in exposed per unit time
    dI         = omega*as.vector(E) - gamma*as.vector(I)                                #Change in infected per unit time
    dR         = gamma*as.vector(I)                                                     #Change in recovered per unit time
      
    CumIncid   = as.vector(beta*S)*as.matrix(C)%*%as.matrix(I/N)                        #Cumulative infections per unit time
    
    #Output values per compartment over time period 
    out = c(dS,           #1 
            dE,           #2
            dI,           #3
            dR,           #4
            CumIncid)     #5
    
    list(out)
    
  })
}


######################################## Fixed Parameters ################################################

beta    = 0.015 #probability of transmission per contact with an individual in the infectious state (Davies et al. 2020)
omega   = 1/5.5 #rate of infectiousness (1/latent period; Xin et al. 2021 & Cheng et al. 2021)
gamma   = 1/10  #rate of recovery (1/infectious period; Walsh et al. 2020)

