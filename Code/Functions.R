########################################### Code Summary ##############################################
### TITLE:            Functions.R
### BY:               Mackenzie Hamilton 
### DATE CREATED:     2022/03/03
### DESCRIPTION:      Functions to balance and transform contact matrices from Prem et al.
########################################### Functions ##############################################


transform_age = function(data){                      #Function to transform the age structure of a contact matrix or vector from 5 year 
                                                     # increments (up to 75+) to 2 groups <15 and 15+ 
   
  if(class(data)[1] == "matrix"){                    #action if data is a matrix
    
    summed_rows = matrix(, nrow=16, ncol = 2)        #initialize an empty matrix with 16 rows (equal to nrow in input matrix) and 2 columns
    
    i = 1                                            #index for rows
    
    while(i <= nrow(data)){                          #index through rows and transform column data from old age structure to new
      
      summed_rows[i,1] = sum(data[i,1:3]) 
      summed_rows[i,2] = sum(data[i,4:16]) 
      
      i = i + 1
      
    }
    
    transformed_matrix = matrix(,nrow = 2, ncol = 2) #initialize an empty 2x2 matrix to save transformed data in
    
    j = 1                                            #index for columns
    
    while(j <= ncol(summed_rows)){                   #index through columns of "summed_rows" and transform row data from old age structure to new
      
      transformed_matrix[1,j] = sum(summed_rows[1:3,j])
      transformed_matrix[2,j] = sum(summed_rows[4:16,j])
      
      j = j + 1
      
    }
    
    rownames(transformed_matrix) = c("<15", "15+")  #name rows and columns of transformed_matrix following new age groups
    colnames(transformed_matrix) = c("<15", "15+")
    
    
    return(transformed_matrix)                       #output transformed_matrix
    
      
  }else if(class(data)[1] == "numeric"){             #action if data is a vector
    
    summed_vector = vector(,length = 2)              #initialize an empty vector of length 2
    
    summed_vector[1] = sum(data[1:3])
    summed_vector[2] = sum(data[4:16])
    
    return(summed_vector)                            #output transformed vector
    
  }else{                                             #return error if data class is not matrix or vector
    
    simpleError("Error in class(data): Unknown data class")
    
  }
  
}


transform_age2 = function(data){                     #Function to transform the age structure of a contact matrix or vector from 5 year 
                                                     # increments (up to 75+) to 2 groups <40 and 40+ (for sensitivity analysis) 
  
  if(class(data)[1] == "matrix"){                    #action if data is a matrix
    
    summed_rows = matrix(, nrow=16, ncol = 2)        #initialize an empty matrix with 16 rows (equal to nrow in input matrix) and 2 columns
    
    i = 1                                            #index for rows
    
    while(i <= nrow(data)){                          #index through rows and transform column data from old age structure to new
      
      summed_rows[i,1] = sum(data[i,1:8]) 
      summed_rows[i,2] = sum(data[i,9:16]) 
      
      i = i + 1
      
    }
    
    transformed_matrix = matrix(,nrow = 2, ncol = 2) #initialize an empty 2x2 matrix to save transformed data in
    
    j = 1                                            #index for columns
    
    while(j <= ncol(summed_rows)){                   #index through columns of "summed_rows" and transform row data from old age structure to new
      
      transformed_matrix[1,j] = sum(summed_rows[1:8,j])
      transformed_matrix[2,j] = sum(summed_rows[9:16,j])
      
      j = j + 1
      
    }
    
    rownames(transformed_matrix) = c("<40", "40+")  #name rows and columns of transformed_matrix following new age groups
    colnames(transformed_matrix) = c("<40", "40+")
    
    
    return(transformed_matrix)                       #output transformed_matrix
    
    
  }else if(class(data)[1] == "numeric"){             #action if data is a vector
    
    summed_vector = vector(,length = 2)              #initialize an empty vector of length 2
    
    summed_vector[1] = sum(data[1:8])
    summed_vector[2] = sum(data[9:16])
    
    return(summed_vector)                            #output transformed vector
    
  }else{                                             #return error if data class is not matrix or vector
    
    simpleError("Error in class(data): Unknown data class")
    
  }
  
}


intensive_to_extensive = function(c_list, pop_list){              #Function to transform list of contact matrices from individual scale to population scale
  
 i = 1                                                            
 
 pop_c_list = vector(mode = "list", length = length(c_list))      #initialize list
 
 while(i <= length(c_list)){                                      #iterate through list of contact matrices

      pop_c_list[[i]] = c_list[[i]]*pop_list[[i]]                 #assign extensive form of contact matrix at index i to pop_c_list at index i
   
      i = i + 1
 }

 names(pop_c_list) = names(c_list)                                #name list elements following names in contact matrix list
 return(pop_c_list)                                               #output extensive C list

}

extensive_to_intensive = function(pop_c_list, pop_list){          #Function to transform list of contact matrices from population scale to individual scale
  
  i = 1
  
  c_list = vector(mode = "list", length = length(pop_c_list))     #initialize list
  
  while(i <= length(c_list)){                                     #iterate through list of input contact matrices
    
    c_list[[i]] = pop_c_list[[i]]/pop_list[[i]]                   #divide input contact matrix at i by population at i and assign to c_list
    
    i = i + 1
  }
  
  names(c_list) = names(pop_c_list)                               #name list elements following names in input contact matrix list
  return(c_list)                                                  #output intensive C list
  
}



balance_pop_c_matrix = function(pop_contact_data){                                           #Function to balance contact matrices according to available population contacts
  
  pop_balanced_matrix = matrix(,nrow=ncol(pop_contact_data), ncol = ncol(pop_contact_data))  #initialize an empty matrix of same size as contact_data to store balanced matrix in
  
  i = 1                                                                                      #index for rows
  
  while(i <= nrow(pop_contact_data)){                                                        #iterate through rows of contact_data
    
    j = 1                                                                                    #index for columns
    
    while(j <=ncol(pop_contact_data)){                                                       #iterate through rows' columns of contact_data  
      
      c.ind = pop_contact_data[i,j]                                                          #assign population contacts of individual at row i column j to c.ind
      c.cont = pop_contact_data[j,i]                                                         #assign population contacts of c.ind's contact to c.cont
      
      pop_balanced_matrix[i,j] = (c.ind + c.cont)/2                                          #calculate average population contacts between c.ind and c.cont (in essence, weighted by size of the ind and cont population)
      
      j = j+1
    }
    
    i = i+1
  }

  return(pop_balanced_matrix)                                                                #output balanced matrix
  
}


change_bias_direction_c = function(pop_contact_data){
  
  pop_c_matrix_new      = matrix(,nrow=ncol(pop_contact_data), ncol = ncol(pop_contact_data))     #initialize an empty matrix of same size as 
  pop_c_matrix_new[1,1] = pop_contact_data[1,1]                                                   # contact_data to store new matrix in
  pop_c_matrix_new[1,2] = pop_contact_data[2,1]
  pop_c_matrix_new[2,1] = pop_contact_data[1,2]
  pop_c_matrix_new[2,2] = pop_contact_data[2,2]
  
  return(pop_c_matrix_new)  
}


R0_matrix = function(C_list, N_list, beta, gamma){                    #Function to calculate list of 2x2 matrix of baby r0s (i.e. R0s per subgroup) 
                                                                      # from list of 2x2 contact matrix 
  R0_list = vector(mode = "list", length = length(C_list))            #Initialize list to save baby r0 matrices in (per country)
  
  i = 1 
  
  while(i<= length(C_list)){                                          #Iterate through list of input contact matrices
    
    N_i = N_list[[i]]                                                 #Assign population vector N_i according to input list of population vectors
    C_i = C_list[[i]]                                                 #Assign matrix C_i according to input list of contact matrices

    #R0 = BND
    R0_matrix = matrix(,nrow=2, ncol=2)                               #Initialize baby R0 matrix
    
    R0_matrix[1,1] = (beta*C_i[1,1]/N_i[1])*N_i[1]*(1/gamma)          #assign R0_matrix values according to probability of transmission (beta), 
    R0_matrix[1,2] = (beta*C_i[1,2]/N_i[2])*N_i[1]*(1/gamma)          # duration of infectiousness (1/gamma), population vector and contact matrix 
    R0_matrix[2,1] = (beta*C_i[2,1]/N_i[1])*N_i[2]*(1/gamma)
    R0_matrix[2,2] = (beta*C_i[2,2]/N_i[2])*N_i[2]*(1/gamma)
    
    R0_list[[i]] = R0_matrix                                          #assign baby R0 matrix to position i in R0_list
    
    i = i+1
    
  }
  
  names(R0_list) = names(C_list)                                      #name R0_list elements following names in input contact matrix list

  return(R0_list)                                                     #output R0_list
  
}

R0_matrix_determinant = function(R0_matrix_list){                      #Function to calculate maximum matrix determinant (i.e. dominant eigenvalue; applied to 
                                                                       # calculate overall R0 from baby R0 matrix)
  R0_list = vector(mode = "list", length = length(R0_matrix_list))     #Initialize list to save overall R0 per country
  
  i = 1
  
  while(i<= length(R0_list)){                                          #Iterate through list of baby R0 matrices
    
    R0_matrix = R0_matrix_list[[i]]                                    #Assign baby R0_matrix according to R0_matrix at position i in R0_list
    
    a = 1                                                              #solve a, b and c to solve quadratic equations following equation: 
    b = -R0_matrix[1,1] - R0_matrix[2,2]                               # (R0,11 - R0)(R0,22 - R0) - R012*R021 = 0 (see manuscript equation 3)
    c = R0_matrix[1,1]*R0_matrix[2,2] - R0_matrix[1,2]*R0_matrix[2,1] 
    
    R0 = vector(,length = 2)                                           #Initialize R0 vector to save magtrix determinants in
    
    R0[1] = (-b - sqrt(b^2 - 4*a*c))/(2*a)                             #Solve quadratic equation, solution 1 and assign to R0_vector position 1
    R0[2] = (-b + sqrt(b^2 - 4*a*c))/(2*a)                             #Solve quadratic equation, solution 2 and assign to R0_vector position 2
    
    R0_list[[i]] = max(R0)                                             #Assign maximum of R0 vector (i.e. dominant eigenvalue) to position i of R0_list
    
    i = i+1                                                            #Continue to iterate through country baby R0 matrices
    
    
  }
  
  names(R0_list) = names(R0_matrix_list)                               #name R0_list elements following country names in input R0 matrix list
  
  return(R0_list)                                                      #output R0_list
}



model_function = function(iso3c, balance, vax){                    #Function to run SEIR model 
  
  N             = N_list[[iso3c]]                                  #Initialize population size with input country code iso3c
  E.init        = rep(0,2)                                         #Initialize 0 exposed individuals in each age group
  I.init        = rep(1,2)                                         #Initialize 1 infected individual in each age group
  R.init        = rep(0,2)                                         #Initialize 0 recovered individuals in each age group
  CumIncid.init = rep(0,2)                                         #Initialize 0 cumulative incidence in each age group
  
  
  S.init = N - E.init - I.init - R.init                            #Initialize susceptible as population vector minus exposed, infected and recovered vectors
  
  
  #If else to initialize compartments depending on input vaccination strategy: not vaccination (vax == 0), 50% of <15 vaccinated (vax == 1) or 50% of 15+ 
  # vaccinated (vax == 2)
  
  if(vax == 0){                          
    
    inits <-c(S        = S.init,                                   #Assign initial compartments values to inits
              E        = E.init,        
              I        = I.init,        
              R        = R.init,        
              CumIncid = CumIncid.init) 
    
  }else if(vax == 1){
    
    inits <-c(S        = c(S.init[1]*0.5, S.init[2]),               #Assign initial compartments to inits, and move 50% of susceptible individuals <15 to recovered
              E        = E.init,        
              I        = I.init,        
              R        = c(S.init[1]*0.5, 0),        
              CumIncid = CumIncid.init) 
    
  }else if(vax == 2){
    
    inits <-c(S        = c(S.init[1], S.init[2]*0.5),               #Assign initial compartments to inits, and move 50% of susceptible individuals 15+ to recovered
              E        = E.init,        
              I        = I.init,        
              R        = c(0, S.init[2]*0.5),        
              CumIncid = CumIncid.init) 
    
  }
  

  times = seq(0, 365, 1)                                           #Run model for 1 year
  
  parms <- c(beta  = beta,                                         #Set parameters equal to those in the environment where function is run (values set in model.R)
             omega = omega,
             gamma = gamma,
             C     = C)
  
  model_output = as.data.frame(lsoda(inits, times, seir.5, parms)) #Run seir.5 model (from Model.R) and save output. The lsoda function numerically solves the set of coupled differential equations
  
  model_output_long =                                              #Transform model output to long format
    model_output %>%
    pivot_longer(cols      = 2:11,
                 names_to  = c(".value", "Age.Group"),
                 names_sep = "([0-9]+)") %>%
    mutate(N = S + E + I + R)
  
  Age_cat_string = c("<15", "15+")                                  #Name age groups in output
  AgeGroups = rep(Age_cat_string, 366)
  model_output_long$Age.Group = AgeGroups 
  
  model_output_long$context = iso3c                                 #Assign  iso3c country code to new variable context
  model_output_long$balance = balance                               #Assign balance status of contact rates to new variable "balance"
  model_output_long$vax = vax                                       #Assign vax status of model
  
  return(model_output_long)                                         #Output cleaned long-format model output
}

N_stability = function(model_output){          #Function to model changes in population size (N) over time from compartmental model output
                                               #NOTE: given we are modelling a closed population, N should be stable over the study period
  Figure_N_Stability =  
    ggplot(model_output,
           aes(x = time,
               y = N, 
               group = factor(Age.Group), 
               color = factor(Age.Group))) +
    geom_line(size = 0.9) +
    xlab(label= "Time (Days)") +
    ylab(label= "N") +
    labs(color = "Age Group")
  
  return(Figure_N_Stability)
  
}

