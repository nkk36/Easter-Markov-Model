# Setup ====

# Define functions ====

# Function: Calculate Poisson Probability ====

calculate_poisson_probability = function(length, width, clearance_rate, n_eggs, n_total_eggs, n_sweepers){
  
  area_to_cover = length*width # Area to sweep
  time_to_cover = area_to_cover/(clearance_rate*n_sweepers) # Time it takes to sweep given the area to sweep and number of sweepers
  avg_eggs_per_time = n_total_eggs/time_to_cover # Poisson parameter
  
  probability_output = ((avg_eggs_per_time^n_eggs)/(factorial(n_eggs)))*exp(-avg_eggs_per_time) # Calculate probability
  
  return(probability_output)
  
}

# Function: Build Markov Transition Matrix ====

build_transition_matrix = function(n_total_eggs, length, width, clearance_rate, n_sweepers){
  
  # Initialize transition matrix
  transition_matrix = matrix(rep(0,n_total_eggs^2 + 2*n_total_eggs + 1), nrow = n_total_eggs + 1, ncol = n_total_eggs + 1)
  
  for (i in 0:nrow(transition_matrix)){
    for (j in 0:ncol(transition_matrix)){
      
      if (j >= i){
        transition_matrix[i,j] = calculate_poisson_probability(length = length, 
                                                               width = width,
                                                               clearance_rate = clearance_rate, 
                                                               n_eggs = j - i,
                                                               n_total_eggs = n_total_eggs,
                                                               n_sweepers = n_sweepers)
      }
    }
  }
  
  for (i in 1:nrow(transition_matrix)){
    transition_matrix[i,] = transition_matrix[i,]/sum(transition_matrix[i,])
  }
  
  return(transition_matrix)
  
}
# Function: Raise matrix power successively ====

matrix_power_loop = function(matrix, max_power){
  
  ncol = ncol(matrix)
  temp = matrix
  counter = 1
  i = 2
  out = c()
  out[counter] = temp[1,ncol]
  
  if (is.null(max_power) == T){
  
    while (1 - temp[1,ncol] > 0.001 ){
      
      
      temp = matrix.power(matrix,i)
      
      i = i + 1
      counter = counter + 1
      out[counter] = temp[1,ncol]
    }
  } else {
    for (pow in 2:max_power){
      temp = matrix.power(matrix,pow)
      
      i = i + 1
      counter = counter + 1
      out[counter] = temp[1,ncol]
    }
  }
  
  out = data.frame(Time = 1:(i - 1), Probability = out)
  
  output = list(n = i, prob = out)
  
}