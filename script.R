# Setup ====

# Load packages
library(extrafont)
library(ggplot2)
library(matrixcalc)
library(RColorBrewer)

# Load functions

source("R/helper_functions.R")

# Run Markov Model ====

# Build transition matrix
m_matrix = build_transition_matrix(n_total_eggs = 30, 
                                   length = 5, 
                                   width = 5, 
                                   clearance_rate = 0.05, 
                                   n_sweepers = 1)

# Calculate probability of clearing entire field within time
results = matrix_power_loop(matrix = m_matrix, max_power = NULL)

# Plot results 

ggplot(data = results[[2]]) + 
  geom_line(mapping = aes(x = Time, y = Probability)) + 
  scale_x_continuous(limits = c(0,max(results[[2]]$Time)), breaks = seq(0,max(results[[2]]$Time),5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) + 
  labs(title = "Probability Curve",
       x = "Time to Clear Field",
       y = "Probability") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(face = "bold", size = 18),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        text = element_text(family = "Cambria"))

# Run Markov Model by varying the Number of Sweepers ====
  
out = list()
for (sweepers in 1:4){

  # Build transition matrix
  m_matrix = build_transition_matrix(n_total_eggs = 30, 
                                     length = 5, 
                                     width = 5, 
                                     clearance_rate = 0.3, 
                                     n_sweepers = sweepers)
  
  # Calculate probability of clearing entire field within time
  results = matrix_power_loop(matrix = m_matrix, max_power = 113)
  out[[sweepers]] = results
}

out2 = data.frame(Time = rep(out[[1]]$prob$Time,4), 
                 Probability = c(out[[1]]$prob$Probability, out[[2]]$prob$Probability, out[[3]]$prob$Probability, out[[4]]$prob$Probability),
                 Result = c(rep("1 sweeper", 113), rep("2 sweepers", 113), rep("3 sweepers", 113), rep("4 sweepers", 113)))

# Plot results 

ggplot(data = out2) + 
  geom_line(mapping = aes(x = Time, y = Probability, colour = Result), 
            size = 1) + 
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120,10)) + 
  scale_y_continuous(limits = c(-0.02,1.02), breaks = seq(0,1,0.1)) + 
  scale_colour_manual(values = brewer.pal(n = 4, name = "Paired"),
                      name = "Number of Sweepers") + 
  labs(title = "Probability of Clearing Field of all Eggs",
       x = "Time",
       y = "Probability") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(face = "bold", size = 18),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        text = element_text(family = "Cambria"))

remove(out)