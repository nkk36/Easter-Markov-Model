# Setup ====

# Load packages
library(dplyr)
library(extrafont)
library(ggplot2)
library(magrittr)
library(matrixcalc)
library(RColorBrewer)

# Load functions

source("R/helper_functions.R")

# Run Markov Model ====

# Build transition matrix
m_matrix = build_transition_matrix(n_total_eggs = 30, 
                                   length = 5, 
                                   width = 5, 
                                   clearance_rate = 0.25, 
                                   n_sweepers = 1)

# Calculate probability of clearing entire field within time
results = matrix_power_loop(matrix = m_matrix, max_power = NULL)

# Plot results 

results[[2]] = results[[2]] %>%
  mutate(Time_Days = Time/24)

ggplot(data = results[[2]]) + 
  geom_line(mapping = aes(x = Time_Days, y = Probability)) + 
  scale_x_continuous(limits = c(0,round(max(results[[2]]$Time_Days))), breaks = seq(0,round(max(results[[2]]$Time_Days)),0.5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) + 
  labs(title = "Probability of Clearing Field of all Eggs",
       x = "Time to Clear Field (Days)",
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
                                     clearance_rate = 0.25, 
                                     n_sweepers = sweepers)
  
  # Calculate probability of clearing entire field within time
  results = matrix_power_loop(matrix = m_matrix, max_power = 200)
  out[[sweepers]] = results
}

out2 = data.frame(Time = rep(out[[1]]$prob$Time,4), 
                 Probability = c(out[[1]]$prob$Probability, out[[2]]$prob$Probability, out[[3]]$prob$Probability, out[[4]]$prob$Probability),
                 Result = c(rep("1 sweeper", 200), rep("2 sweepers", 200), rep("3 sweepers", 200), rep("4 sweepers", 200)))

out2 = out2 %>%
  mutate(Time_Days = Time/24)

# Plot results 

ggplot(data = out2) + 
  geom_line(mapping = aes(x = Time_Days, y = Probability, colour = Result), 
            size = 1) + 
  scale_x_continuous(limits = c(0,7), breaks = seq(0,7,0.5)) + 
  scale_y_continuous(limits = c(-0.02,1.02), breaks = seq(0,1,0.1)) + 
  scale_colour_manual(values = brewer.pal(n = 4, name = "Paired"),
                      name = "Number of Sweepers") + 
  labs(title = "Probability of Clearing Field of all Eggs",
       x = "Time to Clear Field (Days)",
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
# Eggs uniformly distributed ====

set.seed(124)

egg_locations = data.frame(x = runif(n = 30, min = 0, max = 5),
                           y = runif(n = 30, min = 0, max = 5))

# One sweeper
ggplot(data = egg_locations) + 
  geom_point(mapping = aes(x = x, y = y), colour = "red") + 
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 0, ymax = 5), 
            fill = brewer.pal(n = 4, name = "Dark2")[1], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 0 + (5 - 0)/2, label = "One Sweeper"), size = 4) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  labs(title = "30 Eggs Randomly (Uniformly) Distributed") + 
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        text = element_text(family = "Cambria"))

# Two sweepers
ggplot(data = egg_locations) + 
  geom_point(mapping = aes(x = x, y = y), colour = "red") +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 0, ymax = 2.5), 
            fill = brewer.pal(n = 4, name = "Dark2")[2], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 2.5 + (5 - 2.5)/2, label = "Sweeper #2"), size = 4) +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 2.5, ymax = 5), 
            fill = brewer.pal(n = 4, name = "Dark2")[1], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 0 + (2.5 - 0)/2, label = "Sweeper #1"), size = 4) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  labs(title = "30 Eggs Randomly (Uniformly) Distributed") + 
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        text = element_text(family = "Cambria"))

# Three sweepers
ggplot(data = egg_locations) + 
  geom_point(mapping = aes(x = x, y = y), colour = "red") +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 10/3, ymax = 5), 
            fill = brewer.pal(n = 4, name = "Dark2")[1], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 10/3 + (5 - 10/3)/2, label = "Sweeper #3"), size = 4) +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 5/3, ymax = 10/3), 
            fill = brewer.pal(n = 4, name = "Dark2")[2], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 5/3 + (10/3 - 5/3)/2, label = "Sweeper #2"), size = 4) +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 0, ymax = 5/3), 
            fill = brewer.pal(n = 4, name = "Dark2")[3], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 0 + (5/3 - 0)/2, label = "Sweeper #1"), size = 4) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  labs(title = "30 Eggs Randomly (Uniformly) Distributed") + 
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        text = element_text(family = "Cambria"))  

# Four sweepers
ggplot(data = egg_locations) + 
  geom_point(mapping = aes(x = x, y = y), colour = "red") +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 15/4, ymax = 5), 
            fill = brewer.pal(n = 4, name = "Dark2")[1], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 15/4 + (5 - 15/4)/2, label = "Sweeper #4"), size = 4) +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 10/4, ymax = 15/4), 
            fill = brewer.pal(n = 4, name = "Dark2")[2], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 10/4 + (15/4 - 10/4)/2, label = "Sweeper #3"), size = 4) +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 5/4, ymax = 10/4), 
            fill = brewer.pal(n = 4, name = "Dark2")[3], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 5/4 + (10/4 - 5/4)/2, label = "Sweeper #2"), size = 4) +
  geom_rect(mapping=aes(xmin = 0, xmax = 5, ymin = 0, ymax = 5/4), 
            fill = brewer.pal(n = 4, name = "Dark2")[4], colour = "black", alpha = 0.0025) +
  geom_text(mapping = aes(x = 0 + (5 - 0)/2, y = 0 + (5/4 - 0)/2, label = "Sweeper #1"), size = 4) +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1)) + 
  labs(title = "30 Eggs Randomly (Uniformly) Distributed") + 
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        text = element_text(family = "Cambria"))