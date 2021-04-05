# Cohort Module
# Goals: Generate cohort Groupings for schools or offices based on minimizing 
# additional connections in the network and keeping the most influential nodes
# of the graph (with high betweenness and degree centralities) contained within
# their circles
# ---- basic components
# return the groupings of nodes that make up the advised cohorts
#
#
#
#
# Sample graph for use in checking and exploring optimization
install.packages("GGally")
install.packages('sna')
install.packages("ROI")
install.packages("ompr")
install.packages("ompr.roi")
install.packages("ROI.plugin.glpk")
install.packages("ROI.plugin.symphony")
install.packages("pracma")
#https://www.r-orms.org/mixed-integer-linear-programming/

library(ROI)
library(ggplot2)
library(igraph)
library(GGally)
library(sna)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)
library(pracma)







sample_g <- erdos.renyi.game(35, .2 )
dev.off()
ggnet2(sample_g, label = TRUE)

# As an optimization problem 
#
# Objective Function
# This can be changed based on what we decide we actually would like to minimize
# The constraint is currently written as follows:
# Minimize the amount of adjacent neighbors for each individual that is not a 
# member of its cohort (sum over all individuals in the network)
# weight the score of the neighbors by their degree
# 
# another objective idea: maximize teams coming in on same days


# Parameters: (THESE NEED TO BE CHANGED TO THE OFFICE SIM AND ARE BASED OFF OF MADE UP NETWORK)
# MY MADE UP NETWORK HAS 35 PEOPLE
n_people <- 35
# Number of cohorts (can adjust this; 5 so that each cohort can work in office once a week)
total_cohorts <- 5
# Minimum amount of people per cohort (to ensure some level of social interaction)
min_cohort_size <- 5
# Maximum amount of people per cohort (to ensure distancing and capacity constraints)
max_cohort_size <- 10
# Matrix of nodes and neighbors (1 if nodes are adjacent; 0 if nodes are not adjacent)
neighbor_mat <- as_adjacency_matrix(sample_g)
as_adjacency_matrix(sample_g)
print(neighbor_mat[31,30])

# Cost matrix of the adjacency for minimization prob (0 if nodes are adjacent; 1 if nodes are not adjacent))
cost_mat <- matrix(0, ncol = n_people, nrow = n_people)
for (i in 1:nrow(cost_mat)) { 
  for (j in 1:ncol(cost_mat)){
    adj <- neighbor_mat[i,j]
    if (adj == 0) {
      cost_mat[i,j] <- 1
    }
  }
}

#List of Decision Variable indices
#n choose 2 subsets of teammates multiplied with total_cohorts
combos <- total_cohorts*nchoosek(n_people, 2)
index_array <- rep(NA, your_length)
for (i in 1:total_cohorts){
  for (j in 1:(n_people)){
    for (k in 1:(n_people-1)){
      if (j < (k+1)){
        print(paste0(i, " ",j," ", k+1))
      }
    }
  }
}
print(size(combos))
# Decision Variables
# X_pc Binary [0,1] variable for assignment of Person p to Cohort c
MIP1_model <- MIPModel() %>%
  
  # 1 iff person p gets assigned to cohort c
  add_variable(X[p, c], p = 1:n_people, c = 1:total_cohorts, type = "binary") %>%

# Objective Function: minimize extra edges from cohort creation or maximize adjacent neighbors; t are teammates
  set_objective(sum_expr(cost_mat(p, t)*X[p, c], p = 1:n_people, c = 1:total_cohorts), "min") %>%
  
# Constraints
# 1. Cohort Unique Membership Constraint
# Sum (over 1..C) X_px (for 1..P) = 1
  add_constraint(sum_expr(X[p, c], c = 1:total_cohorts) == 1, p = 1:n_people) %>% 
  
# 2. Cohort Minimum Size Constraint
# Sum (over 1..P) X_pc (for 1..C) >= min_cohort_size
  add_constraint(sum_expr(X[p, c], p = 1:n_people) >= min_cohort_size, c = 1:total_cohorts) %>% 
  
# 3. Cohort Maximum Size Constraint
# Sum (over 1..P) X_pc (for 1..C) <= max_cohort_size
  add_constraint(sum_expr(X[p, c], p = 1:n_people) <= max_cohort_size, c = 1:total_cohorts)
MIP1_model  
# 4. having a min number of teammates   
#

#Solve model
MIP1_result <- solve_model(MIP1_model, with_ROI(solver = "glpk", verbose = TRUE))
MIP1_result[solution]


#______________________________________________________________________

#MIP2
sample_g <- erdos.renyi.game(35, .04 )
dev.off()
ggnet2(sample_g, label = TRUE)

# As an optimization problem 
#
# Objective Function
# This can be changed based on what we decide we actually would like to minimize
# The constraint is currently written as follows:
# Minimize the amount of adjacent neighbors for each individual that is not a 
# member of its cohort (sum over all individuals in the network)
# weight the score of the neighbors by their degree
# 
# another objective idea: maximize teams coming in on same days


# Parameters: (THESE NEED TO BE CHANGED TO THE OFFICE SIM AND ARE BASED OFF OF MADE UP NETWORK)
# MY MADE UP NETWORK HAS 35 PEOPLE
n_people <- 35
# Number of cohorts (can adjust this; 5 so that each cohort can work in office once a week)
total_cohorts <- 5
# Minimum amount of people per cohort (to ensure some level of social interaction)
min_cohort_size <- 5
# Maximum amount of people per cohort (to ensure distancing and capacity constraints)
max_cohort_size <- 8
# Matrix of nodes and neighbors (1 if nodes are adjacent; 0 if nodes are not adjacent)
neighbor_mat <- as_adjacency_matrix(sample_g)
as_adjacency_matrix(sample_g)
print(neighbor_mat[31,30])

# Cost matrix of the adjacency for minimization prob (0 if nodes are adjacent; 1 if nodes are not adjacent))
cost_mat <- matrix(0, ncol = n_people, nrow = n_people)
for (i in 1:nrow(cost_mat)) { 
  for (j in 1:ncol(cost_mat)){
    adj <- neighbor_mat[i,j]
    if (adj == 0) {
      cost_mat[i,j] <- 1
    }
  }
}
# Decision Variables
# X_pc Binary [0,1] variable for assignment of Person p to Cohort c
MIP2_model <- MIPModel() %>%
  
  # 1 iff person p and teammate t get assigned to cohort c
  add_variable(X[p, t, c], p = 1:n_people, t = 1:n_people, c = 1:total_cohorts, type = "binary") %>%
  
  # 1 iff person p is in cohort c 
  add_variable(Y[p, c], p = 1:n_people, c = 1:total_cohorts, type = "binary") %>%
  
  # Objective Function: minimize extra edges from cohort creation or maximize adjacent neighbors; t are teammates
  set_objective(sum_expr(cost_mat[p, t]*X[p, t, c], p = 1:n_people, t = 1:n_people, c = 1:total_cohorts), "min") %>%
  
  # Constraints
  #5. Zero out the diagonal and the upper triangular matrix of decision variable person teammate matrix
  #add_constraint(sum_expr(X[p, t, c], t = p:n_people) == 0, p = 1:n_people, c = 1:total_cohorts) %>% 
  add_constraint(X[p, p, c] == 0, c = 1:total_cohorts, p = 1:n_people) %>% 
  


  # 1. Cohort Unique Membership Constraint- CHECK THIS FIRST ONE
  # Sum (over 1..C) X_pc (for 1..P) = 1
  #add_constraint(sum_expr(X[p, t, c]+X[t, p, c],  p = 1:n_people, t = 1:n_people) >= 1,  c = 1:total_cohorts) %>%
  add_constraint(sum_expr(Y[p, c], c = 1:total_cohorts) == 1, p = 1:n_people) %>% 

  # Sum (over 1..C) Y_pc (for 1..P) = 1
  #add_constraint(X[p,t,c] == Y[p, c], p = 1:n_people, t = 1:n_people, c = 1:total_cohorts) %>% 
  
  #X => Y
  #fix diagonal zeroes issue
  add_constraint(X[p,t,c]*2 <= Y[t, c] + Y[p, c], p = 1:n_people, t = 1:n_people, c = 1:total_cohorts) %>% 
  #works add_constraint(sum_expr(X[p, t, c], c = 1:total_cohorts, t = 1:n_people, p = 1:n_people) 
  #               == sum_expr(Y[t, c], c = 1:total_cohorts, t = 1:n_people))  %>%
  
  # I think diagonal of zeroes is making this infeasible
  # wrap x[p != t.......]if(X[T] != X[P]){other condition}
  
  #add_constraint(sum_expr(X[p,t,c] - Y[t, c] - Y[p, c] + 1 + floor(p/t)*2, t = p:n_people) >= 0, p = 1:n_people, c = 1:total_cohorts) %>% 
  add_constraint(X[p,t,c] - Y[t, c] - Y[p, c] + 1 + 2*abs(ceiling(abs(p-t)/(n_people-1))-1)  >= 0, t = 1:n_people, p = 1:n_people, c = 1:total_cohorts) %>% 
  #add_constraint(X[p,t,c] >= Y[t, c] + Y[p, c] - 1 , p = 1:n_people, t = 1:n_people, c = 1:total_cohorts) %>% 
      #add_constraint(X[p,t,c] == Y[p, c], p = 1:n_people, t = 1:n_people, c = 1:total_cohorts) #%>%
  #add_constraint(sum_expr(X[p,t,c], p = 1:n_people) == Y[t, c], t = 1:n_people,  c = 1:total_cohorts) %>%
  #add_constraint(sum_expr(X[p,t,c], t = 1:n_people) == Y[p, c], p = 1:n_people,  c = 1:total_cohorts) %>%
  #add_constraint(sum_expr(Y[p, c], c = 1:total_cohorts) == 1, p = 1:n_people) #%>% 


  # 2. Cohort Minimum Size Constraint
  # Sum (over 1..P) X_pc (for 1..C) >= min_cohort_size
  add_constraint(sum_expr(Y[p, c], p = 1:n_people) >= min_cohort_size, c = 1:total_cohorts) %>% 
  
  # 3. Cohort Maximum Size Constraint
  # Sum (over 1..P) X_pc (for 1..C) <= max_cohort_size
  add_constraint(sum_expr(Y[p, c], p = 1:n_people) <= max_cohort_size, c = 1:total_cohorts) 


MIP2_model  

# 5. having a min number of teammates   
#

#Solve model
#MIP2_result <- solve_model(MIP2_model, with_ROI(solver = "glpk", verbose = TRUE))
MIP2_result <- solve_model(MIP2_model, with_ROI(solver = "symphony", verbosity=-1, gap_limit=1.5))

suppressPackageStartupMessages(library(dplyr))
matching <- MIP2_result %>% 
  get_solution(Y[p,c]) %>%
  filter(value > .9) %>%  
  select(p, c)


suppressPackageStartupMessages(library(dplyr))
matching <- MIP2_result %>% 
  get_solution(X[p,t,c]) %>%
  filter(value > .9) %>%  
  select(p, t, c)



