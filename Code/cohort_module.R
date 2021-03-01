# Cohort Module
# Goals: Generate cohort Groupings for schools or offices based on minimizing 
# additional connections in the network and keeping the most influential nodes
# of the graph (with high betweenness and degree centralities) contained within
# their circles
# ---- basic components
# return the groupings of nodes that make up the advised cohorts

# constraints
# individuals 

# alg
# 1. sort nodes from highest to lowest degree
# 2. select first node from list to start cohort, and while we have less than X
#    people in cohort, add adjacent nodes of highest degree (betweenness
#    as tiebreaker) to cohort
#    If we run out of adjacent nodes, go to the adjacent node with highest degree 
#    and designate its neighbors of highest degree to join cohort
# 3. when we reach X people in the cohort, stop adding adjacent nodes
# 4. Remove nodes already assigned to cohorts from list of unassigned nodes
# 5. Repeat sorting of unassigned nodes from highest to lowest degree
# 6. Repeat creation of cohort
# 7. If we reach a point where unassigned nodes do not have neighbors, create
#    a cohort of these nodes (or multiple cohorts of X people)

# questions: how to deal with group of 1 (if X is 5 and we have 21 people)
#
#
#
#
# Sample graph for use in checking and exploring optimization
install.packages("GGally")
install.packages('sna')
library(ggplot2)
library(igraph)
library(GGally)
library(sna)








sample_g <- erdos.renyi.game(35, .1 )
dev.off()
ggnet2(sample_g, label = TRUE)
#
# As an optimization problem 
#
# Objective Function
# This can be changed based on what we decide we actually would like to minimize
# The constraint is currently written as follows:
# Minimize the amount of adjacent neighbors for each individual that is not a 
# member of its cohort (sum over all individuals in the network)
# 
# Decision Variables
# X_PC Binary [0,1] variable taking value 1 if Person P is in Cohort C and taking
# value 0 if Person P is not in Cohort C
# 
# Parameters: (THESE NEED TO BE CHANGED TO THE OFFICE SIM AND ARE BASED OFF OF MADE UP NETWORK)
# MY MADE UP NETWORK HAS 35 PEOPLE
# Number of cohorts (can adjust this; 5 so that each cohort can work in office once a week)
total_cohorts -> 5
# Maximum amount of people per cohort (to ensure distancing and capacity constraints)
min_cohort_size -> 5
# Minimum amount of people per cohort (to ensure some level of social interaction)
max_cohort_size -> 10
# Matrix of nodes and neighbors (1 if nodes are adjacent; 0 if nodes are not adjacent)
neighbort_mat -> #need to create
#
# Constraints
# 1. Cohort Unique Membership Constraint
# Sum (over 1..C) X_PC (for 1..P) = 1
#
# 2. Cohort Minimum Size Constraint
# Sum (over 1..P) X_PC (for 1..C) >= min_cohort_size
#
# 3. Cohort Maximum Size Constraint
# Sum (over 1..P) X_PC (for 1..C) >= min_cohort_size
#
# 
#


