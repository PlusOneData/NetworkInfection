# Cohort Module: Clustering
# Goals: Generate cohort Groupings for schools or offices based on minimizing 
# additional connections in the network and keeping the most influential nodes
# of the graph (with high betweenness and degree centralities) contained within
# their circles
# ---- basic components
# implement clustering algorithm
# return the groupings of nodes that make up the advised cohorts
# assign a cohort attribute to each node based on grouping

# import libraries
library(igraph)
library(GGally)
library(ggplot2)


# random graph to demonstrate network of 40 people with probability of .1 of sharing edge
total_nodes = 40
sample_g <- erdos.renyi.game(total_nodes, .1, edgemode = "undirected" )
dev.off()
ggnet2(sample_g, label = TRUE)
neighbor_mat <- as_adjacency_matrix(sample_g)

# create cost matrix
cost_mat_fun <- function(x) {
  return(abs(x-1))
} 
return_mat_fun <- function(x) {
  return(x)
}
cost_mat <- apply(neighbor_mat, 2, cost_mat_fun)
neighbor_mat_View <- apply(neighbor_mat, 2, return_mat_fun)

# define number of cohorts
total_cohorts <- 3
min_cohort_size <- 10
max_cohort_size <- 20

# arbitrarily split nodes into feasible cohorts
# divide total nodes by number of cohorts and round up; use this to segment
segment_size <- ceiling(total_nodes/total_cohorts)
cohort_assignments <- data.frame(cohort = c(1:total_cohorts))
segment <- function(x){
  start <- 1+1*(segment_size*(x-1))
  end <- segment_size*x
  if(x == total_cohorts){
    if(segment_size*x > total_nodes){
      end <- total_nodes
    }
  }
  return(list(start:end))
}
cohort_assignments$members <- lapply(cohort_assignments$cohort, segment)

###################
# BASIC FUNCTIONS #
###################

# some basic functions that we will call on throughout

# get members in specific cohort; input is the cohort number
get_members <- function(cohort_number){
  return(unlist(cohort_assignments$members[[cohort_number]]))
}

# plot colored initial network graph based on cohort assignments
plot_network_graph <-function(network_mat, current_cohort_assignments){
  # not urgent DO THIS LATER
  ggnet2(graphname, label = TRUE)
}

get_connected_graph_edgelist <- function(vertices_list){
  # number of unique edge pairs in complete graph will be nCr(number vertices, 2)
  n_combo <- choose(length(vertices_list), 2)
  edgelist <- as.list(rep(0, n_combo))
  count <- 1
  for(i in vertices_list){
    for(j in vertices_list){
      if(j > i){
        pair <- c(i, j)
        print(pair)
        edgelist[[count]] <- pair
        count <- count + 1
      }
    }
  }
  return(edgelist)
}

# get complete graph of a cohort; input is the cohort number
# don't use this, it is inefficient- use get_all_cohorts_graph
get_cohort_graph <-function(cohort_number){
  # vertices and edges
  vertices <- get_members(cohort_number)
  vertices <- get_members(1)
  edgelist <- get_connected_graph_edgelist(vertices)
  
  
}

# get complete graph of all cohorts; input is the current cohort assignments
get_all_cohorts_graph <-function(current_cohort_assignments){
  # generate adjacency matrix
  
}

# gets adjacency matrix of all cohorts; input is the current cohort assignments
get_all_cohorts_adj_mat <-function(current_cohort_assignments){
  # generate adjacency matrix
  adj_mat <- matrix(0, total_nodes, total_nodes)
  # for each cohort, give value of 1 in adjacency matrix to each node pair in the cohort; the result is a complete graph for each cohort
  for(i in 1:nrow(current_cohort_assignments)){
    members <- current_cohort_assignments$members[[i]]
    for(j in members){
      for(k in members){
        adj_mat[j,k] <- 1
      }
    }
  }
  diag(adj_mat) <- 0
  # plot graph
  cohorts_graph <- graph_from_adjacency_matrix(adj_mat, mode="undirected")
  Layout <- layout.circle(cohorts_graph)
  plot.igraph(cohorts_graph, 
              vertex.label = V(cohorts_graph)$name, vertex.label.color = "gray20",
              vertex.size = 10,
              vertex.color = "gray90", vertex.frame.color = "gray20",
              edge.curved = T, 
              layout = Layout)
  return(adj_mat)
}


# score all cohorts; this is the current objective value; input is adjacency matrix
get_objective_Val <- function(adjacency_mat){
  # multiply cost matrix by adjacency matrix and divide by 2 to avoid double counting cost of (i,j) and (j,i)
  score_mat <- matrix(0, total_nodes, total_nodes)
  for(i in 1:total_nodes){
    for(j in 1:total_nodes){
      score_mat[i,j] <- adj_mat[i,j]*cost_mat[i,j]
    }
  }
  obj_val <- sum(score_mat)/2
  return(obj_val)
} 


########################
# OPTIMIZING FUNCTIONS #
########################

# some optimizing functions that will help us improve the objective value

# execute transfers that will improve the objective value
opt_transfer <- function(current_cohort_assignments){
  # get current objective value
  #___CODE
  
  # identify candidate cohorts that can hand off or receive a member
  candidate_handoff <- c()
  candidate_receive <- c()
  for(i in 1:nrow(cohort_assignments)){
    if(length(unlist(cohort_assignments$members[[i]])) > min_cohort_size){
      candidate_handoff <- append(candidate_handoff, cohort_assignments$cohort[[i]])
    }
  }
  for(i in 1:nrow(cohort_assignments)){
    if(length(unlist(cohort_assignments$members[[i]])) < max_cohort_size){
      candidate_receive <- append(candidate_receive, cohort_assignments$cohort[[i]])
    }
  }
  # if there are no elements in handoff, or if there are no elements in receive, or if the only elements are the same cohort; break out of this 
  # and mark as exhausted
  #____code________
  
  # now we just need one move to find a better objective value, and exit and return once we do
  # start with first element, which we will call the explorer, in first candidate handoff cohort; remove and add to first available receiver cohort
  current_explorer <- unlist(cohort_assignments$members[candidate_handoff[1]])[1]
  check <- FALSE
  while(check == FALSE){
    the_num <- 1
    if(candidate_handoff[1] == candidate_receive[the_num]){
      the_num <- the_num + 1
    }
    else{
      check <- TRUE
    }
  }
  current_receiver <- unlist(cohort_assignments$members[candidate_receive[the_num]])[1]
  # make exploratory cohort assignments dataframe
  
  # score this cohort assignment
  
  # if it is better than the objective, assign this to cohort_assignments, and leave this function
  
  # if it is not better, keep exploring next nodes in the cohort followed by the next cohorts that can handoff a member
  
  # if we don't find a better objective value, transfer is exhausted for this iteration and we return nothing

  # remove node from current cohort, place in the first candidate cohort with 
}


  
  
  
#######################
# ALGORITHM EXECUTION #
#######################
  
# in this part of the code, we execute an algorithm that takes on the heuristic approach to the optimization problem

while(cohorts_complete == FALSE){

# transfer function
  
# swap function
  
# return current cohorts
  

  
  
}
  
  
#test
get_all_cohorts_adj_mat(cohort_assignments)

adj <- get_all_cohorts_adj_mat(cohort_assignments)
adj_mat
get_objective_Val(adj)  


