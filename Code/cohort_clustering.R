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
library(qgraph)
library(purrr)


# random graph to demonstrate network of 40 people with probability of .1 of sharing edge

total_nodes = 40
set.seed(23)
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
# COHORT_ASSIGNMENTS IS A GLOBAL VARIABLE
cohort_assignments <<- data.frame(cohort = c(1:total_cohorts))
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
plot_network_graph <-function(network_mat, x_cohort_assignments){
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
get_all_cohorts_graph <-function(x_cohort_assignments){
  # generate adjacency matrix
  
}

# gets adjacency matrix of all cohorts; input is the current cohort assignments
get_all_cohorts_adj_mat <-function(x_cohort_assignments){
  # generate adjacency matrix
  adj_mat <- matrix(0, total_nodes, total_nodes)
  # for each cohort, give value of 1 in adjacency matrix to each node pair in the cohort; the result is a complete graph for each cohort
  for(i in 1:nrow(x_cohort_assignments)){
    members <- x_cohort_assignments$members[[i]]
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
  # nicer graph
  e <- get.edgelist(cohorts_graph)
  l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(cohorts_graph),
                                         area=8*(vcount(cohorts_graph)^2),repulse.rad=(vcount(cohorts_graph)^3.1))
  plot(cohorts_graph,layout= l, vertex.label=V(cohorts_graph)$name, vertex.label.color = "gray20",
       vertex.size = 10, vertex.label.cex = .75,
       vertex.color = "gray90", vertex.frame.color = "gray20")
  return(adj_mat)
}


# score all cohorts; this is the current objective value; input is adjacency matrix
get_objective_Val <- function(adjacency_mat){
  # multiply cost matrix by adjacency matrix and divide by 2 to avoid double counting cost of (i,j) and (j,i)
  score_mat <- matrix(0, total_nodes, total_nodes)
  for(i in 1:total_nodes){
    for(j in 1:total_nodes){
      score_mat[i,j] <- adjacency_mat[i,j]*cost_mat[i,j]
    }
  }
  obj_val <- sum(score_mat)/2
  return(obj_val)
} 

# generate test cohort assignments; input is current cohort assignments, current explorer, current handoff, and current receive
generate_test_assignments <- function(y_cohort_assignments, y_explorer, y_handoff, y_receive){
  y_exp_ind <- match(y_explorer, unlist(y_cohort_assignments$members[candidate_handoff[y_handoff]]))
  print(paste0(y_explorer, y_handoff, y_receive, y_exp_ind))
  test_cohort_assignments <- data.frame(y_cohort_assignments)
  handoff_number <- candidate_handoff[y_handoff]
  receive_number <- candidate_receive[y_receive]
  test_handoff <- unlist(test_cohort_assignments$members[handoff_number])[- y_exp_ind]
  print(paste0("test handoff, ", test_handoff, "candidate handoff: ",candidate_handoff[y_handoff] ))
  test_cohort_assignments$members[[handoff_number]] <- as.list(test_handoff)
  
  test_receive <- unlist(test_cohort_assignments$members[receive_number])
  test_cohort_assignments$members[[receive_number]] <- as.list(c(test_receive, y_explorer))
  #print(paste0("test receive, ", test_cohort_assignments$members[[y_explorer]]))
  return(test_cohort_assignments)
}

# compare objective values of test and current configurations and make decisions on next handoff and receive elements
#' Title
#'
#' @param x_cohort_assignments 
#' @param x_test_configuration 
#' @param x_explorer 
#' @param x_handoff 
#' @param x_receive 
#'
#' @return
#' @export
#'
#' @examples
compare_test_and_next <- function(x_cohort_assignments, x_test_configuration, x_explorer, x_handoff, x_receive){
  first_receive <- x_receive
  receive_updated <- FALSE
  # see if test configuration is better than current objective value
  test_obj_val <<- get_objective_Val(get_all_cohorts_adj_mat(x_test_configuration))
  current_obj_val <<- get_objective_Val(get_all_cohorts_adj_mat(x_cohort_assignments))
  if(test_obj_val < current_obj_val){
    # if test objective is better, set current global variable to this configuration and break from function (set to unexhausted)
    cohort_assignments <<- x_test_configuration[,]
    #_________more code to show that we improved objective___________
    #break
    improvement_to_objective <<- current_obj_val - test_obj_val
    print(paste0("IMPROVED OBJ, explorer: ", x_explorer, "handoff: ", x_handoff, "receive: ",x_receive))
    return(improvement_to_objective)
  }
  else{
    # the test objective was not better; first try next receive group and then change current explorer
    # since we always start receive group index to the smallest possible, try to increase while making sure that this index is not
    # the same index as our handoff group index, while also making sure that the group number exists
    
    if(length(candidate_receive) > x_receive){
      print("here1")  
      x_receive <- x_receive + 1
      print(x_receive)
      receive_updated <- TRUE
      #_____ great, now go start a new iteration of compare_test_and_next with updated input
      new_test_configuration <- generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
      print(paste0("Uexplorer: ", x_explorer, "handoff: ", x_handoff, "receive: ",x_receive))
      compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)      
    }
    if(x_handoff == x_receive){
      # reset indicator because we are not positive that the previous update was valid
      receive_updated <- FALSE
      # need to verify that the there is a next available receive cohort in the receive list
      if(length(candidate_receive) > x_receive){
        x_receive <- x_receive + 1
        receive_updated <- TRUE
        #_____ great, now go start a new iteration of compare_test_and_next with updated input
        new_test_configuration <- generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
        print(paste0("Pexplorer: ", x_explorer, "handoff: ", x_handoff, "receive: ",x_receive))
        compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
      }
    }
    
    # Changing receive group didn't work, we need to change current explorer
    
    if(receive_updated == FALSE){
      x_receive <- first_receive
      print("explorer switchup in progress")
      # we don't have anymore potential receivers for this current explorer and need to set next current explorer
      #exp_ind <- which(unlist(x_cohort_assignments$members[x_handoff]) == x_explorer)[[1]]
      exp_ind <- match(x_explorer, unlist(x_cohort_assignments$members[candidate_handoff[x_handoff]]))
      print(paste0("explorer index :", exp_ind))
      if(is.na(exp_ind) == TRUE){
        print("problematic")
        break##########################
      }
      if(length(unlist(x_cohort_assignments$members[candidate_handoff[x_handoff]])) > exp_ind){
        exp_ind <- exp_ind + 1
        print(paste0("explorer index :", exp_ind))
        x_explorer <- unlist(x_cohort_assignments$members[candidate_handoff[x_handoff]])[exp_ind]
        print(paste0("explorer  :", x_explorer))
        #_____ great, now go start a new iteration of compare_test_and_next with updated input
        new_test_configuration <- generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
        print(paste0("Yexplorer: ", x_explorer, "handoff: ", x_handoff, "receive: ",x_receive))
        compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
      }
      
      # if there are no more explorers in the handoff cohort, remove this from handoff candidate list and go to the next handoff candidate
      else{
        candidate_handoff <<- candidate_handoff[-x_handoff]
        print(paste0("TTTTTTTTTTTT",unlist(candidate_handoff)))
        print(paste0("TTTTTTTTTTTTTT",unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]))
    
        # first check if there is another candidate handoff cohort and unique receive cohort
        if(length(candidate_handoff) >= 1){
          x_handoff <- 1
          x_receive <- 1
        }
        else{
          print(paste0("exhausted!!!"))
          T_Exhausted <<- TRUE
          return(paste0("T_Exhausted = ", T_Exhausted))
          #____BREAK from transfer function________
        }
        
        if(candidate_handoff[1] == candidate_receive[x_receive]){
          # need to verify that the there is a next available receive cohort in the receive list
          if(length(candidate_receive) > x_receive){
            x_receive <- x_receive + 1
            #_____ great, now go start a new iteration of compare_test_and_next with updated input
            x_explorer <- unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]
            new_test_configuration <- generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
            print(paste0("Zexplorer: ", x_explorer, "handoff: ", x_handoff, "receive: ",x_receive))
            compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
          }
          else{
            T_Exhausted <<- TRUE
            return(paste0("T_Exhausted = ", T_Exhausted))
            #____BREAK from transfer function________
          }
        }
        # check again
        else{
          #_____ great, now go start a new iteration of compare_test_and_next with updated input
          x_explorer <- unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]
          new_test_configuration <- generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
          print(paste0("Xexplorer: ", x_explorer, "handoff: ", x_handoff, "receive: ",x_receive))
          compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
        }
      }
    }
  }
}

########################
# OPTIMIZING FUNCTIONS #
########################

# some optimizing functions that will help us improve the objective value

# execute transfers that will improve the objective value
opt_transfer <- function(x_cohort_assignments){
  # get current objective value
  #___CODE
  
  # identify candidate cohorts that can hand off or receive a member
  candidate_handoff <<- c()
  candidate_receive <<- c()
  for(i in 1:nrow(x_cohort_assignments)){
    if(length(unlist(x_cohort_assignments$members[[i]])) > min_cohort_size){
      candidate_handoff <<- append(candidate_handoff, x_cohort_assignments$cohort[[i]])
    }
  }
  for(i in 1:nrow(x_cohort_assignments)){
    if(length(unlist(x_cohort_assignments$members[[i]])) < max_cohort_size){
      candidate_receive <<- append(candidate_receive, x_cohort_assignments$cohort[[i]])
    }
  }
  # if there are no elements in handoff, or if there are no elements in receive, or if the only elements are the same cohort; break out of this 
  # and mark as exhausted
  #____BREAK code________
  
  # OPTIMIZATION: search for better objective
  # Initial assignments for first current explorer (from a handoff cohort) and the first receive group index
  # start with first element, which we will call the explorer, in first candidate handoff cohort; remove and add to first available receiver cohort
  current_explorer <<- unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]
  han_ind <<- 1
  check <<- FALSE
  rec_ind <<- 0
  # need to verify that the receive and handoff cohorts are different
  while(check == FALSE){
    rec_ind <<- 1
    if(candidate_handoff[1] == candidate_receive[rec_ind]){
      # need to verify that the there is a next available receive cohort in the receive list
      if(length(candidate_receive)>rec_ind){
        rec_ind <<- rec_ind + 1
        break
      }
      else{
        T_Exhausted <<- TRUE
        break
      }
    }
    else{
      check <<- TRUE
      break
    }
  }
}  
  
#######################
# ALGORITHM EXECUTION #
#######################

# in this part of the code, we execute an algorithm that takes on the heuristic approach to the optimization problem
# make test cohort assignments configuration; feed through transfer function
#T_Exhausted <- FALSE

#initialize transfer opt fx
T_Exhausted <<- FALSE

while(T_Exhausted == FALSE){
  t2 <- opt_transfer(cohort_assignments)
  test_configuration <- generate_test_assignments(cohort_assignments, current_explorer, han_ind, rec_ind)
  compare_test_and_next(cohort_assignments, test_configuration, current_explorer, han_ind, rec_ind)  

}

#check 218


#this is not necessary
#current_receiver <- unlist(x_cohort_assignments$members[candidate_receive[rec_ind]])[1]




  
  
  


  
  
#test
get_all_cohorts_adj_mat(cohort_assignments)

adj <- get_all_cohorts_adj_mat(cohort_assignments)
adj_mat
get_objective_Val(adj)  

test_configuration1 <- generate_test_assignments(cohort_assignments, current_explorer, 1, 2)
adj1 <- get_all_cohorts_adj_mat(test_configuration1)
test1 <- compare_test_and_next(cohort_assignments, test_configuration1, current_explorer, 1, 2)
 
get_objective_Val(get_all_cohorts_adj_mat(cohort_assignments))
#218

T_Exhausted <- FALSE
opt_transfer(cohort_assignments)
