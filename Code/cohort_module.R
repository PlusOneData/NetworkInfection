# Cohort Module
# Goals: Generate cohort Groupings for schools or offices based on minimizing 
# additional connections in the network and keeping the most influential nodes
# of the graph (with high betweenness and degree centralities) contained within
# their circles
# ---- basic components
# return the groupings of nodes that make up the advised cohorts
# cohorts should be within the min and max size allowed

#' Cohort Assignment Class
#'
#' A default class to implement cohorting policies.
#' A graph of n nodes with a \code{$cohort} property have based on network position. 
#' When \code{$init} is run  
#'
#' @field min_cohort_size . 
#' @field max_cohort_size . 
#' @field total_cohorts . 
#'
#' @export default_cohort
default_cohort <- setRefClass(
  "cohort",
  fields = list(
    min_cohort_size="numeric",
    max_cohort_size="numeric",
    total_cohorts ="numeric"
  ),
  methods = list(
    #' Create cost matrix
    #'
    #' @param x adjacency matrix for contact network
    #'
    #' @return cost matrix of input contact network with cost of 1 for nodes that did not share an edge between them
    #' 
    cost_mat_fun = function(x) {
      return(abs(x-1))
    },
    
    
    #' Return a matrix
    #'
    #' @param x matrix to be returned (if there is a need to export for comparison to other methods)
    #'
    #' @return matrix
    return_mat_fun = function(x) {
      return(x)
    },
    
    #' Get members in a specific cohort
    #'
    #' @param cohort_number 
    #'
    #' @return elements of specified cohort
    #' 
    get_members = function(cohort_number){
      return(unlist(cohort_assignments$members[[cohort_number]]))
    },
    
    
    #' Get unique edge pairings of a complete graph given a set of vertices
    #'
    #' @param vertices_list list of vertices of prospective complete graph
    #'
    #' @return unique edge pairings for a complete graph
    #'
    #' @examples
    get_connected_graph_edgelist = function(vertices_list){
      # number of unique edge pairs in complete graph will be nCr(number vertices, 2)
      n_combo <- choose(length(vertices_list), 2)
      edgelist <- as.list(rep(0, n_combo))
      count <- 1
      for(i in vertices_list){
        for(j in vertices_list){
          if(j > i){
            pair <- c(i, j)
            #print(pair)
            edgelist[[count]] <- pair
            count <- count + 1
          }
        }
      }
      return(edgelist)
    },
    
    
    #' Get adjacency matrix representing all cohorts
    #' This function also plots graphs
    #' @param x_cohort_assignments dataframe of current cohort assignments
    #'
    #' @return adjacency matrix representing all cohorts; also plots 2 graphs of current cohort assignments
    get_all_cohorts_adj_mat_with_graph = function(x_cohort_assignments){
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
      
      df_results <- data.frame(
        member = 1:total_nodes,
        cohort = NA
      )
      
      for(i in 1:total_cohorts){
        for(j in 1:length(unlist(x_cohort_assignments$members[[i]]))){
          df_results$cohort[unlist(x_cohort_assignments$members[[i]])[j]] <- i
          cohorts_graph$cohort[unlist(x_cohort_assignments$members[[i]])[j]] <- i
        }
      }
      
      Layout <- layout.circle(cohorts_graph)
      plot.igraph(cohorts_graph, 
                  vertex.label = V(cohorts_graph)$name, vertex.label.color = "gray20",
                  vertex.size = 10,
                  vertex.color = "gray90", vertex.frame.color = "gray20",
                  edge.curved = T, 
                  layout = Layout)
      
      
      # nicer graph  
      p <- RColorBrewer::brewer.pal(8, "Set2")[ c(3, 4, 5, 6, 1, 2) ]
      x <- cohorts_graph$cohort
      names(p) <- levels(x)
      
      e <- get.edgelist(cohorts_graph)
      l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(cohorts_graph),
                                             area=8*(vcount(cohorts_graph)^2),repulse.rad=(vcount(cohorts_graph)^3.1))
      plot(cohorts_graph,layout= l, vertex.label=V(cohorts_graph)$name, vertex.label.color = "gray20",
           vertex.size = 10, vertex.label.cex = .75,
           vertex.color = x, palette = c("1" = p[1], "2" = p[2], "3" = p[3]), color.legend = "Cohort", vertex.frame.color = "gray20")
      return(adj_mat)
    },
    
    
    #' Get adjacency matrix representing all cohorts
    #' 
    #' @param x_cohort_assignments dataframe of current cohort assignments
    #'
    #' @return adjacency matrix representing all cohorts
    get_all_cohorts_adj_mat = function(x_cohort_assignments){
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
      
      return(adj_mat)
    },
    
    
    #' Objective Value scoring function
    #'
    #' @param adjacency_mat adjacency matrix of the current cohort assignments
    #'
    #' @return integer objective value based on the cost function and current cohort assignment adjacency matrix
    #'
    get_objective_Val = function(adjacency_mat){
      # multiply cost matrix by adjacency matrix and divide by 2 to avoid double counting cost of (i,j) and (j,i)
      score_mat <- matrix(0, total_nodes, total_nodes)
      for(i in 1:total_nodes){
        for(j in 1:total_nodes){
          score_mat[i,j] <- adjacency_mat[i,j]*cost_mat[i,j]
        }
      }
      obj_val <- sum(score_mat)/2
      return(obj_val)
    },
    
    #' Transfer Function: Test Cohort Assignment Generation
    #'
    #' @param y_cohort_assignments dataframe of current cohort assignments 
    #' @param y_explorer vertex to be transferred to another cohort
    #' @param y_handoff cohort number that the vertex to be transferred is leaving from
    #' @param y_receive cohort number that the vertex to be transferred is going to
    #'
    #' @return dataframe of test cohort assignments 
    #' 
    T_generate_test_assignments = function(y_cohort_assignments, y_explorer, y_handoff, y_receive){
      y_exp_ind <- match(y_explorer, unlist(y_cohort_assignments$members[candidate_handoff[y_handoff]]))
      test_cohort_assignments <- data.frame(y_cohort_assignments)
      handoff_number <- candidate_handoff[y_handoff]
      receive_number <- candidate_receive[y_receive]
      test_handoff <- unlist(test_cohort_assignments$members[handoff_number])[- y_exp_ind]
      test_cohort_assignments$members[[handoff_number]] <- as.list(test_handoff)
      
      test_receive <- unlist(test_cohort_assignments$members[receive_number])
      test_cohort_assignments$members[[receive_number]] <- as.list(c(test_receive, y_explorer))
      return(test_cohort_assignments)
    },
    
    #' Transfer Function: helper function to compare objective values of test and current configurations and make decisions on next handoff and receive elements
    #'
    #' @param x_cohort_assignments dataframe of current cohort assignments
    #' @param x_test_configuration dataframe of test cohort assignments
    #' @param x_explorer vertex to be transferred to another cohort
    #' @param x_handoff cohort number that the vertex to be transferred is leaving from
    #' @param x_receive cohort number that the vertex to be transferred is going to
    #'
    #' @return potentially return improvement to objective (if there is one); also potentially updates the global variable for current cohort assignments
    #` 
    T_compare_test_and_next = function(x_cohort_assignments, x_test_configuration, x_explorer, x_handoff, x_receive){
      first_receive <- x_receive
      receive_updated <- FALSE
      # see if test configuration is better than current objective value
      test_obj_val <<- get_objective_Val(get_all_cohorts_adj_mat(x_test_configuration))
      current_obj_val <<- get_objective_Val(get_all_cohorts_adj_mat(x_cohort_assignments))
      if(test_obj_val < current_obj_val){
        # if test objective is better, set current global variable to this configuration and break from function (set to unexhausted)
        cohort_assignments <<- x_test_configuration[,]
        #_________more code to show that we improved objective___________
        # break by returning improvement to objective
        improvement_to_objective <<- current_obj_val - test_obj_val
        current_obj_val <<- test_obj_val
        T_improved_optimality <<- TRUE
        return(improvement_to_objective)
      }
      else{
        # the test objective was not better; first try next receive group and then change current explorer
        # since we always start receive group index to the smallest possible, try to increase while making sure that this index is not
        # the same index as our handoff group index, while also making sure that the group number exists
        
        if(length(candidate_receive) > x_receive){
          x_receive <- x_receive + 1
          receive_updated <- TRUE
          #_____ great, now go start a new iteration of T_compare_test_and_next with updated input
          new_test_configuration <- T_generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
          T_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)      
        }
        if(x_handoff == x_receive){
          # reset indicator because we are not positive that the previous update was valid
          receive_updated <- FALSE
          # need to verify that the there is a next available receive cohort in the receive list
          if(length(candidate_receive) > x_receive){
            x_receive <- x_receive + 1
            receive_updated <- TRUE
            #_____ great, now go start a new iteration of T_compare_test_and_next with updated input
            new_test_configuration <- T_generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
            T_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
          }
        }
        
        # Changing receive group didn't work, we need to change current explorer
        
        if(receive_updated == FALSE){
          x_receive <- first_receive
          # we don't have anymore potential receivers for this current explorer and need to set next current explorer
          exp_ind <- match(x_explorer, unlist(x_cohort_assignments$members[candidate_handoff[x_handoff]]))
          # This should not happen, but just in case we can catch the error and break from the function
          if(is.na(exp_ind) == TRUE){
            print("problematic")
            break
          }
          if(length(unlist(x_cohort_assignments$members[candidate_handoff[x_handoff]])) > exp_ind){
            exp_ind <- exp_ind + 1
            x_explorer <- unlist(x_cohort_assignments$members[candidate_handoff[x_handoff]])[exp_ind]
            #_____ great, now go start a new iteration of T_compare_test_and_next with updated input
            new_test_configuration <- T_generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
            T_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
          }
          
          # if there are no more explorers in the handoff cohort, remove this from handoff candidate list and go to the next handoff candidate
          else{
            candidate_handoff <<- candidate_handoff[-x_handoff]
            
            # first check if there is another candidate handoff cohort and unique receive cohort
            if(length(candidate_handoff) >= 1){
              x_handoff <- 1
              x_receive <- 1
            }
            else{
              T_Exhausted <<- TRUE
              return(paste0("T_Exhausted = ", T_Exhausted))
              #____BREAK from transfer function________
            }
            
            if(candidate_handoff[1] == candidate_receive[x_receive]){
              # need to verify that the there is a next available receive cohort in the receive list
              if(length(candidate_receive) > x_receive){
                x_receive <- x_receive + 1
                #_____ great, now go start a new iteration of T_compare_test_and_next with updated input
                x_explorer <- unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]
                new_test_configuration <- T_generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
                T_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
              }
              else{
                T_Exhausted <<- TRUE
                return(paste0("T_Exhausted = ", T_Exhausted))
                #____BREAK from transfer function________
              }
            }
            # check again
            else{
              #_____ great, now go start a new iteration of T_compare_test_and_next with updated input
              x_explorer <- unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]
              new_test_configuration <- T_generate_test_assignments(x_cohort_assignments, x_explorer, x_handoff, x_receive)
              T_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_explorer, x_handoff, x_receive)
            }
          }
        }
      }
    },
    
    
    #' Switch Function: Test Cohort Assignment Generation
    #'
    #' @param y_cohort_assignments dataframe of current cohort assignments
    #' @param y_swapper_a Vertex A (to be swapped with Vertex B)
    #' @param y_swapper_b Vertex B (to be swapped with Vertex A)
    #' @param y_orig_ind index (row number) in dataframe representing the initial cohort of Vertex A
    #' @param y_dest_ind index (row number) in dataframe representing the initial cohort of Vertex B
    #'
    #' @return dataframe of test cohort assignments 
    #'
    S_generate_test_assignments = function(y_cohort_assignments, y_swapper_a, y_swapper_b, y_orig_ind, y_dest_ind){
      
      # find index of the swappers a and b by matching these element to their indices in the origin swap list for their cohorts
      y_swap_a_ind <- match(y_swapper_a, unlist(dynamic_cohorts$members[candidate_swap_origin[y_orig_ind]]))
      y_swap_b_ind <- match(y_swapper_b, unlist(dynamic_cohorts$members[candidate_swap_origin[y_dest_ind]]))
      test_cohort_assignments <- data.frame(y_cohort_assignments)
      
      # actual value of the name of the cohort that has the specified index
      orig_number <- candidate_swap_origin[y_orig_ind]
      dest_number <- candidate_swap_origin[y_dest_ind]
      
      
      # perform swap for the test assignments
      # first remove swapper a and swapper b from their cohorts
      test_orig <- unlist(dynamic_cohorts$members[orig_number])[- c(y_swap_a_ind)]
      test_dest <- unlist(dynamic_cohorts$members[dest_number])[- c(y_swap_b_ind)]
      
      
      # next add swapper a and swapper b to their opposite cohorts
      # send swapper b to the origin cohort
      test_cohort_assignments$members[[orig_number]] <- as.list(c(test_orig, y_swapper_b))
      
      # send swapper a to the destination cohort
      test_cohort_assignments$members[[dest_number]] <- as.list(c(test_dest, y_swapper_a))
      
      
      return(test_cohort_assignments)
    },
    
    
    #' Switch Function: helper function to compare objective values of test and current configurations and make decisions on whether to switch two elements from different cohorts
    #'
    #' @param x_cohort_assignments  dataframe of current cohort assignments
    #' @param x_test_configuration 
    #' @param x_swapper_a Vertex A (to be swapped with Vertex B)
    #' @param x_swapper_b Vertex B (to be swapped with Vertex A)
    #' @param x_orig_ind index (row number) in dataframe representing the initial cohort of Vertex A
    #' @param x_dest_ind index (row number) in dataframe representing the initial cohort of Vertex B
    #'
    #'
    #' @return potentially return improvement to objective (if there is one); also potentially updates the global variable for current cohort assignments
    #'
    S_compare_test_and_next = function(x_cohort_assignments, x_test_configuration, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind){
      first_swapper <- x_swapper_a
      x_swap_a_ind <- match(x_swapper_a, unlist(x_cohort_assignments$members[candidate_swap_origin[x_orig_ind]]))
      x_swap_b_ind <- match(x_swapper_b, unlist(x_cohort_assignments$members[candidate_swap_origin[x_dest_ind]]))
      swapper_b_updated <- FALSE
      # see if test configuration is better than current objective value
      test_obj_val <<- get_objective_Val(get_all_cohorts_adj_mat(x_test_configuration))
      current_obj_val <<- get_objective_Val(get_all_cohorts_adj_mat(x_cohort_assignments))
      if(test_obj_val < current_obj_val){
        # if test objective is better, set current global variable to this configuration and break from function (already set to unexhausted)
        cohort_assignments <<- x_test_configuration[,]
        # update that we improved objective value
        improvement_to_objective <<- current_obj_val - test_obj_val
        current_obj_val <<- test_obj_val
        S_improved_optimality <<- TRUE
        return(improvement_to_objective)
      }
      else{
        # if there is another swapper b element in current cohort (so there are either more elements in the destination cohort, 
        # or swapper b is the last element in the destination cohort but there is another destination cohort)
        
        if(length(unlist(dynamic_cohorts$members[[x_dest_ind]])) > x_swap_b_ind){
          
          # set swapper b index to the next index, get value for swapper b
          x_swap_b_ind <- x_swap_b_ind + 1
          x_swapper_b <- unlist(dynamic_cohorts$members[candidate_swap_origin[x_dest_ind]])[x_swap_b_ind]
          swapper_b_updated <- TRUE
          # rerun S_compare_test_and_next with updated swapper_b
          new_test_configuration <- S_generate_test_assignments(x_cohort_assignments, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
          S_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
          
        }
        else{
          # if there is a next destination cohort in dynamic cohorts list for swaps to take place
          # go to next destination cohort and reset swapper b to first element of that new cohort
          if(x_dest_ind < length(dynamic_cohorts$members)){
            x_dest_ind <- x_dest_ind + 1
            x_swap_b_ind <- 1
            x_swapper_b <- unlist(dynamic_cohorts$members[candidate_swap_origin[x_dest_ind]])[x_swap_b_ind]
            swapper_b_updated <- TRUE
            # rerun S_compare_test_and_next with updated swapper_b
            new_test_configuration <- S_generate_test_assignments(x_cohort_assignments, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
            S_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
          }
        }
        
        
        # if this is the last element in the origin cohort, remove it from the candidate_swap_origin list, set swapper a to first element of the first dynamic
        # cohort element list and set swapper b to the first element of the next cohort in dynamic list
        # if we make it to the point that there is only one cohort in the dynamic cohorts list, we've exhausted the switch function, set S_Exhausted to true
        if(swapper_b_updated == FALSE){
          # check to see if we are on the last possible origin cohort to try swapper at
          if(x_swap_a_ind == length(unlist(dynamic_cohorts$members[candidate_swap_origin[x_orig_ind]]))){
            # check to see if we are at the last element of 2nd to last cohort in dynamic cohorts list- this means that the function is exhausted
            if((x_orig_ind + 1) == length(dynamic_cohorts$members)){
              S_Exhausted <<- TRUE
              return()
            }
            else
              # remove this cohort from the dynamic cohort df and candidate swap origin list and reset swapper a and swapper b
              #candidate_swap_origin <<- candidate_swap_origin[- x_orig_ind]
              #dynamic_cohorts <<- dynamic_cohorts[-c(x_orig_ind),]
              # reset index
              #rownames(dynamic_cohorts) <<- NULL
              x_orig_ind <- x_orig_ind + 1
            x_dest_ind <- x_orig_ind + 1
            x_swapper_a <- unlist(dynamic_cohorts$members[candidate_swap_origin[x_orig_ind]])[1]
            x_swapper_b <- unlist(dynamic_cohorts$members[candidate_swap_origin[x_dest_ind]])[1]
            # rerun S_compare_test_and_next with updated swapper_b
            new_test_configuration <- S_generate_test_assignments(x_cohort_assignments, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
            S_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
          }
          # there is another element in the current origin swapper cohort to move on to, update swapper a to this next element
          else{
            x_swap_a_ind <- x_swap_a_ind + 1
            x_swapper_a <- unlist(dynamic_cohorts$members[candidate_swap_origin[x_orig_ind]])[x_swap_a_ind]
            x_swap_b_ind <- 1
            x_swapper_b <- unlist(dynamic_cohorts$members[candidate_swap_origin[x_dest_ind]])[x_swap_b_ind]
            # rerun S_compare_test_and_next with updated swapper_b
            new_test_configuration <- S_generate_test_assignments(x_cohort_assignments, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
            S_compare_test_and_next(x_cohort_assignments, new_test_configuration, x_swapper_a, x_swapper_b, x_orig_ind, x_dest_ind)
          }
        }
      }
    },
    
    
    ########################
    # OPTIMIZING FUNCTIONS #
    ########################
    # some optimizing functions that will help us improve the objective value
    
    
    #' Transfer Function: main executing method
    #'
    #' @param x_cohort_assignments current cohort assignments
    #'
    opt_transfer = function(x_cohort_assignments){
      
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
      
      # OPTIMIZATION: search for better objective
      # Initial assignments for first current explorer (from a handoff cohort) and the first receive group index
      # start with first element, which we will call the explorer, in first candidate handoff cohort; remove and add to first available receiver cohort
      current_explorer <<- unlist(x_cohort_assignments$members[candidate_handoff[1]])[1]
      han_ind <<- 1
      check <- FALSE
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
          check <- TRUE
          break
        }
      }
    },  
    
    
    #' Switch Function: main executing method
    #'
    #' @param x_cohort_assignments current cohort assignments
    #'
    opt_switch = function(x_cohort_assignments){
      # create list of swap origin cohorts. we need to recreate this every time we restart opt_switch as we remove elements from the list throughout the iteration
      candidate_swap_origin <<- c()
      for (i in 1:nrow(x_cohort_assignments)){
        candidate_swap_origin <<- append(candidate_swap_origin, x_cohort_assignments$cohort[[i]])
        current_swapper_a <<- unlist(x_cohort_assignments$members[candidate_swap_origin[1]])[1]
        current_swapper_b <<- unlist(x_cohort_assignments$members[candidate_swap_origin[2]])[1]
      }
      
      # initialize the origin and destination of the swap (we must have at least 2 cohorts, which is an assumption of the problem)
      orig_ind <<- 1  
      dest_ind <<- 2
      
      # make copy of cohort_assignments to be used in a dynamic way through deleting swap a elements that exhaust test swaps and do not improve the objective
      dynamic_cohorts <<- data.frame(x_cohort_assignments)
      
    },
    
    
    init = function(g) {
      # Initialization
      
      # Take in contact matrix and assign a numeric value to each node
      testadj <- as_adjacency_matrix(dlContactGraph)
      
      # produce cost matrix 
      cost_mat <- apply(neighbor_mat, 2, cost_mat_fun)
      
      # arbitrarily split nodes into feasible cohorts
      # divide total nodes by number of cohorts and round up; use this to segment
      segment_size <- ceiling(total_nodes/total_cohorts)
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
      
      
      # Create optimal cohorts given an initial contact network
      
      T_Exhausted <<- FALSE
      S_Exhausted <<- FALSE
      T_improved_optimality <<- TRUE
      S_improved_optimality <<- TRUE
      Optimal_Obj <<- FALSE
      
      while(Optimal_Obj == FALSE){
        T_Exhausted <<- FALSE
        S_Exhausted <<- FALSE
        
        while(S_Exhausted == FALSE){
          # first member of first cohort become the candidate swapper
          S_improved_optimality <<- FALSE
          s2 <- opt_switch(cohort_assignments)
          S_test_configuration <- S_generate_test_assignments(cohort_assignments, current_swapper_a, current_swapper_b, orig_ind, dest_ind)
          S_compare_test_and_next(cohort_assignments, S_test_configuration, current_swapper_a, current_swapper_b, orig_ind, dest_ind)
        }
        
        while(T_Exhausted == FALSE){
          T_improved_optimality <<- FALSE
          t2 <- opt_transfer(cohort_assignments)
          T_test_configuration <- T_generate_test_assignments(cohort_assignments, current_explorer, han_ind, rec_ind)
          T_compare_test_and_next(cohort_assignments, T_test_configuration, current_explorer, han_ind, rec_ind)  
          
        }
        if((T_improved_optimality == FALSE) & (S_improved_optimality == FALSE)){
          Optimal_Obj <<- TRUE
        }
      }
      
      end_time <- Sys.time()
      
      elapsed_time <- end_time - start_time
      elapsed_time
      
      
      
      check_obj <- get_objective_Val(get_all_cohorts_adj_mat_with_graph(cohort_assignments))  
      
      
      #dataframe with nodes and node attributes for cohort
      df_results <- data.frame(
        member = 1:total_nodes,
        cohort_assignment = NA
      )
      
      for(i in 1:total_cohorts){
        for(j in 1:length(unlist(cohort_assignments$members[[i]]))){
          df_results$cohort[unlist(cohort_assignments$members[[i]])[j]] <- i
          # return g, with a cohort property for each node
          igraph::V(g)$cohort[unlist(cohort_assignments$members[[i]])[j]] <- i
        }
      }
      
      return(g)
    }
  )
)

