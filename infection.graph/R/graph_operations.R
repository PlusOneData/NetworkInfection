#' Isolate Largest Component
#'
#' Some networks generate small disconnected nodes or component. Use \code{keepLargeComponent} to
#' isolate the largest component so infection can spread in the model.
#'
#' @param g An \code{igraph}
#' @return The graph \code{g} pruned of small disconnected components
#' @export
keepLargeComponent <- function(g){
  lrgComp <- g %>%
    igraph::components() %>%
    {
      comp_id <- which.max(.$csize); # csize = numeric vector giving sizes of the clusters
      comp_v <- which(.$membership == comp_id) # collect all vertices belonging to comp_id
      comp_v
    }

  # Remove all vertices not in largest component
  g <- g - igraph::V(g)[!igraph::V(g) %in% lrgComp]

  return(g)
}

#' Initialize graph with infected values
#'
#' Given a graph \code{g} and an infection model, prunes the disconnected components
#' from \code{g} and initializes the graph using the model components stored in model
#'
#' @param g An \code{igraph}
#' @param model An infection model initalized with components
#' @param onlyLarge Boolean indicating whether to prune \code{g} or not
#' @return A graph \code{g} initialized with properties to run the model
#' @export
initG <- function(g, model, onlyLarge = T){
  # Drop smaller disconnected components
  if(onlyLarge){
    g <- keepLargeComponent(g)
  }

  g <- model$init_model(g)

  g
}

#' Execute modification to next time step
#'
#' Run a single time step of the model on graph \code{g}
#'
#' @param g An \code{igraph}
#' @param model An infection model with components
#' @return A graph \code{g} modified by actions stored in model
#' @export
nextTurn <- function(g, model){

  g <- model$next_turn(g)
  g
}

#' Process time increments and model the infection
#'
#' Computes a time series of \code{t} steps on graph \code{g} as t_0 using model. The graph
#' must already be initialized. Run \code{initG(g, model)} prior to this function.
#'
#' @param g An \code{igraph}
#' @param t Number of time steps to compute
#' @param model An infection model
#' @return A list of networks at each time step
#' @export
createTimeline <- function(g, t, model){
  timedNetworks <- list(g)

  for( x in 2:t){
    timedNetworks[[x]] <- nextTurn(timedNetworks[[x-1]], model)
  }
  timedNetworks
}
