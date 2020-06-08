# Select largest component from Scale Free network because infection may begin in smaller component
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

# Initialize graph with infected values
#' @export
initG <- function(g, n, onlyLarge = T){
  # Drop smaller disconnected components
  if(onlyLarge){
    g <- keepLargeComponent(g)
  }

  # Create an array of n infected and g-n uninfected. Assign to infected property
  igraph::V(g)$infected <- c(rep(T, n), rep(F, igraph::vcount(g)-n)) %>%
    sample() # reorders the arrangement of infected nodes
  igraph::V(g)$color <- ifelse(igraph::V(g)$infected, 'red', 'blue')
  igraph::V(g)$counter <- 0 # Used to compute recovery time
  igraph::V(g)$recovered <- F

  g
}

# Execute modification to next time step
#' @export
nextTurn <- function(g, prob.infect){

  igraph::V(g)[infected]$counter = igraph::V(g)[infected]$counter + 1

  # Probabilistically get adjacent nodes to infect
  infect_adja <- g %>%
    # Ego gets neighboring nodes a mindist away
    igraph::ego(nodes = igraph::V(.)[infected], mindist = 1) %>%
    # Turn list of neighbors into an unique, atomic list
    unlist() %>%
    unique() %>%
    # If not infected or recovered, randomly infect
    {igraph::V(g)[.][!infected & !recovered]} %>%
    {
      l <- length(.)
      bool <- runif(l) <= prob.infect
      .[bool]
    }

  # Infect adjacent nodes
  igraph::V(g)[infect_adja]$infected <- T
  igraph::V(g)[infect_adja]$color <- "red"

  # Recover infected nodes
  ## Infected nodes have a probability of infected days/20 to recover
  infectedNodes <- igraph::V(g)[infected]
  propRecover <- infectedNodes$counter/20
  rollDice <- runif(length(infectedNodes))
  # Update recovered nodes
  igraph::V(g)[infectedNodes]$recovered <- rollDice < propRecover
  igraph::V(g)[recovered]$infected <- F
  igraph::V(g)[recovered]$color <- "green"

  g
}

# Process time increments and model the infection
#' @export
createTimeline <- function(g, t, prob.infect){
  timedNetworks <- list(g)

  for( x in 2:t){
    timedNetworks[[x]] <- nextTurn(timedNetworks[[x-1]], prob.infect)
  }
  timedNetworks
}
