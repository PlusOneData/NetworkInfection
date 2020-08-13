#' Infection Class 
#'
#' A default class to implement spreading an infection from initial nodes.
#' An \code{init_num} of nodes have their \code{$infected} property set to T.
#' When \code{$donext} is called each node adjacent to an infected node will
#' generate a random number. If this value is less than \code{rate}, the node
#' becomes infected.
#'
#' @field init_num Number of initial infected nodes
#' @field rate Probability an adjacent node will become infected
#' @export ppe_infect
ppe_infect <- setRefClass(
  "infectionPPE",
  fields = list(init_num="numeric",
                rate="numeric"
  ),
  methods = list(
    init = function(g) {
      "Initialize the graph with init_num infected nodes and set infected to red and susceptible to blue"
      igraph::V(g)$infected <- c(rep(1, init_num), rep(0, igraph::vcount(g)-init_num)) %>%
        sample() # reorders the arrangement of infected nodes
      igraph::V(g)$color <- ifelse(igraph::V(g)$infected==1, 'red', 'blue')
      return(g)
    },
    donext = function(g) {
      "Infect nodes adjacent to currently infected nodes"
      # Probabilistically get adjacent nodes to infect
      infect_adja <- g %>%
        # Ego gets neighboring nodes a mindist away
        igraph::ego(nodes = igraph::V(.)[infected==1], mindist = 1) %>%
        # Turn list of neighbors into an unique, atomic list
        unlist() %>%
        unique() %>%
        # If not infected or recovered, randomly infect
        {igraph::V(g)[.][infected==0]} %>%
        {
         
          l <- length(.)
          # reduce the probability of infection given ppe use
          bool <- runif(l) <= rate*V(g)[.]$infProbReduction
          .[bool]
        }
      
      # Infect adjacent nodes
      igraph::V(g)[infect_adja]$infected <- 1
      igraph::V(g)[infect_adja]$color <- "red"
      
      return(g)
    }
  )
)
