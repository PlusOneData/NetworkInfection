# Testing module
# Goals: ability to establish different testing regimes on contact network.
# ---- basic components
# testing frequency
# testing results delay
# action taken based on result 
# ---- additional items
# test sensitivity
# test specificity
# false positive rate
# false negative rate
# sampling size


#' Testing Class
#'
#' A default class to implement testing on initial nodes.
#' A graph of n nodes with an \code{$infected} property have their \code{$detected} property set to T or F. 
#' When \code{$donext} is called 1 is added to the \code{$testCounter} property and is evaluated against \code{testDelay}. 
#' If \code{$testCounter} is equal to or greater than \code{testDelay} and \code{$detected} is TRUE, the node becomes recovered.
#'
#' @field testDelay
#' @export default_testing
default_infect <- setRefClass(
  "testing",
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
          bool <- runif(l) <= rate
          .[bool]
        }
      
      # Infect adjacent nodes
      igraph::V(g)[infect_adja]$infected <- 1
      igraph::V(g)[infect_adja]$color <- "red"
      
      return(g)
    }
  )
)