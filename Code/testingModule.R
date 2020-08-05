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
#' When \code{$donext} is called 1 is added to the \code{$outbreakDay} property and 
#' 1 is added to the \code{$testCounter} property if a an infection was previously detected.
#' If \code{$testCounter} is equal to or greater than \code{testDelay} and \code{$detected} is TRUE, the node becomes recovered.
#'
#' @field testDelay Number time steps to test results
#' @field testFrequency Number time steps between testing
#' @field falseNegRate frequency that a person with covid tests negative
#' @field falsePosRate frequency that a person without covid tests positive
#' @export default_testing
default_testing <- setRefClass(
  "testing",
  fields = list(testDelay="numeric",
                testFrequency="numeric",
                falseNegRate="numeric",
                falsePosRate="numeric"
  ),
  methods = list(
    init = function(g) {
      "Check whether or not nodes are infected"
      igraph::V(g)$detected <- ifelse(igraph::V(g)$infected ==1, rbinom(1,1,(1-falseNegRate)), rbinom(1,1,0+falsePosRate))
      "Set test delay counter"
      igraph::V(g)$testCounter <- 0
      "Set outbreak day counter for testing interval"
      igraph::V(g)$outbreakDay <- 0
      "Set Reported value"
      igraph::V(g)$reported <- 0
      return(g)
    },
    donext = function(g) {
      
      "add a day to the outbreak day counter"
      igraph::V(g)$outbreakDay <- igraph::V(g)$outbreakDay + 1 
      
      "Test for disease at certain intervals"
      if((max(igraph::V(g)$outbreakDay) %% testFrequency) == 0){ 
      "Test whether or not nodes are infected"
      igraph::V(g)$detected <- ifelse(igraph::V(g)$infected ==1, rbinom(1,1,(1-falseNegRate)), rbinom(1,1,0+falsePosRate))
      }
      
      "increment or reset testCounter for detected cases"
      igraph::V(g)$testCounter <- ifelse(igraph::V(g)$detected == 1,igraph::V(g)$testCounter +1, 0)
      
      "Check testCounter and Remove detected nodes"
      igraph::V(g)$reported <- ifelse(igraph::V(g)$testCounter == testDelay,1, igraph::V(g)$infected)
      igraph::V(g)$color <- ifelse(igraph::V(g)$testCounter == testDelay,"yellow", igraph::V(g)$color)
      
      return(g)
    }
  )
)
