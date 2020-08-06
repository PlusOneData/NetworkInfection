# Testing module
# Goals: ability to establish different testing regimes on contact network.
# ---- basic components
# testing frequency
# testing results delay
# ---- additional items
# test sensitivity
# test specificity
# false positive rate
# false negative rate
# sampling size


#' Testing Class
#'
#' A default class to implement testing on nodes.
#' A graph of n nodes with an \code{$infected} property have their \code{$detected} property set to T or F. 
#' When \code{$donext} is called 1 is added to the \code{$outbreakDay} property and 
#' 1 is added to the \code{$testCounter} property if a an infection was previously detected.
#' If \code{$testCounter} is equal to or greater than \code{testDelay} and \code{$detected} is TRUE, 
#' the \code{$reported} attribute is set to T. 
#'
#' @field testDelay Number time steps after test day to test results
#' @field testFrequency Number time steps between testing
#' @field falseNegRate frequency that a person with covid tests negative
#' @field falsePosRate frequency that a person without covid tests positive
#' @field propTested Proportion of people tested for covid
#' @export default_testing
default_testing <- setRefClass(
  "testing",
  fields = list(testDelay="numeric",
                testFrequency="numeric",
                falseNegRate="numeric",
                falsePosRate="numeric",
                propTested="numeric"
  ),
  methods = list(
    init = function(g) {
      "Test a subest of population"
      igraph::V(g)$tested <- c(rep(T, igraph::vcount(g)*propTested), rep(F, igraph::vcount(g)-(igraph::vcount(g)*propTested))) %>%
        sample()
      "Check whether or not nodes are infected"
      igraph::V(g)$detected <- ifelse(igraph::V(g)$infected ==1 & igraph::V(g)$tested == T, rbinom(1,1,(1-falseNegRate)),0)
      igraph::V(g)$detected <- ifelse(igraph::V(g)$infected ==0 & igraph::V(g)$tested == T, rbinom(1,1,0+falsePosRate), igraph::V(g)$detected)
      
      "Set test delay counter"
      igraph::V(g)$testCounter <- 0
      "Set outbreak day counter for testing interval"
      igraph::V(g)$outbreakDay <- 1
      "Set Reported value"
      igraph::V(g)$reported <- 0
      return(g)
    },
    donext = function(g) {
      
      "add a day to the outbreak day counter"
      igraph::V(g)$outbreakDay <- igraph::V(g)$outbreakDay + 1 
      
      "increment or reset testCounter for detected cases"
      igraph::V(g)$testCounter <- ifelse(igraph::V(g)$detected == 1,igraph::V(g)$testCounter + 1, 0)
      
      "Test for disease at certain intervals"
      if((max(igraph::V(g)$outbreakDay) %% testFrequency) == 0){ 
        "Test a subest of population"
        V(g)$tested <- c(rep(T, igraph::vcount(g)*propTested), rep(F, igraph::vcount(g)-(igraph::vcount(g)*propTested))) %>%
          sample()

        "Test whether or not nodes are infected if they are part of the sample and didnt test positive in the last round - would prefer these tests be independent"
        igraph::V(g)$detected <- ifelse(igraph::V(g)$infected ==1 & igraph::V(g)$tested == T & igraph::V(g)$testCounter < testDelay, rbinom(1,1,(1-falseNegRate)),igraph::V(g)$detected)
        igraph::V(g)$detected <- ifelse(igraph::V(g)$infected !=1 & igraph::V(g)$tested == T & igraph::V(g)$testCounter < testDelay, rbinom(1,1,0+falsePosRate), igraph::V(g)$detected)
        
      }
      
      
      "Check testCounter and report nodes that have completed delay period"
      igraph::V(g)$reported <- ifelse(igraph::V(g)$testCounter == testDelay,1, igraph::V(g)$reported)
      igraph::V(g)$color <- ifelse(igraph::V(g)$reported == 1,"yellow", igraph::V(g)$color)
      
      return(g)
    }
  )
)
