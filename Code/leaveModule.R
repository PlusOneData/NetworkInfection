# Leave Module
# Goals: Create leave of absence for nodes with reported covid
# ---- basic components
# return infection status
# duration of leave
# testing
# ---- additional items
# compliance

#' Leave Class
#'
#' A default class to implement leave policies.
#' A graph of n nodes with a \code{$reported} property have their \code{$leave} property set to T or F 
#' and \code{$infected} property set to 3. 
#' When \code{$donext} is called 1 is added to the \code{$leaveCounter} property and 
#' infection status evaluated.
#' If \code{$reported} is false and \code{$leaveCounter} equal to or
#' greater than \code{leaveDuration} and \code{$infected} is 2, 
#' the \code{$infect} attribute is set to 2 or "recovered". 
#'
#' @field leaveDuration Number time steps a reported case is place on leave
#'
#' @export default_leave
default_leave <- setRefClass(
  "leave",
  fields = list(
    leaveDuration="numeric",
    max_recovery_time="numeric"
  ),
  methods = list(
    init = function(g) {
      "Set leave status"
      igraph::V(g)$leave <- FALSE
      "Set leave counter"
      igraph::V(g)$leaveCounter <- 0
      return(g)
    },
    donext = function(g) {
      
      "Increments counter and computes whether a nodes state should be moved to recovered"
      igraph::V(g)[infected==3]$counter <- igraph::V(g)[infected==3]$counter + 1
      # Recover infected nodes
      ## Infected nodes have a probability of infected days/20 to recover
      leaveNodes <- igraph::V(g)[infected==3]
      propRecover <- leaveNodes$counter/max_recovery_time
      rollDice <- runif(length(leaveNodes))
      # Update recovered nodes
      igraph::V(g)[leaveNodes]$recovered <- rollDice < propRecover
      igraph::V(g)[recovered]$infected <- 2
      igraph::V(g)[recovered]$color <- "green"
      
      "set leave status based on whether or not the covid case was reported or if they have passed through the leave period"
      igraph::V(g)$leave <- ifelse(igraph::V(g)$reported == 1 | (igraph::V(g)$leaveCounter <= leaveDuration & igraph::V(g)$leaveCounter > 0) ,TRUE, FALSE)
      
      "add a day to the leaveCounter"
      igraph::V(g)$leaveCounter <- ifelse(igraph::V(g)$leave == TRUE, igraph::V(g)$leaveCounter + 1,igraph::V(g)$leaveCounter) 
      
      "set infection status to Removed"
      igraph::V(g)$infected <- ifelse(igraph::V(g)$leave == TRUE, 3,igraph::V(g)$infected)
      
      "Set leave color"
      igraph::V(g)$color <- ifelse(igraph::V(g)$leave == TRUE,"orange", igraph::V(g)$color)
  
      return(g)
    }
  )
)
