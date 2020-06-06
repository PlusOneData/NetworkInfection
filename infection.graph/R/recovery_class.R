#' Default Recovery Class
#'
#' A default class to model a node's recovery. An infected node has a chance to
#' recover equal to counter/max_recovery_time. Counter keeps track the duration
#' of time a node has been infected. We set to -1 because donext automatically
#' increments counter and newly infected nodes this turn have a chance to
#' recover
#'
#' @field max_recovery_time Maximum time steps before a node is guaranteed to recover
#' @export default_recover
default_recover <- setRefClass(
  "recovery",
  fields = list(max_recovery_time = "numeric"),
  methods = list(
    init = function(g) {
      "Initialize all nodes with a counter and a recovered property"
      igraph::V(g)$counter <- -1 # Used to compute recovery time
      igraph::V(g)$recovered <- F
      return(g)
    },
    donext = function(g) {
      "Increments counter and computes whether a nodes state should be moved to recovered"
      igraph::V(g)[infected]$counter = igraph::V(g)[infected]$counter + 1
      # Recover infected nodes
      ## Infected nodes have a probability of infected days/20 to recover
      infectedNodes <- igraph::V(g)[infected]
      propRecover <- infectedNodes$counter/max_recovery_time
      rollDice <- runif(length(infectedNodes))
      # Update recovered nodes
      igraph::V(g)[infectedNodes]$recovered <- rollDice < propRecover
      igraph::V(g)[recovered]$infected <- F
      igraph::V(g)[recovered]$color <- "green"

      return(g)
    }
  )
)
