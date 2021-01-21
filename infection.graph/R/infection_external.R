#' Infection External Class
#'
#' An additional class to implement spreading an infection from external factors
#' or outside of the office. We initialize the graph with the existing nodes.
#' When \code{$donext} is called a random number of nodes will be infected based
#' the external infection \code{rate}.
#'
#' @field rate Probability an adjacent node will become infected
#' @export external_infect
external_infect <- setRefClass(
  "infection_external",
  fields = list(rate="numeric"
                ),
  methods = list(
    init = function(g) {
      return(g)
    },
    donext = function(g) {
      "Randomly infect nodes based on external ratte"
      # Probabilistically get adjacent nodes to infect
      infect_ex <-
        # If not infected or recovered, randomly infect
        igraph::V(g)[infected==0] %>%
        {
          l <- length(.)
          bool <- runif(l) <= rate
          .[bool]
        }

      # Infect random nodes
      igraph::V(g)[infect_ex]$infLoc <- "external"
      igraph::V(g)[infect_ex]$infected <- 1
      igraph::V(g)[infect_ex]$color <- "red"

      return(g)
    }
  )
)
