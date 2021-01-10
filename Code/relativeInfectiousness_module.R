#' Relative Infectiousness Class
#'
#' Each transmitting person has  an infectiousness profile that modifies transmission. 
#' That infectiousness profile is defined a by a function (recommended is gamma a function).
#' Infectiousness changes through time and is defined by the counter attribute. 
#' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7325181.2/
#'
#' @field relInfFunc Vector function that provides values of y between 0 and 1. Define all parameters but x.
#' @field max_recovery_time Numeric, recovery rate. Used to calculate max y from relInfFunc
#' @export rel_infect
rel_infect <- setRefClass(
  "relInfect", #change this to create a new class
  fields = list( relInfFunc="function",
                 max_recovery_time="numeric"
  ),
  methods = list(
    init = function(g) {
      "get Maximum infectiousness value"
      maxInf <- max(relInfFunc(x = 0:max_recovery_time))
      "Initialize the graph with relative infectiousness profile"
      igraph::V(g)$relInf <- relInfFunc(x = igraph::V(g)$counter)/maxInf
      
      return(g)
    },
    donext = function(g) {
      "Update infectiousness profile"
      maxInf <- max(relInfFunc(x = 0:max_recovery_time))
      igraph::V(g)$relInf <- relInfFunc(x = igraph::V(g)$counter)/maxInf
      return(g)
    }
  )
)