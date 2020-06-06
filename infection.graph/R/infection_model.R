#' Infection Model
#'
#' An infection model collects the different components in the model and
#' executes them.
#'
#' @field components A list containing components of the model as reference classes
#' @export infection_model
infection_model <- setRefClass(
  "infection_model",
  fields = list(
    components = "list"
  ),
  methods = list(
    add_component = function(c) {
      "Adds the component c to the list components"
      components <<- c(components, c)
    },
    init_model = function(g) {
      "Initializes g using the init function of each component"
      g <- keepLargeComponent(g)
      for (c in components) {
        g <- c$init(g)
      }
      return(g)
    },
    next_turn = function(g) {
      "Modifies g using the donext function of each component"
      for(c in components) {
        g <- c$donext(g)
      }
      return(g)
    },
    keepLargeComponent <- function(g){
      "Prunes smaller disconnected components off of graph"
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
  )
)
