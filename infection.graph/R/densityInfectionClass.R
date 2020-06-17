#' Density Infection Class
#'
#' A default class to implement spreading an infection from initial nodes based on
#' infection density.
#' An \code{init_num} of nodes have their \code{$infected} property set to T.
#' When \code{$donext} the probability of infection is calculated using the sum of
#' adjacent infected nodes times the probability of infection.
#'
#' @field init_num Number of initial infected nodes
#' @field transRate Probability of infection given contact
#' @export density_infect
density_infect <- setRefClass(
  "denInfection", #change this to create a new class
  fields = list(init_num="numeric",
                transRate="numeric"
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
      print("In donext")

      infectedStatus <- g %>%
        #get susceptible nodes' neighbors
        igraph::ego(nodes = igraph::V(.)[infected==0], mindist = 1) %>%
        # get number of infected neighbors
        purrr::map_lgl(.x = .,.f = function(x){
           print(x$infected)

           infN<- sum(x$infected == 1)

          #prob infection
          probInf  <- infN*transRate
          #infected status
          infStatus <- rbinom(1,1,min(probInf,1))

          lglInf <- as.logical(infStatus)

          print(infN)
          return(lglInf)
        })


        igraph::V(g)[infected == 0][infectedStatus]$infected <- 1
        igraph::V(g)[infected == 1]$color <- "red"

      return(g)
    }
  )
)
