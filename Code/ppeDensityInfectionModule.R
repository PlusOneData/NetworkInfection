#' Density Infection Class
#'
#' A default class to implement spreading an infection from initial nodes based on
#' number of contacts (density), ppe use, and infectious profiles of individuals.
#' An \code{init_num} of nodes have their \code{$infected} property set to T.
#' When \code{$donext} the probability of infection is calculated using the sum of
#' adjacent infected nodes times the probability of infection. Each transmitting has
#' both an infectiousness profile and ppe attribute which modify transmission. 
#'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7325181.2/
#'
#' @field init_num Number of initial infected nodes
#' @field transRate Probability of infection given contact
#' @field infProfile Vector of relative infectiousness, values range from 0 to 1 
#' @export density_infect
ppe_density_infect <- setRefClass(
  "denInfection", #change this to create a new class
  fields = list(init_num="numeric",
                transRate="numeric",
                infProfile="numeric"
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
      
      ## get susceptible person's ppe value
      focalProbRed <- igraph::V(g)[infected == 0]$infProbReduction
      
      ## get vector of infection statuses
      infStatus <- g %>%
        #get susceptible nodes' neighbors
        igraph::make_ego_graph(nodes = igraph::V(.)[infected==0], mindist = 1) %>%
        # get probability of infection from neighbors
        purrr::map2_lgl(.x = .,.y = focalProbRed, .f = function(x,y){

          #print(y)
          # get infected neighbors
          xInf <- V(x)[infected == 1]$infProbReduction
          xRelInf <- V(x)[infected == 1]$relInf
          
          # print(sprintf("Transmission Rate = %s", transRate))
          # print(sprintf("Focal Node PPE = %s", y))
          # print(sprintf("Neighbor PPE = %s", xInf))

          #prob infection with ppe
          probInf <- sum(transRate*xInf*y*xRelInf)

          infStatus <- rbinom(1,1,min(probInf,1))

          lglInf <- as.logical(infStatus)

          return(lglInf)
        })
      
      #print(head(infStatus))
      
      ## need to think about how to 
      
      # print(head(probInfEgo))
      
      # add focal node PPE
      # probInf <- igraph::V(g)[infected == 0]$infProbReduction*probInfEgo
      
      #print(head(probInfEgo))
      
      #infected status 
      # infStatus <- rbinom(length(probInf),1,min(probInf,1))
      
      # lglInf <- as.logical(infStatus)
    
      igraph::V(g)[infected == 0][infStatus]$infected <- 1
      igraph::V(g)[infected == 1]$color <- "red"
      
      #print("made it out of density inf")
      
      return(g)
    }
  )
)