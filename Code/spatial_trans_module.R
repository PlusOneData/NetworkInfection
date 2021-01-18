#' Spatial transmission Module
#'
#' At the start of each day it is assumed all rooms are not contaminated. Rooms are contaminated at
#' a given rate and decontaminated at a given rate. Individuals can be infected after spending
#' a certain amount of time in a room with a certain level of contamination.
#'
#' Emission rate  infection risk based on work by Buonanno et al. 
#' \url{https://www.sciencedirect.com/science/article/pii/S0160412020312800}
#' 
#' 
#' Emission Rate
#' 
#' \deqn{ER_{q,j} = c_v*c_i*IR*\sum_{i=1}^4(N_{i,j}*V_i)}
#' 
#' \eqn{ER_{q,j}} = quanta emission rate
#' \eqn{c_v} = viral load in sputum
#' \eqn{c_i} = conversion factor between quanta and infectious dose viral rna copies
#' \eqn{IR} = inhalation rate
#' \eqn{\sum_{i=1}^4(N_{i,j}*V_i)} = sum of different expiration types
#'
#' In this module, we have made simplifying assumptions about expiration types and 
#' inhalation rates - we assume they are homogeneous for all individuals. 
#'
#'Viral concentration
#'
#'\deqn{n(t) = \frac{ER_q*I}{IVRR*V}+(n_o+frac{ER_q*I}{IVRR})*frac{e^{IVRR*t}}{V}}
#'
#'\eqn{IVRR} = infectious viral removal rate (ventilation + decay + deposition)
#'\eqn{I} = number of infecitous individuals
#'\eqn{ER_q} = quanta emission rate
#'\eqn{V} = volume of space
#'\eqn{n_0} = initial concentration of virus
#'
#'Infection Risk 
#'
#'A funciton of exposure time
#'
#'\deqn{R = (1-e^{-IR\int_0^Tn(t)dt)}
#'
#'\eqn{IR} = inhalation rate
#'\eqn{T} = total time of exposure
#'
#'
#' @field rooms Dataframe of rooms with name and volume attributes
#' @field infConc Numeric, concentration where room becomes infectious
#' @field conRate Numeric, rate of contamination by infectious individuals
#' @field deconRate Numeric, rate of decontamination (may move to room attribute)
#' @field envTransRate Numeric, transmission rate for environmental contamination (0 to 1)
#' @export spat_tran
spat_tran <- setRefClass(
  "spatInf", #change this to create a new class
  fields = list( rooms="data.frame",
                 infConc = "numeric",
                 conRate="numeric",
                 deconRate= "numeric",
                 envTransRate = "numeric"
                 #schedule
  ),
  methods = list(
    init = function(g) {
      "give each node a schedule"
      # creating random schedules in 15 min blocks
      igraph::V(g)$schedule <- sample(rooms$name,32*igraph::vcount(g),replace = T) %>% 
                                split(., ceiling(seq_along(.)/32)) %>% 
                                purrr::map(.x=., function(x){
                                  paste(x,collapse = "")
                                }) %>% 
                                purrr::flatten_chr()
      
      "simulate spatial infections and movements"
      
      rooms$infStatus <- 0
      rooms$virusConc <- 0 
      
        for(room in rooms$name){
          df <- rooms %>% 
            filter(name == room)
          
          # think about making a room class to handle unique room actions to 
          # compute who gets infected. Would initialize a group of rooms. At 
          # each time step individuals would be assigned and room object would
          # tell us who got infected. 
          
          for(i in 1:32){
            # need to find where each node is at time step 1
            igraph::V(g)$currentLoc <- igraph::V(g)$schedule %>% 
              stringr::str_sub(.,start = i,end = i)
            
            nodes <- igraph::V(g)[currentLoc == room]
            
            #how much sars-cov-2 is emitted into the room
            roomCon <- (sum(nodes$relInf*conRate))/df$volume
            
            #update virus concentration in room and remove 
            df$virusConc <- (df$virusConc + roomCon)*deconRate # additional terms vent vs natural? 
            
            if(df$virusConc >= infConc){ 
              
              ## get probInf given infProbReduction
              probInf <- igraph::V(g)[infected == 0]$infProbReduction * envTransRate #secondary attack rate from aerosols
              
              ## get inf status
              infStatus <- rbinom(1,1,min(probInf,1))
              
              igraph::V(g)[infected == 0][infStatus]$infected <- 1
              igraph::V(g)[infected == 1]$color <- "red"
              
              # room x time matrix
              # need to be compute schedules on mass
            }
          }
        }
      
      return(g)
    },
    donext = function(g) {

      "simulate spatial infections and movements"
      rooms$infStatus <- 0
      rooms$virusConc <- 0 
      
      for(room in rooms$name){
        df <- rooms %>% 
          filter(name == room)
        
        for(i in 1:32){
          # need to find where each node is at time step 1
          igraph::V(g)$currentLoc <- igraph::V(g)$schedule %>% 
            stringr::str_sub(.,start = i,end = i)
          
          nodes <- igraph::V(g)[currentLoc == room]
          
          #how much sars-cov-2 is emitted into the room
          roomCon <- (sum(nodes$relInf*conRate))/df$volume
          
          #update virus concentration in room and remove 
          df$virusConc <- (df$virusConc + roomCon)*deconRate 
          
          if(df$virusConc >= infConc){ 
            
            ## get probInf given infProbReduction
            probInf <- igraph::V(g)[infected == 0]$infProbReduction * envTransRate
            
            ## get inf status
            infStatus <- rbinom(1,1,min(probInf,1))
            
            igraph::V(g)[infected == 0][infStatus]$infected <- 1
            igraph::V(g)[infected == 1]$color <- "red"
            
            
          }
        }
      }
      
      return(g)
    }
  )
)