#' Spatial transmission Module
#'
#' At the start of each day it is assumed all rooms are not contaminated. Rooms are contaminated at
#' a given rate and decontaminated at a given rate. Individuals can be infected after spending
#' a certain amount of time in a room with a certain level of contamination.
#'
#' Emission rate  infection risk based on work by Buonanno et al. 
#' \url{https://www.sciencedirect.com/science/article/pii/S0160412020312800}
#' \url{https://www.sciencedirect.com/science/article/pii/S0160412020320675?via%3Dihub}
#' 
#' Important terms:
#' 
#' Quanta = a quantum is defined as the dose of airborne 
#' droplet nuclei required to cause infection in 63% of
#' susceptible persons.
#' 
#' Emission Rate
#' 
#' \deqn{ER_{q,j} = c_v*c_i*IR*\sum_{i=1}^4(N_{i,j}*V_i)}
#' 
#' \eqn{ER_{q,j}} = quanta emission rate = \eqn{quanta h^{-1}}
#' \eqn{c_v} = viral load in sputum = RNA copies per mL
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
#' @field rooms Dataframe of rooms with name, volume (m3), and airExchange (air exchanges per hour) attributes
#' @field inRate Numeric, inhalation rate for individuals (\eqn{m^3/hour})
#' @field emRate Numeric, rate of contamination by infectious individuals (\eqn{quanta/hour/person})
#' @field maxRoomDensity Numeric, maximum number of people/m^3 in a room
#' @export spat_tran
spat_tran <- setRefClass(
  "spatInf", #change this to create a new class
  fields = list( rooms="data.frame",
                 inRate="numeric",
                 emRate="numeric",
                 maxRoomDensity = "numeric"
                 #schedule
  ),
  methods = list(
    init = function(g) {
      
      V(g)$schedule <- ""
      "get nodes not on leave"
      gP <- igraph::V(g)[infected!=3]
      
      g1 <- igraph::induced_subgraph(g,gP)
      
      "give each node a schedule"
      # creating random schedules in 15 min blocks
      # need to add capacity attribute to rooms
      
      g2 <- nodeSchedule(g1,rooms,maxRoomDensity,32)
      
      "simulate spatial infections and movements"
      
      rooms$infStatus <- 0
      rooms$virusConc <- 0 
      
      g3 <- spatialTrans(g2,rooms)
      
      igraph::V(g)[infected!=3]$color <- igraph::V(g3)$color
      igraph::V(g)[infected!=3]$infLoc <- igraph::V(g3)$infLoc
      igraph::V(g)[infected!=3]$infected <- igraph::V(g3)$infected
      
      return(g)
    },
    donext = function(g) {
      
      V(g)$schedule <- ""
      "get nodes not on leave"
      gP <- igraph::V(g)[infected!=3]
      
      g1 <- igraph::induced_subgraph(g,gP)
      
      "give each node a schedule"
      # creating random schedules in 15 min blocks
      # need to add capacity attribute to rooms
      
      # browser()
      g2 <- nodeSchedule(g1,rooms,maxRoomDensity,32)
      
      "simulate spatial infections and movements"
      
      rooms$infStatus <- 0
      rooms$virusConc <- 0 
      
      g3 <- spatialTrans(g2,rooms)
      
      igraph::V(g)[infected!=3]$color <- igraph::V(g3)$color
      igraph::V(g)[infected!=3]$infLoc <- igraph::V(g3)$infLoc
      igraph::V(g)[infected!=3]$infected <- igraph::V(g3)$infected
      
      return(g)
    },
    spatialTrans = function(g,rooms){
      
      ## want to get location at time point 1 
      for(i in 1:32){
        # need to find where each node is at time step 1
        igraph::V(g)$currentLoc <- igraph::V(g)$schedule %>% 
          stringr::str_sub(.,start = i,end = i)

        # loop through each room
        for(room in rooms$name){
        
        gP <- V(g)[currentLoc == room]
        
        if(length(gP)!=0){
          
            g1 <- igraph::induced_subgraph(g,gP)
            
            "make sure there is someone in the room"
                
            df <- rooms %>% 
              filter(name == room)
            #how much sars-cov-2 is emitted into the room
            roomCon <- (sum(igraph::V(g1)$relInf*emRate))/((df$airExchange/4)*df$volume)
            timeStepEm <- (sum(igraph::V(g1)$relInf*emRate))/((df$airExchange/4))
            #update virus concentration in room and remove
            # need to add decay and settling term here
            df$virusConc <- roomCon + (df$virusConc +  timeStepEm)*((exp(-(df$airExchange/4)))/df$volume) 
            
            infRisk <- infectionRisk(inRate/4,df$virusConc)
          
            ## get probInf given infProbReduction
            
            probInf <- igraph::V(g1)[infected == 0]$infProbReduction * infRisk
            
            if(length(probInf) > 0){
              
            ## get inf status
            infStatus <- rbinom(1,1,min(probInf,1))
            
            igraph::V(g1)[infected == 0][infStatus]$infLoc <- room
            igraph::V(g1)[infected == 0][infStatus]$infected <- 1
            igraph::V(g1)[infected == 1]$color <- "red"
            
            #browser()
            
            igraph::V(g)[currentLoc == room]$color <- igraph::V(g1)$color
            igraph::V(g)[currentLoc == room]$infLoc <- igraph::V(g1)$infLoc
            igraph::V(g)[currentLoc == room]$infected <- igraph::V(g1)$infected
            }
            
            #replace values in room before exiting loop
            rooms[rooms$name == room,] <- df
        } else {
          
          df <- rooms %>% 
            filter(name == room)
          
          df$virusConc <- (df$virusConc)/((df$airExchange/4)*df$volume) 
          
          rooms[rooms$name == room,] <- df
          
          }
        }
      }
      
      return(g)
    },
    infectionRisk = function(inRate,roomConc){
      
      "infection risk is expressed as a percent"
      infRisk <- (1-exp(-inRate*roomConc))
      
      print(infRisk)
      
      return(infRisk)
    },
    nodeSchedule = function(g,rooms,maxRoomDensity,timeSteps){
      
      schedList <- list()
      
      # seats available in all rooms
      roomVec <- rep(rooms$name, floor(rooms$volume*maxRoomDensity),rooms$capacity)
      
      for(i in 1:timeSteps){
        
        if(length(roomVec)<igraph::vcount(g)){
          stop("more nodes than capacity allows")
        }
        
        schedList[[i]] <- sample(roomVec, size = igraph::vcount(g),replace = F)
        
      }
      #browser()
      igraph::V(g)$schedule <- schedList %>%
        purrr::set_names(as.character(1:timeSteps)) %>% 
        purrr::map_dfc(.,paste) %>%
        do.call(paste0,.)
      
      return(g)
    }
  )
)