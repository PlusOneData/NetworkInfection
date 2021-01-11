#' Spatial transmission Module
#'
#' At the start of each day it is assumed all rooms are not contaminated. Rooms are contaminated at
#' a given rate and decontaminated at a given rate. Individuals can be infected after spending
#' a certain amount of time in a room with a certain level of contamination.
#'
#' @field rooms Dataframe of rooms with name and volume attributes
#' @field infConc Numeric, concentration where room becomes infectious
#' @field conRate Numeric, rate of contamination by infectious individuals
#' @field deconRate Numeric, rate of decontamination (may move to room attribute)
#' @field envTransRate Numeric, transmission rate for environmental contamination (0 to 1)
#' @export spatial_transmission
spatial_transmission <- setRefClass(
  "spatInf", #change this to create a new class
  fields = list( rooms="dataframe",
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
              
              ## get 
              infStatus <- rbinom(1,1,min(probInf,1))
              
              igraph::V(g)[infected == 0][infStatus]$infected <- 1
              igraph::V(g)[infected == 1]$color <- "red"
              
              
            }
          }
        }
        
        
       
      
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