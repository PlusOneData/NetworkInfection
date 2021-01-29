###### build model simulator

#' Generate multiple simulation runs
#'
#'
#' @param graphObj a graph object from iGraph
#' @param modelObj a model object from network.infection modules
#' @param runs Numeric, the number of times the simulation should be executed
#' @param timeSteps Numeric, how many time steps should each simulation last
#'
#' @return dataframe 
#' @export
#'
#' @examples
runSims <- function(graphObj, modelObj, runs, timeSteps){
  
  #data store
  runList <- list()
  timeLineList <- list()
  ### loop through simulation
  for(i in 1:runs){
  
  # initialize simulation run
  simInit <- graphObj %>% 
    modelObj$init_model()
  
  # create timeline
  timeLine <- createTimeline(simInit, timeSteps, modelObj)
  ### Summarize outputs
  stats3 <- getStats(timeLine)
  
  stats3$simRun <- as.character(i)
  
  #store runs
  runList[[i]] <- stats3
  timeLineList[[i]] <- timeLine[[max(timeSteps)]]
  }
  
  simObj <- do.call(rbind,runList)
  
  
  
  return(list(sirStats = simObj, timeLines =  timeLineList))
}



