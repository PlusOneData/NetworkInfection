###### build model simulator

library(igraph)
library(infection.graph)
library(animation)
library(readxl)
library(dplyr)
library(tidyr)
library(visNetwork)
library(networkD3)
library(ggplot2)

source("./Code/densityInfectionModule.R")

## Eventually want to parallelize

#### ingest Graph Data

#read in DL Graph

dlContactMatrix <- readxl::read_xlsx("./Data/Discovery Lab Contact Network.xlsx")

groupCols <- names(dlContactMatrix)[-1]

dlContactMatrix <- dlContactMatrix %>% 
  mutate_at(vars(groupCols), as.integer)

dlContactMatrix[is.na(dlContactMatrix)]<- 6

## edge list 

## origin not equal to destination 

edgeList <- dlContactMatrix %>% 
  pivot_longer(cols = -Name) %>% 
  filter(value > 0) %>% 
  rename("Origin" = "Name") %>% 
  rename("Dest" = "name") %>% 
  select(-value)

#get graph from edgelist 
dlContactGraph <- igraph::graph_from_edgelist(el = as.matrix(edgeList),directed = F)

dlContactGraph <- simplify(graph = dlContactGraph,remove.loops = T)

## dl contac


### take in model with modules

n <- 1000
ed <- n * 4
prob.infect <- .1
gmma <- 14

covid_di <- density_infect(init_num = 3, transRate = prob.infect)
covid_dr <- default_recover(max_recovery_time = 20)
covid_model_density <- infection_model(components = list(covid_di, covid_dr))

covid_di <- default_infect(init_num = 3, rate = prob.infect)
covid_dr <- default_recover(max_recovery_time = 20)
covid_model_freq <- infection_model(components = list(covid_di, covid_dr))



### loop through simulation
runSims <- function(graphObj, modelObj, runs, timeSteps){
  
  runList <- list()
  
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
  }
  
  simObj <- do.call(rbind,runList)
  
  return(simObj)
}


testSim <- runSims(graphObj = dlContactGraph,modelObj = covid_model_density, runs = 100,timeSteps = 50)

sumSim <- testSim %>% 
  group_by(type,time) %>% 
  summarize(meanValue = median(value)) %>% 
  ungroup() %>% 
  mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered") ))

testSim %>% 
  mutate(group = paste0(type,simRun)) %>% 
  mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered") )) %>% 
  # filter(type == "infected") %>% 
  ggplot() +
  geom_line(aes(x = time, y = value, group = group ), color = "grey", size = 1.5, alpha = 0.2) +
  geom_line(data = sumSim, aes(x = time, y = meanValue, color = typeFac), size = 1.5) +
    theme_bw() +
  labs(title = "SIR Distribution") +
  scale_color_brewer(type = 'qual') +
  facet_wrap(~typeFac)

