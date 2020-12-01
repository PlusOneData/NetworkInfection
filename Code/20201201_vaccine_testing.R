### simulation test ground

# devtools::install("./infection.graph")

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
source("./Code/testingModule.R")
source("./Code/leaveModule.R")
source("./Code/simulationModule.R")
source("./Code/vaccine_module.R")
source("./Code/ppeInfect_Module.R")
# source("./infection_external.R")


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
# secondary attack rate in non-household contacts ~ 3.9%
# secondary attack rate in household contacts ~ 11.3%
# https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2772238

n <- 1000
ed <- n * 4
prob.infect <- .07
ext.infect <- .00000001
gmma <- 14


covid_di <- ppe_infect(init_num = 3, rate = prob.infect)
covid_dr <- default_recover(max_recovery_time = 20)
covid_dt <- default_testing(testDelay = 1, testFrequency = 2, falseNegRate = 0.03, falsePosRate = 0.001, propTested = 1)
covid_lv <- default_leave(leaveDuration = 10, max_recovery_time = 20)
covid_ex <- external_infect(rate = ext.infect)
covid_vx <- default_vax(vaxEff = 0.95, propVax = 0.2)
covid_model_vax <- infection_model(components = list(covid_di, covid_dr,covid_dt,covid_vx, covid_lv, covid_ex))

### simulation


testSim <- runSims(graphObj = dlContactGraph,modelObj = covid_model_vax, runs = 100,timeSteps = 50)

testSim$type %>% unique

sumSim <- testSim %>% 
  group_by(type,time) %>% 
  summarize(meanValue = median(value)) %>% 
  ungroup() %>% 
  mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered","leave") ))

testSim %>% 
  mutate(group = paste0(type,simRun)) %>% 
  mutate(typeFac = factor(x = type,levels = c("susceptible","infected","recovered","leave") )) %>% 
  # filter(type == "infected") %>% 
  ggplot() +
  geom_line(aes(x = time, y = value, group = group ), color = "grey", size = 1.5, alpha = 0.2) +
  geom_line(data = sumSim, aes(x = time, y = meanValue, color = typeFac), size = 1.5) +
  theme_bw() +
  labs(title = "SIR Distribution") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~typeFac, nrow = 1)
