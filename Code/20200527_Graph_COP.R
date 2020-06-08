library(igraph)
library(infection.graph)
?random.graph.game

# Select largest component from Scale Free network because infection may begin in smaller component
keepLargeComponent <- function(g){
  lrgComp <- g %>%
    components() %>%
    {
      comp_id <- which.max(.$csize); # csize = numeric vector giving sizes of the clusters
      comp_v <- which(.$membership == comp_id) # collect all vertices belonging to comp_id
      comp_v
    }
  
  # Remove all vertices not in largest component
  g <- g - V(g)[!V(g) %in% lrgComp]
  
  return(g)
}

# Initialize graph with infected values
initG <- function(g, n, onlyLarge = T){
  # Drop smaller disconnected components
  if(onlyLarge){
    g <- keepLargeComponent(g)
  }
  
  # Create an array of n infected and g-n uninfected. Assign to infected property
  V(g)$infected <- c(rep(T, n), rep(F, vcount(g)-n)) %>%
    sample() # reorders the arrangement of infected nodes
  V(g)$color <- ifelse(V(g)$infected, "#d95f02", "#7570b3")
  V(g)$counter <- 0 # Used to compute recovery time
  V(g)$recovered <- F
  
  g
}

# Execute modification to next time step
nextTurn <- function(g, prob.infect){
  
  V(g)[infected]$counter = V(g)[infected]$counter + 1
  
  # Probabilistically get adjacent nodes to infect
  infect_adja <- g %>%
    # Ego gets neighboring nodes a mindist away
    ego(nodes = V(.)[infected], mindist = 1) %>%
    # Turn list of neighbors into an unique, atomic list
    unlist() %>%
    unique() %>%
    # If not infected or recovered, randomly infect
    {V(g)[.][!infected & !recovered]} %>%
    {
      l <- length(.)
      bool <- runif(l) <= prob.infect
      .[bool]
    }
  
  # Infect adjacent nodes
  V(g)[infect_adja]$infected <- T
  V(g)[infect_adja]$color <- "#d95f02"
  
  # Recover infected nodes
  ## Infected nodes have a probability of infected days/20 to recover
  infectedNodes <- V(g)[infected]
  propRecover <- infectedNodes$counter/20
  rollDice <- runif(length(infectedNodes))
  # Update recovered nodes
  V(g)[infectedNodes]$recovered <- rollDice < propRecover
  V(g)[recovered]$infected <- F
  V(g)[recovered]$color <- "#1b9e77"
  
  g
}

# Process time increments and model the infection
createTimeline <- function(g, t, prob.infect){
  timedNetworks <- list(g)
  
  for( x in 2:t){
    timedNetworks[[x]] <- nextTurn(timedNetworks[[x-1]], prob.infect)
  }
  timedNetworks
}

# Turn time series into gif
animate_system <- function(g_list, main, filepath){
  animation::saveGIF({
    lapply(1:length(g_list), function(i){
      set.seed(4321); plot(g_list[[i]],
                           main = main, 
                           sub = paste('Turn:', i - 1), 
                           vertex.label = '', 
                           vertex.size = 3)
    })
  },
  movie.name = filepath,
  ani.width = 600,
  ani.height = 600,
  interval = 1
  )
}

# Generate stats on SIR components
getStats <- function(gCollection){
  gCollection %>%
    lapply(function(x){
      infected <- V(x)$infected %>% sum
      recovered <- V(x)$recovered %>% sum
      susceptible <- vcount(x) - infected - recovered
      
      data.frame(infected, recovered, susceptible)
    }) %>%
    do.call('rbind', .) %>%
    dplyr::mutate(time = 1:nrow(.)) %>%
    tidyr::gather(type, value, -time)
}

n <- 1000
ed <- n * 4
prob.infect <- .1
gmma <- 14

#################
## Initialize graph networks
#################

# Erdos-Renyi network: constant probability to connect nodes
set.seed(4321); rn <- sample_gnm(n, ed, directed = F) %>%
  initG(1)

# Scale free network
set.seed(4321); sfree <- sample_fitness_pl(n, ed, 2.2) %>%
  initG(5)

# Small world network
set.seed(4321); sw <-  sample_smallworld(1, n, 4, .1) %>%
  initG(1)

set.seed(4321); plot(sw, vertex.label = '', vertex.size = 3)

#################
## Progress models through time steps
#################

set.seed(4321)
test1 <- createTimeline(sfree, 30, prob.infect)
test0 <- createTimeline(rn, 30, prob.infect)
test2 <- createTimeline(sw, 30, prob.infect)

set.seed(4321); plot(test0[[10]], vertex.label = '', vertex.size = 5)

#################
## Generate data files and plots
#################

# Plot snapshot of final state
set.seed(4321); plot(test1[[30]], vertex.label = '', vertex.size = 3)
set.seed(4321); plot(test0[[15]], vertex.label = '', vertex.size = 3)
set.seed(4321); plot(test2[[15]], vertex.label = '', vertex.size = 3)

#Windows permission issue, cannot save to other folder for some reason
#need to save in current directory and manually move to images
# Generate gifs
animate_system(test1, 
               paste0("Scale Free Network of size ~", n), 
               '20200603_ScaleFree_1000.gif')

animate_system(test0, 
               paste0("Random Network of size ~", n), 
               '20200603_Random_1000.gif')

animate_system(test2, 
               paste0("Small World Network of size ~", n), 
               '20200603_SmallWorld_1000.gif')

# Save timeline as data files
readr::write_rds(test1, "../Data/20200603_ScaleFree_1000_1-15.rds")
readr::write_rds(test0, "../Data/20200603_Random_1000_1-15.rds")
readr::write_rds(test2, "../Data/20200603_SmallWorld_1000_1-15.rds")

# Generate stat blocks of each network
stats1 <- getStats(test1)
stats0 <- getStats(test0)
stats2 <- getStats(test2)

stats1$model <- 'scale free'
stats0$model <- 'random'
stats2$model <- 'small world'

# Plot stats
library(ggplot2)
ggplot(rbind(stats1, stats0, stats2)) +
  geom_line(aes(time, value, color = type), size = 1.5) +
  facet_wrap(~model) +
  theme_bw() +
  labs(title = "SIR Distribution") +
  scale_color_brewer(type = 'qual')

ggsave('../Images/2000603_SIR_Distro.pdf')

