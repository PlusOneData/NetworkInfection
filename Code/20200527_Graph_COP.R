library(igraph)
?random.graph.game

keepLargeComponent <- function(g){
  lrgComp <- g %>%
    components() %>%
    {
      comp_id <- which.max(.$csize);
      comp_v <- which(.$membership == comp_id)
      comp_v
    }
  
  g <- g - V(g)[!V(g) %in% lrgComp]
  
  return(g)
}

initG <- function(g, n, onlyLarge = T){
  if(onlyLarge){
    g <- keepLargeComponent(g)
  }
  
  V(g)$infected <- c(rep(T, n), rep(F, vcount(g)-n)) %>%
    sample()
  V(g)$color <- ifelse(V(g)$infected, 'red', 'blue')
  V(g)$counter <- 0
  V(g)$recovered <- F
  
  g
}

n <- 1000
ed <- n * 4
prob.infect <- .3
gmma <- 14


set.seed(4321); rn <- sample_gnm(n, ed, directed = F) %>%
  initG(1)

set.seed(4321); sfree <- sample_fitness_pl(n, ed, 2.2) %>%
  initG(5)

set.seed(4321); sw <-  sample_smallworld(1, n, 4, .1) %>%
  initG(1)

set.seed(4321); plot(sw, vertex.label = '', vertex.size = 3)

nextTurn <- function(g, prob.infect){
   
  V(g)[infected]$counter = V(g)[infected]$counter + 1
  
  infect_adja <- g %>%
    ego(nodes = V(.)[infected], mindist = 1) %>%
    unlist() %>%
    unique() %>%
    {V(g)[.][!infected & !recovered]} %>%
    {
      l <- length(.)
      bool <- runif(l) <= prob.infect
      .[bool]
    }
  
  V(g)[infect_adja]$infected <- T
  V(g)[infect_adja]$color <- "red"
  
  infectedNodes <- V(g)[infected]
  propRecover <- infectedNodes$counter/20
  rollDice <- runif(length(infectedNodes))
  V(g)[infectedNodes]$recovered <- rollDice < propRecover
  V(g)[recovered]$infected <- F
  V(g)[recovered]$color <- "green"
  
  g
}


createTimeline <- function(g, t, prob.infect){
  timedNetworks <- list(g)
  
  for( x in 2:t){
    timedNetworks[[x]] <- nextTurn(timedNetworks[[x-1]], prob.infect)
  }
  timedNetworks
}

set.seed(4321)
test1 <- createTimeline(sfree, 30, .3)
test0 <- createTimeline(rn, 30, .3)
test2 <- createTimeline(sw, 30, .3)

# Scale free has multiple components; reduce to just he largest
set.seed(4321); plot(test0[[10]], vertex.label = '', vertex.size = 5)

lrg_comp_1 <- test1[[1]] %>%
  components() %>%
  {
    comp_id <- which.max(.$csize);
    comp_v <- which(.$membership == comp_id)
    comp_v
  }


for(x in 1:length(test1)){
  test1[[x]] <- test1[[x]] %>%
    {. - V(.)[!V(.) %in% lrg_comp_1]} 
}


set.seed(4321); plot(test1[[30]], vertex.label = '', vertex.size = 3)
set.seed(4321); plot(test0[[15]], vertex.label = '', vertex.size = 3)
set.seed(4321); plot(test2[[15]], vertex.label = '', vertex.size = 3)

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

#Windows permission issue, cannot save to other folder for some reason
#need to save in current directory and manually move to images
animate_system(test1, 
               paste0("Scale Free Network of size ~", n), 
               '20200603_ScaleFree_1000.gif')

animate_system(test0, 
               paste0("Random Network of size ~", n), 
               '20200603_Random_1000.gif')

animate_system(test2, 
               paste0("Small World Network of size ~", n), 
               '20200603_SmallWorld_1000.gif')

readr::write_rds(test1, "../Data/20200603_ScaleFree_1000_1-15.rds")
readr::write_rds(test0, "../Data/20200603_Random_1000_1-15.rds")
readr::write_rds(test2, "../Data/20200603_SmallWorld_1000_1-15.rds")

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

stats1 <- getStats(test1)
stats0 <- getStats(test0)
stats2 <- getStats(test2)

stats1$model <- 'scale free'
stats0$model <- 'random'
stats2$model <- 'small world'

library(ggplot2)
ggplot(rbind(stats1, stats0, stats2)) +
  geom_line(aes(time, value, color = type), size = 1.5) +
  facet_wrap(~model) +
  theme_bw() +
  labs(title = "SIR Distribution") +
  scale_color_brewer(type = 'qual')

ggsave('../Images/2000603_SIR_Distro.pdf')

ggplot(stats0) +
  geom_line(aes(time, value, color = type))
