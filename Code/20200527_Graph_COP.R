library(igraph)
library(infection.graph)

n <- 1000
ed <- n * 4
prob.infect <- .1
gmma <- 14

covid_di <- default_infect(init_num = 1, rate = prob.infect)
covid_dr <- default_recover(max_recovery_time = 20)
covid_model <- infection_model(components = list(covid_di, covid_dr))

flu_di <- default_infect(init_num = 7, rate = 0.01)
flu_dr <- default_recover(max_recovery_time = 7)
flu_model <- infection_model(components = list(flu_di, flu_dr))

#################
## Initialize graph networks
#################

# Erdos-Renyi network: constant probability to connect nodes
set.seed(4321); rn <- sample_gnm(n, ed, directed = F) %>%
  covid_model$init_model()

# Scale free network
set.seed(4321); sfree <- sample_fitness_pl(n, ed, 2.2) %>%
  covid_model$init_model()

# Small world network
set.seed(4321); sw <-  sample_smallworld(1, n, 4, .1) %>%
  covid_model$init_model()

set.seed(4321); plot(sw, vertex.label = '', vertex.size = 3)

#################
## Progress models through time steps
#################

set.seed(4321)
test0 <- createTimeline(rn, 30, covid_model)
test1 <- createTimeline(sfree, 30, covid_model)
test2 <- createTimeline(sw, 60, covid_model)

#################
## Generate data files and plots
#################

# Plot snapshot of final state
set.seed(4321); plot(test0[[30]], vertex.label = '', vertex.size = 3)
set.seed(4321); plot(test1[[30]], vertex.label = '', vertex.size = 3)
set.seed(4321); plot(test2[[60]], vertex.label = '', vertex.size = 3)

#Windows permission issue, cannot save to other folder for some reason
#need to save in current directory and manually move to images
# Generate gifs
animate_system(test0, 
               paste0("Random Network of size ~", n), 
               '20200603_Random_1000.gif')

animate_system(test1, 
               paste0("Scale Free Network of size ~", n), 
               '20200603_ScaleFree_1000.gif')

animate_system(test2, 
               paste0("Small World Network of size ~", n), 
               '20200603_SmallWorld_1000.gif')

# Save timeline as data files
readr::write_rds(test0, "../Data/20200603_Random_1000_1-15.rds")
readr::write_rds(test1, "../Data/20200603_ScaleFree_1000_1-15.rds")
readr::write_rds(test2, "../Data/20200603_SmallWorld_1000_1-15.rds")

# Generate stat blocks of each network
stats0 <- getStats(test0)
stats1 <- getStats(test1)
stats2 <- getStats(test2)

stats0$model <- 'random'
stats1$model <- 'scale free'
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

