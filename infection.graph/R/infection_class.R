default_infect <- function(i, r) {
  infection = list(init_num=i, rate=r)
  class(infection) = "infection"
}

init.default_infect <- function(g) {
  igraph::V(g)$infected <- c(rep(T, n), rep(F, igraph::vcount(g)-n)) %>%
    sample() # reorders the arrangement of infected nodes
  igraph::V(g)$color <- ifelse(igraph::V(g)$infected, 'red', 'blue')
}

next.default_infect <- function(g) {
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
}
