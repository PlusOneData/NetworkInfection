#' Generate infection gif
#'
#' @param g_list A list of graphs representing each time step
#' @param main Title of the animated plot
#' @param filepath Name of the file
#' @export
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

#' Generate stats on SIR components
#'
#' Computes the number of susceptible, infected, and recovered nodes in the graph
#' @param gCollection List of networks at each timestep
#' @export
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
