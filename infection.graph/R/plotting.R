# Turn time series into gif
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

# Generate stats on SIR components
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
