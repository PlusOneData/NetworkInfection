library(grid)
library(gridGraphics)
library(igraph)
library(ggplot2)
library(animation)


drawSIRLines <- function(dta){
  ggplot(dta) +
    geom_line(aes(time, value, color = type), size = 1.5) +
    theme_bw()+
    scale_x_continuous(limits = c(0, 30)) +
    scale_color_manual(values = c(infected = "#d95f02",
                                  recovered = "#1b9e77",
                                  susceptible = "#7570b3")) +
    labs(color = "Population Status",
         x = "Time",
         y = "Population")
  
}

drawSIRLines(dplyr::arrange(stats2, time) %>% head(12))
dplyr::filter(stats2, time <= 1)

gridAnimPrep <- purrr::map(seq_along(test2), function(i){
  print(i)
  
  set.seed(4321); plot(test2[[i]],
       vertex.label = '', 
       vertex.size = 3,
       asp = 0)
  
  grid.echo()
  a <- grid.grab()
  
  b <- drawSIRLines(dplyr::filter(stats2, time <= i))
  b <- ggplotGrob(b)
  
  grid.newpage()
  # grid.draw(textGrob(paste0("T = ", i), x = .5, y = .2))
  pushViewport(viewport(x = .75, y = .7, width = .6, height = .6))
  grid.draw(a)
  popViewport()
  pushViewport(viewport(x = .25, y = .7, width = .5, height = .5))
  grid.draw(b)
  popViewport()
  finalChart <- grid.grab()
  # grid.draw(finalChart)
})

saveGIF({
  ani.options(ani.width = 1000,
              ani.width = 1000,
              interval = .4)

  purrr::map(seq_along(gridAnimPrep), function(i){
    grid.newpage()
    grid.draw(gridAnimPrep[[i]])
  })
}, movie.name = '20200605_small_world_upper.gif')
