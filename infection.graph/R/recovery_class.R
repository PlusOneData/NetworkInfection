default_recover <- function(m) {
  rec <- list(max_recovery_time = m)
}

init.default_recover <- function(g) {
  igraph::V(g)$counter <- 0 # Used to compute recovery time
  igraph::V(g)$recovered <- F
}

next.default_recover <- function(g) {

}
