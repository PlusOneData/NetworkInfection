testing <- setRefClass(
  "testing",
  fields = list(
    init_num="numeric"
  ),
  methods = list(
    init = function(g) {
      igraph::V(g)$tested <- c(rep(T, init_num), rep(F, igraph::vcount(g)-init_num)) %>%
        sample()
      return(g)
    },
    donext = function(g) {
      igraph::V(g)$tested <- c(rep(T, init_num), rep(F, igraph::vcount(g)-init_num)) %>%
        sample()
      return(g)
    }
  )
)

increasing_testing <- setRefClass(
  "inc_testing",
  fields = list(
    init_num="numeric",
    inc_num="numeric"
  ),
  methods = list(
    init = function(g) {
      igraph::V(g)$tested <- c(rep(T, init_num), rep(F, igraph::vcount(g)-init_num)) %>%
        sample()
      return(g)
    },
    donext = function(g) {
      V(g)[sample(V(g)[!tested], inc_num)]$tested<-T
      return(g)
    }
  )
)