# Centrality Module
# Goals: Generate centrality measures for graph nodes
# ---- basic components
# return degree centrality
# return closeness centrality
# return betweenness centrality

# ---- additional items
# potentially add in eigenvector centrality if that is of use

#' Centrality Class
#'
#' A default class to generate centrality measures.
#' Each of the \code{init_num} nodes has measures of centrality that are useful
#' to know in order to identify individuals with high connectivity within the network.
#'
#' @field degree_centrality Number of edges adjacent to node
#' @field closeness_centrality The reciprocal of the sum of length of shortest paths between
#' all the other nodes in the graph.For nodes with no edges, total number of vertices is used
#' instead of path length 
#' @field betweenness_centrality Number of these shortest paths that pass through the vertex
#' 
#' @export centrality
centrality <- setRefClass(
  "centrality",
  fields = list(
    degree_centrality="numeric",
    closeness_centrality="numeric",
    betweenness_centrality="numeric"
  ),
  methods = list(
    init = function(g) {
      "Set initial measures"
      igraph::V(g)$degree_centrality <- 0
      igraph::V(g)$closeness_centrality <- 0
      igraph::V(g)$betweenness_centrality <- 0
      return(g)
    },
    donext = function(g) {

      "set the degree centrality of the node"
      igraph::V(g)$degree_centrality <-  igraph::degree(g)
      igraph::V(g)$closeness_centrality <- igraph::closeness(g, mode = ("all"))
      igraph::V(g)$betweenness_centrality <- igraph::betweenness(g, directed = FALSE)
      return(g)
    }
  )
)
