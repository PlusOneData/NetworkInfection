## Potential Clustering algorithms

## Name
### link
### descriptions
### pros
### cons 

<br>

## Minimum Spanning Tree - Clustering <br>
https://www.cs.cmu.edu/~ckingsf/bioinfo-lectures/mst.pdf (will find better link) <br>
Pros: we can specify K; the highly connected groups are cut off and clustered <br>
Cons: mstknnclust package in R accomplishes this, but I don't think that you can specify K <br>


## cluster_walktrap <br>
https://rdrr.io/cran/igraph/man/cluster_walktrap.html <br>
Pros: returns densely connected clustered groups; we can specify number of walks as well as edgeweights <br>
Cons: again we can't specify K <br>


## shared nearest neighbors clustering algorithm <br>
https://www.rdocumentation.org/packages/dbscan/versions/1.1-6/topics/sNN <br>
Pros: we can specify k; reutnrs  <br>
Cons: input is a matrix, so we would just turn the graph into a matrix <br>
