library(igraph)

# Get fully connected component of n nodes
get_fcc <-
function(n = 10) {
    m = matrix(1, nrow = n, ncol = n)
    diag(m) = 0
    get.data.frame(graph.adjacency(m, mode = 'undirected'))
}

# Get graph of fully connected components
# links is the number of edges between components
get_full <-
function(N = 10, n = N, links = 1) {
    fccs = lapply(0:(N - 1), function(i) get_fcc(n) + (i * n))
    
    inter_cluster <- 
    do.call(rbind, lapply(fccs, function(.from) {
        do.call(rbind, lapply(fccs, function(.to) {
            data.frame(from = unique(unlist(.from))[1:links], 
                       to = unique(unlist(.to))[1:links])
        }))
    }))
    inter_cluster = unique(inter_cluster[inter_cluster$from < inter_cluster$to, ])
    rbind(do.call(rbind, fccs), inter_cluster)
}

# Randomly remove edges (specified by decay rate)
get_decayed <-
function(N = 10, n = N, links = 1, decay = .95) {
    gdf = get_full(N, n, links)
    gdf[sample(1:nrow(gdf), round(nrow(gdf) * decay)), ]
}

# Conversions
df2m <-
function(x) {
    as.matrix(get.adjacency(graph.data.frame(x, directed = FALSE)))
}

df2g <-
function(x) {
    graph.data.frame(x, directed = FALSE)
}

# i/o
write_graph <-
function(x, file) {
    write.table(x, file = file, sep = ' ', col.names = FALSE, row.names = FALSE)
}
