historiografia_data <- read.csv("data/grafo.csv", header = TRUE, stringsAsFactors = FALSE)

library(igraph)
g <- graph_from_data_frame(historiografia_data, directed = FALSE)
plot(g, vertex.label.cex=0.7, vertex.size=5, edge.arrow.size=0.5)

V(g)$degree <- degree(g, mode="all")
V(g)$betweenness <- betweenness(g, directed=FALSE)

plot(g, vertex.size=V(g)$betweenness/100, vertex.label.cex=0.7, edge.arrow.size=0.5)

components <- components(g)
main_component <- which.max(components$csize)
vertices_in_main_component <- V(g)[components$membership == main_component]
g_main <- induced_subgraph(g, vertices_in_main_component)

layout <- layout_with_kk(g_main)
plot(g_main, vertex.size=V(g_main)$betweenness/200, vertex.color = rainbow(10, .8, .8, alpha= .8), layout=layout, vertex.label.cex=0.7, edge.arrow.size=0.5, alpha = .8)

png("figures/graph_layout.png", width=2400, height=1600)


