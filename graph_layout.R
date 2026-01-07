#!/usr/bin/env Rscript

historiografia_data <- read.csv("data/grafo.csv", header = TRUE, stringsAsFactors = FALSE)

library(igraph)

g <- graph_from_data_frame(historiografia_data, directed=FALSE)
plot(g, vertex.label.cex=0.7, vertex.size=5, edge.arrow.size=0.5)

V(g)$degree <- degree(g, mode="all")
V(g)$betweenness <- betweenness(g, directed=FALSE)

node_betweenness_df <- data.frame(name=V(g)$name, betweenness=V(g)$betweenness)
top_20_betweenness <- head(node_betweenness_df[order(-node_betweenness_df$betweenness), ], 20)
cat(t(t(top_20_betweenness$name)), sep="\n")


E(g)$edge_betweenness <- edge_betweenness(g)

plot(g, vertex.size=V(g)$betweenness/100, vertex.label.cex=0.7, edge.arrow.size=0.5)

components <- components(g)
main_component <- which.max(components$csize)
vertices_in_main_component <- V(g)[components$membership == main_component]
g_main <- induced_subgraph(g, vertices_in_main_component)

layout <- layout_with_kk(g_main)
V(g_main)$label.cex <- 1+V(g_main)$degree/10
png("figures/graph_layout.png", width=2400, height=1600)
plot(g_main, vertex.size=2+V(g_main)$betweenness/200, vertex.color = rainbow(10, .8, .8, alpha= .8),
     edge.width=1+E(g_main)$edge_betweenness/50,
     layout=layout, alpha = .8)

dev.off()

