### load libraries needed
packages <- c('igraph','RColorBrewer')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

### igraph basics
# build a graph
graph_01 <- graph(
  edges = c(1,2, 1,3, 1,4, 2,4), n = 4, directed = F
)
# viz it
plot(graph_01)

# edge & vertex
E(graph_01)
V(graph_01)

graph_02 <- graph(
  edges = c(1,1, 1,3, 1,4, 2,1, 2,4, 3,1, 3,4, 4,1, 4,2, 4,3), n = 4, directed = T
)
plot(graph_02)

graph_03 <- graph( c("John", "Jim", "John", "Julia", "Jim", "Jack", "Julia", "Jim", "Jim", "Jack", "John", "John"))  

plot(graph_03) # default plot
plot(graph_03, edge.arrow.size=0.9, edge.curved=0.3,
     vertex.color="pink", vertex.size=20,  vertex.frame.color="red", 
     vertex.label.color="brown", vertex.label.cex=0.8, vertex.label.dist=3) 

# add attributes
V(graph_03)$name
V(graph_03)$gender <- c("male","female","female","male")
vertex_attr(graph_03)

E(graph_03)$type <- c("friend","love")
E(graph_03)$weight <- c(1,2,1,2,1,3)
edge_attr(graph_03)

pal <- RColorBrewer::brewer.pal(length(unique(V(graph_03)$gender)), "Dark2")
plot(graph_03, edge.arrow.size=0.9, edge.curved=0.3,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(graph_03, "gender")))], 
     vertex.size=20, vertex.frame.color="red", 
     vertex.label.color="brown", vertex.label.cex=0.8, vertex.label.dist=3) 

### make graph

# empty graph 
graph_empty <- make_empty_graph(30)
plot(graph_empty, vertex.size=10)

# full graph
graph_full <- make_full_graph(30)
plot(graph_full, vertex.size=10)

# star graph
graph_star <- make_star(30)
plot(graph_star, vertex.size=10) 

# tree graph
graph_tree <- make_tree(30, children = 3)
plot(graph_tree, vertex.size=10) 

# ring graph
graph_ring <- make_ring(30)
plot(graph_ring, vertex.size=10)

### tidygraph
example_nodes <- tibble(name = c("Hadley", "David", "Romain", "Julia", "Mary"))
example_edges <- tibble(from = c(1, 1, 1, 2, 2, 3, 4, 4, 5),
                        to = c(2, 3, 5, 1, 4, 1, 1, 2, 1))
example_graph <- tidygraph::tbl_graph(nodes = example_nodes, edges = example_edges)
example_graph %>%
  ggraph::ggraph(layout = 'nicely') + 
  ggraph::geom_edge_link() + 
  ggraph::geom_node_point(size = 8, colour = 'pink') +
  ggraph::geom_node_text(aes(label = name), colour = 'brown', size = 6)


example_nodes <- tibble(name = c("Hadley", "David", "Romain", "Julia", "Mary", "Bosh", "Taylor", "Lucy", "Brandon", "Roy"))
example_edges <- tibble(from = c(1, 1, 1, 2, 2, 3, 4, 4, 5, 6, 7, 9, 9, 10),
                        to = c(2, 3, 5, 1, 4, 1, 1, 2, 1, 7, 8, 7, 10, 6))
example_graph <- tidygraph::tbl_graph(nodes = example_nodes, edges = example_edges)
example_graph %>%
  ggraph::ggraph(layout = 'nicely') + 
  ggraph::geom_edge_link() + 
  ggraph::geom_node_point(size = 8, colour = 'pink') +
  ggraph::geom_node_text(aes(label = name), colour = 'brown', size = 6)
