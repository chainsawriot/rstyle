require(tidyverse)
require(igraph)
pkgs <- readRDS("pkgs_functions_with_syntax_feature.RDS")
comm <- readRDS("cran_community_20190518.RDS")
graph <- readRDS('cran_graph.RDS')

V(graph)$comm <- membership(comm)
pkgs_to_extract <- names(membership(comm)[membership(comm) %in% c(36, 35, 18, 25)])
cran_sub_graph <- induced_subgraph(graph, which(V(graph)$name %in% pkgs_to_extract))

set.seed(168979208)
plot(cran_sub_graph, vertex.color = V(cran_sub_graph)$comm, vertex.size = page_rank(cran_sub_graph)$vector * 200, 
     edge.arrow.size = 0, vertex.label = ifelse(V(cran_sub_graph)$name %in% c("randomForest", "sna", "tm", "rJava"), V(cran_sub_graph)$name, ""), 
     vertex.label.cex = 3, edge.color = "gray80", vertex.frame.color = "gray80", vertex.label.color = "#000000")

tibble(pkg_name = V(cran_sub_graph)$name, var = page.rank(cran_sub_graph)$vector * 100, comm = V(cran_sub_graph)$comm) %>% group_by(comm) %>% top_n(n = 1, wt = var)
