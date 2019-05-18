require(igraph)
require(tidyverse)

# ref: 
#   - Finding communities in networks with R and igraph (https://www.sixhat.net/finding-communities-in-networks-with-r-and-igraph.html)
#   - Community strucure via short random walks(https://igraph.org/r/doc/cluster_walktrap.html)

###############
#  configure
steps <- 10
type_dependency = 'latest'

###############
test <- readRDS("dependency_graph.RDS") %>% filter(type_dependency==type_dependency)

# df <- readRDS("dependency_graph.RDS") %>% filter(type_dependency==type_dependency)

df_imports <- df %>% filter(neibor_type=="imports")
df_suggests <- df %>% filter(neibor_type=="suggests")

graph_imports <- graph.data.frame(df_imports, directed = TRUE)
graph_suggests <- graph.data.frame(df_suggests, directed = TRUE)

#TODO: wierd elements
plot(graph_imports)

# TODO: suggests
community <- cluster_walktrap(graph_imports,steps = steps)
member <- tibble(
    pkg_name=community$name,
    membership=community$membership,
    modularity= community$modularity
)

df_res <- member %>% 
    group_by(membership) %>% 
    summarise(repr=paste(pkg_name, collapse=', '))

df_res
View(df_res)

#TODO: how to determine number of groups
#TODO: data structure?