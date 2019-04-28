library(desc) 
library(stringr) 
library(igraph)
desc <- description$new(package = "ggplot2")
Imports_li <- desc$get("Imports")
Suggests_li <-desc$get("Suggests")
li<-strsplit(Imports_li, ",") 
package_relate<-str_extract_all(li, "[a-z0-9]{2,}") 
package_relate
df <- data.frame("source"=character(), "dest"=character(), "weight"=numeric(),"depth"=numeric())

for (i in package_relate) {
  df$src = rbind(df$src, "ggplot2")
  df$dest = rbind(df$dest, toString(i))
  df$weight = rbind(df$weight, 1)
  df$depth = rbind(df$depth, 1)
}
df

#g2 <- graph.data.frame(x, directed = TRUE)
#plot(g2)