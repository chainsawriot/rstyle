library(desc) 
library(stringr) 
library(igraph)
desc <- description$new(package = "ggplot2")
Imports_li <- desc$get("Imports")
Suggests_li <-desc$get("Suggests")
li<-strsplit(Imports_li, ",") 
package_relate<-str_extract_all(li, "[a-z0-9]{2,}") 
package_relate_li<-(strsplit(package_relate[[1]], "\'"))
df <- data.frame("id"=numeric(), "source"=character(), "dest"=character(), "weight"=numeric(),"depth"=numeric())
count=0
for (i in package_relate_li) { 
  new_row <- data.frame("source"="ggplot2", "dest"=toString(i), "weight"=1,"depth"=1)
  df<-rbind(df, new_row) 
}
df 