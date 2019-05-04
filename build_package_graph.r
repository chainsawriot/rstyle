#import libraries
library(desc) 
library(stringr) 
library(igraph)

#get import/suggest from description
desc <- description$new(package = "ggplot2")
Imports_li <- desc$get("Imports")
Suggests_li <-desc$get("Suggests")

#clean import list, make it as df
#TODO add suggest list
li<-strsplit(Imports_li, ",") 
package_relate<-str_extract_all(li, "[a-z0-9]{2,}") 
package_relate_li<-(strsplit(package_relate[[1]], "\'"))
df <- data.frame("id"=numeric(), "source"=character(), "dest"=character(), "weight"=numeric(),"depth"=numeric())
for (i in package_relate_li) { 
  new_row <- data.frame("source"="ggplot2", "dest"=toString(i), "weight"=1,"depth"=1)
  df<-rbind(df, new_row) 
}
df

#plot as depth 1 graph
import_plot<-graph.data.frame(df,directed = TRUE)
plot(import_plot)


#for 2 to 10 depth
for (i in 2:10){
  search_target<- df[which(df$depth == (i-1)),]['dest']
  for (target in search_target[[1]]){
    tryCatch({
      # since some package can't get their description page,
      # here we use trycatch
      desc <- description$new(package = toString(target))
      imports_li <- desc$get("Imports")
      li<-strsplit(imports_li, ",") 
      package_relate<-str_extract_all(li, "[a-z0-9]{2,}") 
      package_relate_li<-(strsplit(package_relate[[1]], "\'"))
      
      for (dest in package_relate_li) { 
        new_row <- data.frame("source"=toString(target), "dest"=toString(dest), "weight"=1,"depth"=i)
        #TODO if "no NA" and if "no duplicated source dest", then rbind
        if (new_row['dest']!='NA'){
          df<-rbind(df, new_row) 
        }
        
      }
    }, error = function(err) {
      print(toString(target))
      print("can not found")
    })
  }
}
df
import_plot<-graph.data.frame(df,directed = TRUE)
plot(import_plot)

# community walktrap
communities<-cluster_walktrap(import_plot)
communities