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

#plot as graph
import_plot<-graph.data.frame(df,directed = TRUE)
plot(import_plot)

#___把search_target變成可iterator的物件
search_target<- (df[which(df$depth == 1),]['dest'])
len<-length(search_target[[1]])
for (i in 1:len){
  print(toString(search_target[[1]][i]))}


#for 2 to 10 depth
for (i in 2:10){
  search_target<- df[which(df$depth == (i-1)),]['dest']
  for (target in search_target[[1]]){
    tryCatch({
      desc <- description$new(package = toString(target))
      imports_li <- desc$get("Imports")
      li<-strsplit(imports_li, ",") 
      package_relate<-str_extract_all(li, "[a-z0-9]{2,}") 
      package_relate_li<-(strsplit(package_relate[[1]], "\'"))
      
      for (dest in package_relate_li) { 
        new_row <- data.frame("source"=toString(target), "dest"=toString(dest), "weight"=1,"depth"=i)
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