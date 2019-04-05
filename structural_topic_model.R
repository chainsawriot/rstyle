###install.packages('ctv') ### packed in Dockerfile
require(ctv)
require(tidyverse)
require(stm)

# cran_views <- available.views()
# saveRDS(cran_views, 'cran_view_mar_2019.RDS')

cran_views <- readRDS('cran_view_mar_2019.RDS')

meta <- readRDS('target_meta.RDS') %>% group_by(pkg_name) %>% top_n(1, year) %>% ungroup

for (i in cran_views) {
    meta <- meta %>% mutate(!!(sym(tolower(i$name))) := pkg_name %in% i$packagelist$name)
}

require(stm)
require(purrr)

processed <- textProcessor(meta$desc, metadata = meta)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

map_chr(cran_views, "name") %>% tolower %>% paste(collapse = "+") %>% paste("~ ", .) %>% as.formula() -> prev_formula

find_k <- manyTopics(out$documents,out$vocab, K=c(20, 40, 60, 80, 100), prevalence = prev_formula, max.em.its=100, run = 20, data=out$meta, seed=46709394, emtol = 1e-04)
saveRDS(find_k, "stm_model.RDS")

find_k <- readRDS('stm_model.RDS')
as.formula(map_chr(cran_views, "name") %>% tolower %>% paste(collapse = "+") %>% paste("1:40~ ", .)) -> for40
 
prep <- estimateEffect(stmobj = find_k$out[[2]], metadata = out$meta, formula = for40)

stmes_tibble <- function(stmes) {
    stmes[[1]] %>% as_tibble %>% mutate(var_name = row.names(stmes[[1]]))
}