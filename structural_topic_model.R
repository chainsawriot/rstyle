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

