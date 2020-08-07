require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)

source('helpers.R')

test <- readRDS('pkgs_functions_with_syntax_feature.RDS')

map(map(test$function_feat, 'result'))
    
map(test$function_feat, "result")

summarise_at(test$function_feat[[1]]$result, vars(fx_assign:fx_tab), ~(sum(.) / length(.)))

fx_cal <- function(x) {
    summarise_at(x, vars(fx_assign:fx_tab), ~(sum(.) / length(.)))
}

test$function_feat[[1]]$result %>% fx_cal

count <- function(x) {
    !is.null(nrow(x))
}


test %>% mutate(res_good = map_lgl(map(function_feat, 'result'), count)) %>% filter(res_good) -> test_meat
summ_test <- map(test_meat$function_feat, "result") %>% map_dfr(fx_cal)

require(corrplot)
summ_test %>% cor %>% corrplot( method = "shade", type = "upper")
