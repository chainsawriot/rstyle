require(tidyverse)
require(igraph)
require(rex)
require(datasets)
require(dplyr)
require(purrr)
require(stringr)
require(iterators)

pkgs <- readRDS("pkgs_functions_with_syntax_feature.RDS")
comm <- readRDS("cran_community_20190518.RDS")

style_regexes <- list(
    "alllowercase"   = rex(start, one_or_more(rex(one_of(lower, digit))), end),
    "ALLUPPERCASE"   = rex(start, one_or_more(rex(one_of(upper, digit))), end),
    "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
    "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
    "snake_case"     = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more("_", one_or_more(rex(one_of(lower, digit)))), end),
    "dotted.case"    = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more(dot, one_or_more(rex(one_of(lower, digit)))), end)
)

conv_style <- function(x, style_regexes) {
    x <- x[!is.na(x) & !is.null(x)]
    styles <- map_chr(x, match_function_style, style_regexes = style_regexes)
}

match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    names(style_regexes)[min(which(res))]
}

get_target_pkgs <- function(x) {
    membership(comm)[membership(comm) == x] %>% names -> target_pkgs
    return(target_pkgs)
}

get_feature_table_from_pkgs <- function(target_pkgs) {
    feature_table <- pkgs %>% filter(pub_year <= 2018) %>% 
        filter(pkg_name %in% target_pkgs) %>% 
        group_by(pkg_name) %>% 
        top_n(1, wt = pub_year) %>% 
        ungroup %>% select(function_feat) %>%
        pull %>% map("result") %>% Filter(Negate(is.null), .) %>%
        map_dfr(function(x) x) %>% 
        summarise_at(vars(fx_assign:fx_tab), sum) %>% t
    return(feature_table)
}

get_naming_feature_table_from_pkgs <-function(target_pkgs){
    naming_features_table <- pkgs %>% filter(pub_year <= 2018) %>% 
        filter(pkg_name %in% target_pkgs) %>% 
        group_by(pkg_name) %>% 
        top_n(1, wt = pub_year) %>% 
        ungroup %>% select(function_feat) %>%
        pull %>% map("result") %>% Filter(Negate(is.null), .) %>%
        map_dfr(function(x) x) %>%
        mutate(styles = map(fx_name, conv_style, style_regexes = style_regexes)) %>%
        summarise(alllower = sum(unlist(styles) == "alllowercase"),
                  allupper = sum(unlist(styles) == "ALLUPPERCASE"),
                  upcamel = sum(unlist(styles) == "UpperCamelCase"),
                  lowcamel = sum(unlist(styles) == "lowerCamelCase"),
                  snake = sum(unlist(styles) == "snake_case"),
                  dotted = sum(unlist(styles) == "dotted.case"),
                  other = sum(unlist(styles) == "other"))
    naming_features_table_df <- data.frame(naming_features_table)
    return(as.data.frame(t(naming_features_table_df)))
}

### build two fx feature tables for all communities
### df_total: counting
### df_ratio_total: ratio

community_ids <- list(15,9,4,60,14,35,1,36,25,39,23,19,31,8,64,73,18,20,120)
#community_ids <- list(20,120)
iter_community_ids <- iter(community_ids)
column_name <- c(
    "Rstudio-related packages","base","image plotting",
    "RCpp","GPS and GEO","ML","public health and Statistics",
    "text analysis","social network analysis",
    "mix of graphics and anomaly detection",
    "graph and its visualization","genetics",
    "finance","insurance and actuary","numerical optimization",
    "sparse matrix","Java","time, date, and money","neuronal science")
# column_name <- c(
#     "time, date, and money","neuronal science")

i <- 0 
while (i < length(community_ids)) {
    target_pkgs <- nextElem(iter_community_ids) %>% get_target_pkgs
    
    feature_table <- target_pkgs %>% 
        get_feature_table_from_pkgs
    
    naming_features_table <- target_pkgs %>% 
        get_naming_feature_table_from_pkgs
    
    if (i==0){
        df_total <- feature_table
        df_ratio_total<- prop.table(feature_table)
        df_naming_total <- naming_features_table
        df_naming_ratio_total<- prop.table(naming_features_table)
    }
    else{
        df_total <- cbind(df_total, feature_table)
        df_ratio_total<- cbind(df_ratio_total, prop.table(feature_table))
        df_naming_total <- cbind(df_naming_total, naming_features_table)
        df_naming_ratio_total<- cbind(df_naming_ratio_total, prop.table(naming_features_table))
    }
    i=i+1 
} 

colnames(df_total) <- column_name
colnames(df_ratio_total) <- column_name
colnames(df_naming_total) <- column_name
colnames(df_naming_ratio_total) <- column_name

View(df_total)
View(df_ratio_total)
View(df_naming_total)
View(df_naming_ratio_total)

df_total %>% saveRDS('community_df_total.RDS')
df_ratio_total %>% saveRDS('community_df_ratio_total.RDS')
df_naming_total %>% saveRDS('community_df_naming_total.RDS')
df_naming_ratio_total %>% saveRDS('community_df_naming_ratio_total.RDS')

###