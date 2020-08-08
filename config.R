## We'll analyze all packages published in <= INCLUDE_YR
INCLUDE_YR <- 2019

PATH_CODE_DB <- 'code.db'
PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE <- "data/pkgs_functions_with_syntax_feature.RDS"

PATH_TARGET_META <- "target_meta.RDS"

# community-related parameters
MAX_NUM_COMM_TO_ANALYZE <- 20
FOLDER_COMM_OUPUT <- "visualization_community/"
PATH_CRAN_DEPENDENCY <- "data/cran_dependency.RDS"
PATH_CRAN_GRAPH <- "data/cran_graph.RDS"
PATH_COMM <-"data/comm_walktrap.RDS"
PATH_COMM_SIZE <- "data/comm_size.RDS"
PATH_COMM_NAME <- "data/comm_name.csv"
PATH_NAMING_CONVENTION <- "data/naming_convention.csv"
PATH_COMM_LARGEST_FEATURES <- "data/comm_largest_feature.RDS"

## fun0* -related parameters
FX_DATA_PREFIX <- "data/fx_data_yr"
FX_DATA_SUFFIX <- ".RDS"
PATH_FX_STYLE_BY_YEAR <- "data/fx_style_by_year.RDS"

## syntax0* - related parameters
SYNTAX_DATA_PREFIX <- "data/syntax_feature_yr"
SYNTAX_DATA_SUFFIX <- ".RDS"
