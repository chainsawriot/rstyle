library(dplyr) # Data wrangling & manipulation
library(tidytext) # For unnest_tokens, stop_words
library(stringr) # For managing text
library(ggplot2) # For data visualizations & graphs
library(stringr) # clean text

target_meta <- readRDS("target_meta.RDS")
final_meta <- readRDS("final_meta.RDS")

# get latest record of a library
# --> how does the latest description look like
latest_meta <- (target_meta %>% group_by(pkg_name) %>% slice(which.max(year)))

# get visualization of each year key words
meta_2019 <- (target_meta %>% group_by(pkg_name) %>% filter(year == "2019"))
dataList=list(meta_2019[5])
desc_2019 <- ""
for(i in dataList){
  desc_2019 <- paste(desc_2019, i, sep="")
}

# clean text
temp_desc_2019 <- str_replace_all(desc_2019,"[^a-zA-Z\\s]", " ")
temp_desc_2019 <- str_replace_all(temp_desc_2019,"[\\s]+", " ")

# remove the stop_words
data(stop_words)
text_df <- data_frame(Text = temp_desc_2019) 
text_words <- text_df %>% unnest_tokens(output = word, input = Text) 
text_words  <- text_words  %>% anti_join(stop_words) # Remove stop words in temp_desc_2019
text_wordcounts <- text_words  %>% count(word, sort = TRUE)
text_wordcounts

