
library(stringr)
library(dplyr)

#main dataframe
wines_df <- read.csv('data/wine_revs.csv', stringsAsFactors = F)

wines_df$keywords = strsplit(wines_df$keywords, ',')

#common word list (for keywords)
common_words <- unlist(str_split(readLines("data/common_words_and_bigrams.txt"), pattern = ", "))

common_words = as.list(common_words)

#function to filter out keywords not in common words list 
common_word_filter <- function(x){
  lst = as.list(x)[[1]]
  for (i in (1:length(lst))){
    ifelse(lst[[i]] %in% common_words, return(lst[[i]]), return(''))
  }
}

#function applied to wines_df dataframe 
wines_df$keywords = sapply(wines_df$keywords, common_word_filter)


#df's for maps 

#world: 
wines_df_wpc <- as.data.frame(wines_df %>% group_by(country) %>% filter(!country %in% c('US', 'Italy', 'France') ) %>%  summarise(cnt = n())) 

wines_df_nv <- as.data.frame(wines_df %>% group_by(country) %>% summarise(cnt = n_distinct(variety)))   

wines_df_apr <- as.data.frame(wines_df %>% group_by(country) %>% summarise(avg_pr = mean(price))) 

wines_df_apt <- as.data.frame(wines_df %>% group_by(country) %>% summarise(avg_pt = mean(points))) 


#US 
wines_df_us <- wines_df %>% filter(country == 'US') 

wines_df_wps <- as.data.frame(wines_df_us %>%  group_by(province) %>% summarise(cnt = n())) 

wines_df_nvs <- as.data.frame(wines_df_us %>% group_by(province) %>% summarise(cnt = n_distinct(variety)))   

wines_df_aps <- as.data.frame(wines_df_us %>% group_by(province) %>% summarise(avg_pr = mean(price))) 

wines_df_apts <- as.data.frame(wines_df_us %>% group_by(province) %>% summarise(avg_pt = mean(points))) 

#df's for graphs: 
#keep only countries with more than 1 wine in dataset in the interest of meaningful statistics 
wines_df_filtered <- wines_df %>% group_by(country) %>% summarise(cnt = n()) %>% filter(cnt > 2)

wines_df_country_graph <- semi_join(wines_df, wines_df_filtered, by = 'country')

wines_df_us_filtered <- wines_df %>% filter(country == 'US', province != 'America') %>% group_by(province) %>% summarise(cnt = n()) %>% filter(cnt > 2)
  
wines_df_us_graph <- semi_join(wines_df, wines_df_us_filtered, by = 'province')
  
#price and points stats for entire dataset 
wines_df_world_stats <- wines_df %>%summarise(max_price = max(price), mean_price = round(mean(price)), min_price = min(price), max_points = max(points), mean_points = round(mean(points)), min_points = min(points), corr = round(cor(price, points, method = 'pearson'), digits = 2), red_perc = round((sum(color == 'red')/n()) * 100, digits = 2), white_perc =  round((sum(color == 'white')/n()) * 100, digits = 2)) 

wines_df_price_filtered = wines_df[wines_df$price < 300,]


#statistical tests on red vs. white wines s
t.test(price ~ color, data = wines_df_price_filtered, alternative = 'two.sided')

t.test(points ~ color, data = wines_df_price_filtered, alternative = 'two.sided')

var.test(price~color, wines_df_price_filtered, alternative = 'two.sided')

