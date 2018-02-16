library(jsonlite)
library(dplyr)
library(tidyr)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(ggplot2)

rnorm(100)
qnorm()
pnorm()

#downloading the public NASA metadata as a JSON file
#metadata <- fromJSON("https://data.nasa.gov/data.json")
#saveRDS(metadata, file = "NASA_metadata.RDS")

setwd("c:/University/Second Year/Founders Network/Big Data Educational/")

metadata <- readRDS("data/NASA_metadata.RDS")

#displaying the names NASA stores their data under
names(metadata$dataset)

#creating seperate data frames for the title, description, and keyword but keeping the id's to match later
nasa_title <- data_frame(id = metadata$dataset$`_id`$`$oid`, title = metadata$dataset$title)
nasa_title

nasa_desc <- data_frame(id = metadata$dataset$`_id`$`$oid`, desc = metadata$dataset$description)
nasa_desc %>% select(desc) %>% sample_n(5)

nasa_keyword <- data_frame(id = metadata$dataset$`_id`$`$oid`, keyword = metadata$dataset$keyword) %>%
unnest(keyword)
nasa_keyword

#formating the data into the tidytext format
nasa_title <- nasa_title %>% unnest_tokens(word, title) %>% anti_join(stop_words)
nasa_desc <- nasa_desc %>% unnest_tokens(word, desc) %>%  anti_join(stop_words)

#seeing which words are the most frequent
nasa_title %>%  count(word, sort = TRUE)
nasa_desc %>%   count(word, sort = TRUE)


#filtering the data
my_stopwords <- data_frame(word = c(as.character(1:10), "v1", "v03", "l2", "l3", "l4", "v5.2.0", "v003", "v004", "v005", "v006", "v7"))
nasa_title <- nasa_title %>% anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% anti_join(my_stopwords)

#checking to see their common words
nasa_keyword %>% group_by(keyword) %>% count(sort = TRUE)

#getting rid of duplicate words
nasa_keyword <- nasa_keyword %>% mutate(keyword = toupper(keyword))

#analysiing which words appear together
title_word_pairs <- nasa_title %>% pairwise_count(word, id, sort = TRUE, upper = FALSE)
title_word_pairs

#wait this wouldnt work, why not?
#desc_word_pairs <- nasa_desc %>% pairwise_count(word, id, sort = TRUE, upper = FALSE)
#desc_word_pairs

keyword_pairs <- nasa_keyword %>%  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)
keyword_pairs

#plotting the paired title words and how strong their connection to each other is
set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +theme_void()
readRDS("NASA_title_words_connections_plot.RDS")

#doing the same now for key words
set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) + theme_void()
readRDS("NASA_keyword_connections_plot.RDS")


#finding the correlation between key words
keyword_cors <- nasa_keyword %>% group_by(keyword) %>%filter(n() >= 50) %>% pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)
keyword_cors

set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) + theme_void()
