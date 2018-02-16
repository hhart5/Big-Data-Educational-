library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)


#creating a short example to show tidy text data format
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

#printing our new text in the console
text

#reformating the data into a paragraph
text_df <- data_frame(line = 1:4, text = text)

#transmuting the data into the tidy text format of one word per line, and creating a new column for this 
text_df %>% unnest_tokens(word, text)

#creating the Jane Austen book's as a table we can work with
original_books <- austen_books() %>% group_by(book) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
  ungroup()

#printing the created table
original_books

#reformating the data into the tidy text format
tidy_books <- original_books %>% unnest_tokens(word, text)
tidy_books

#loading in the stop_words" data set to get rid of words that will clutter our analysis
data(stop_words)

#filtering out data to get rid of these words
tidy_books <- tidy_books %>% anti_join(stop_words)

#checking to see which words appear the most in her books
tidy_books %>% count(word, sort = TRUE) 

#replotting the most frequently used words as a graph
tidy_books %>% count(word, sort = TRUE) %>% filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()



