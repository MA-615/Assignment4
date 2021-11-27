#Assignment 4 Part Two:
#As shown in Chapter 2 of Text Mining with R do a sentiment display through the 
#narrative of your book. You should choose an index length and a sentiment 
#dictionary that gives you the best fit between the plotline of the book and 
#the graph that you create. If you don’t know the plotline of the book, 
#you will need to either skim or refer to a synopsis.

pacman::p_load(gutenbergr, dplyr, stringr, tidyverse, tidytext, tidyr, scales)

sherlock <- gutenberg_download(1661)

tidy_sherlock <- sherlock %>%
  unnest_tokens(word, text) %>% anti_join(stop_words)
tidy_sherlock %>% count(word, sort = TRUE)

tidy_sherlock %>% count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + 
  geom_col() + 
  labs(y = NULL)

frequency <- bind_rows(mutate(tidy_sherlock, author = "Arthur Conan Doyle")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n /sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = author, values_from = proportion)

#Sentiment Analysis with tidy data

get_sentiments("nrc")

#sentiments: negative, positive, anger, anticipation, disgust, fear, joy, sadness, surprise, trust
nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

nrc_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

negative <- tidy_sherlock %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)
positive <- tidy_sherlock %>%
  inner_join(nrc_positive) %>%
  count(word, sort = TRUE)
sherlock_sent <- tidy_sherlock %>% 
  inner_join(get_sentiments("bing")) %>% 
               count(index = linenumber %/% 80, sentiment) %>%
               pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
               mutate(sentiment = positive - negative)
                                        

