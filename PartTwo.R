#Text Analysis

#######################################
#Part One

#Peter Pan By 
#Download Peter Pan from Gutenberg Project
peter_pan <- gutenberg_download(16)

pacman::p_load(gutenbergr, dplyr, stringr, tidyverse, 
               tidytext, tidyr, scales, ggplot2)

#######################################
#Part Two

#Tidy Peter Pan
tidy_pp <- peter_pan %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
#Begin from first chapter
tidy_pp <- filter(tidy_pp, chapter >=1)


#Sentiment Analysis with tidy data

nrc_negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")
nrc_positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

negative <- tidy_pp %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)
positive <- tidy_pp %>%
  inner_join(nrc_positive) %>%
  count(word, sort = TRUE)

pp_sent <- tidy_pp %>% 
  inner_join(get_sentiments("bing")) %>% 
               count(index = linenumber %/% 100, sentiment) %>%
               pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
               mutate(sentiment = positive - negative)

#Graph of the bing sentiment analysis                                        
ggplot(pp_sent, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 

#Comparing the three different sentiment dictionaries

afinn <- tidy_pp %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 100) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_pp %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_pp %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 100, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
#All three show similar trends through the book

#Look at the ratio of negative words in each chapter
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_pp %>%
  group_by(chapter) %>%
  summarize(words = n())

pp_neg_tibble <- tidy_pp %>%
  semi_join(bingnegative) %>%
  group_by(chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = "chapter") %>%
  mutate(ratio = negativewords/words) %>%
  ungroup()
pp_neg_tibble

#Look at the ratio of positive words in each chapter
bingpositive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

wordcounts1 <- tidy_pp %>%
  group_by(chapter) %>%
  summarize(words = n())

pp_pos_tibble <- tidy_pp %>%
  semi_join(bingpositive) %>%
  group_by(chapter) %>%
  summarize(positivewords = n()) %>%
  left_join(wordcounts1, by = "chapter") %>%
  mutate(ratio = positivewords/words) %>%
  ungroup()
pp_pos_tibble


####################################
#Part Three
