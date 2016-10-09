# data comes from neg_proba.r

# Author: Conor McDonald
# Contact: @conormacd
# Aim: Determine if sentiment used by the candidates shifted from debate 1

library(tidytext)
library(tidyverse)
library(stringr)
library(broom)
library(ggrepel)

# load data / fix column names

debate_2 <- read_csv("/Users/conormcdonald/Desktop/second_debate.csv")
names(debate_2) <- tolower(names(df))

# code for getting negative sentiments from debate 2

debate_words_2 <- debate_2 %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

sent_2 <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

sent_line_2 <- debate_words_2 %>% 
  inner_join(sent, by = "word") %>% 
  group_by(speaker, line) %>% 
  summarise(sentiment = mean(afinn_score))

neg_lines_2 <- sent_line_2 %>% 
  group_by(speaker) %>% 
  mutate(lines = n()) %>% 
  filter(sentiment < 0) %>% 
  mutate(neg = n(), 
         prop_neg = neg / lines) %>% 
  ungroup() %>% 
  distinct(speaker, lines, neg, prop_neg)

# establish priors based on use of negative sentiment in previous debate

trump_b0 <- eb$beta1[eb$speaker == "Trump"]
trump_a0 <- eb$alpha1[eb$speaker == "Trump"]

hillary_b0 <- eb$beta1[eb$speaker == "Clinton"]
hillary_a0 <- eb$alpha1[eb$speaker == "Clinton"]

eb_1 <- eb %>% filter(speaker != "Holt")
eb_1$speaker <- eb_1$speaker %>% str_c("_1")


