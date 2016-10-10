# data comes from proba_neg_sentiment.r

# Author: Conor McDonald
# Contact: @conormacd
# Aim: Determine if sentiment used by the candidates shifted from debate 1

library(tidytext)
library(tidyverse)
library(stringr)
library(broom)
library(ggrepel)

# load data / fix column names

debate_2 <- read_csv("/Users/conormcdonald/Desktop/debate_second.csv")
names(debate_2) <- tolower(names(df))
debate_2$date

debate_2 <- debate_2 %>% filter(date == "2016-10-09")

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

candidates <- neg_lines_2 %>% filter(speaker %in% c("Trump", "Clinton"))
candidates$speaker <- candidates$speaker %>% str_c("_2")

trump_b0 <- eb$beta1[eb$speaker == "Trump"]
trump_a0 <- eb$alpha1[eb$speaker == "Trump"]

hillary_b0 <- eb$beta1[eb$speaker == "Clinton"]
hillary_a0 <- eb$alpha1[eb$speaker == "Clinton"]

priors <- data.frame(alpha0 = c(hillary_a0, trump_a0), 
                     beta0 = c(hillary_b0, trump_b0), 
                     speaker = c("Clinton_2", "Trump_2"), 
                     stringsAsFactors = FALSE) %>% 
  inner_join(candidates)

eb_1 <- eb %>% filter(speaker != "Holt")
eb_1$speaker <- eb_1$speaker %>% str_c("_1")

# Empirical Bayes 

eb_2 <- priors %>% 
  mutate(eb_est = (neg + alpha0) / (lines + alpha0 + beta0)) %>% # eb = empirical bayes estimate
  mutate(alpha1 = neg + alpha0, beta1 = lines - neg + beta0) %>% 
  mutate(low  = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

eb_2 <- eb_2 %>% 
  bind_rows(eb_1) %>% 
  select(-alpha0, -beta0)

eb_pdf_2 <- eb_2 %>% 
  inflate(x = seq(.10, 1., .0025)) %>%
  mutate(density = dbeta(x, alpha1, beta1))

new_names = c("Clinton_1" = "Clinton - Debate #1", 
              "Clinton_2" = "Clinton - Debate #2", 
              "Trump_1" = "Trump - Debate #1", 
              "Trump_2" = "Trump - Debate #2")

eb_pdf_2$speaker_name <- new_names[eb_pdf_2$speaker] %>% unname()

ggplot(eb_pdf_2, aes(x, density, color = speaker_name)) + 
  geom_line(size = 1, alpha = 0.7) +
  labs(x = "Negativity estimate", 
       y = "Probability density", 
       colour = "Speaker", 
       title = "Probablistic estimate of using negative sentiment") +
  theme_bw() +
  theme(axis.title.x=element_text(margin=margin(t=15))) +
  theme(plot.title = element_text(hjust=0)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1))















