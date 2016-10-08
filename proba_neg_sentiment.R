library(tidytext)
library(tidyverse)
library(stringr)
library(broom)
library(ggrepel)

df <- read_csv("/Users/conormcdonald/Desktop/debate.csv")
names(df) <- tolower(names(df))
suppressWarnings()
df %>% count(speaker, sort = TRUE)

debate_words <- df %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

sent <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

sent_line <- debate_words %>% 
  inner_join(sent, by = "word") %>% 
  group_by(speaker, line) %>% 
  summarise(sentiment = mean(afinn_score))

sent_line %>% 
  filter(!speaker %in% c("Audience", "Holt")) %>% 
  ggplot(aes(x=line, y=sentiment, colour = speaker)) + geom_line()

neg_lines <- sent_line %>% 
  group_by(speaker) %>% 
  mutate(lines = n()) %>% 
  filter(sentiment < 0) %>% 
  mutate(neg = n(), 
         prop_neg = neg / lines) %>% 
  ungroup() %>% 
  distinct(speaker, lines, neg, prop_neg)

sent_line %>% 
  filter(speaker != "Audience") %>% 
  ggplot(aes(x = line, y = sentiment, fill = speaker)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = pretty(sent_line$line, 16)) +
  labs(x = "Time line", 
       y = "Sentiment per statement", 
       colour = "Speaker", 
       title = "Sentiment during the debate") + 
  theme_minimal() +
  theme(axis.title.x=element_text(margin=margin(t=15))) +
  theme(plot.title = element_text(hjust=0)) 

# eb = empirical bayes
library(MASS)

beta_dis <- fitdistr(neg_lines$prop_neg, dbeta, start = list(shape1 = 1, shape2 = 10))

alpha0 <- beta_dis$estimate[1]
beta0 <- beta_dis$estimate[2]

eb <- neg_lines %>% 
  mutate(eb_est = (neg + alpha0) / (lines + alpha0 + beta0)) %>% # eb = empirical bayes estimate
  mutate(alpha1 = neg + alpha0, beta1 = lines - neg + beta0) %>% 
  mutate(low  = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

eb_pdf <- eb %>% 
  inflate(x = seq(.10, 1., .0025)) %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(eb_pdf, aes(x, density, color = speaker)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  labs(x = "Negativity estimate", 
       y = "Probability density", 
       colour = "Speaker", 
       title = "Probablistic estimate of being mean (negative sentiment)") +
  theme_bw() +
  theme(axis.title.x=element_text(margin=margin(t=15))) +
  theme(plot.title = element_text(hjust=0))
