# Author: Conor McDonald
# Contact: @conormacd

library(tidytext)
library(tidyverse)
library(stringr)
library(topicmodels)
library(broom)

write.csv(stop_words, "stop_words_tidytext.csv")

df <- read_csv("/Users/conormcdonald/Desktop/debate.csv")
names(df) <- tolower(names(df))

debate_words <- df %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

sent_line <- debate_words %>% 
  inner_join(sent, by = "word") %>% 
  group_by(speaker, line) %>% 
  summarise(sentiment = mean(afinn_score))

# df %>% 
#  unnest_tokens(word, text) %>% 
#  anti_join(stop_words)

debate_words_2 <-  debate_words %>% filter(!speaker %in%  c("Holt", "Audience", "CANDIDATES"))

debate_words_2 <- debate_words_2 %>% 
  group_by(speaker) %>% 
  mutate(speaker_line = str_c(speaker, line, sep = "_")) %>% 
  ungroup() %>% 
  select(-speaker, -line, -date)

word_counts <- debate_words_2 %>% 
  count(speaker_line, word, sort = TRUE) %>% 
  ungroup()

dtm <- word_counts %>%
  cast_dtm(speaker_line, word, n)

k_topics <- 5

speaker_lda <- LDA(dtm, k = k_topics, control = list(seed = 1234))
tidy_lda <- tidy(speaker_lda)

top_terms_list <- tidy_lda %>%
  group_by(topic) %>%
  top_n(20, beta) %>% 
  do(data_frame(terms = list(.$term)))

top_terms_list %>% by_row(print(.$terms))

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

"""top_terms %>%
mutate(term = reorder(term, beta)) %>%
ggplot(aes(term, beta)) +
geom_bar(stat = "identity") +
facet_wrap(~ topic, scales = "free") +
theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))"""

lda_gamma <- tidy(speaker_lda, matrix = "gamma")
lda_gamma <- lda_gamma %>% 
  separate(document, into = c("speaker", "line"))

"""ggplot(lda_gamma, aes(gamma, fill = factor(topic))) +
geom_histogram() +
facet_wrap(~ speaker, nrow = 2)"""

topic_lines <- lda_gamma %>%
  group_by(speaker, line) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

topic_lines$line <- as.integer(topic_lines$line)

cands_line <- sent_line %>% filter(speaker %in% c("Trump", 'Clinton'))

cands_topic <- cands_line %>% 
  inner_join(topic_lines)

broad_topics <- c(`1` = "Guns / Policing / Domestic", 
                  `2` = "Terrorism / Tax returns / Cyber / Foreign", 
                  `3` = "Defence / Nuclear / Stamina / Iraq", 
                  `4` = "Trade deals / Experience / Business", 
                  `5` = "Economy / Offshoring / Jobs")

cands_topic$topic_desc <- broad_topics[cands_topic$topic] %>% 
  unname()

cands_topic %>% 
  ggplot(aes(x = line, y = sentiment, fill = topic_desc)) + 
  geom_bar(stat = "identity") +
  facet_grid(~ speaker) +
  theme_bw()

personal <- debate_words %>% 
  mutate(personal = if_else(
    word %in% c("hillary", "clinton", "secretary", "donald", "trump"), 
    "personal", "not_personal")) %>% 
  group_by(line) %>% 
  mutate(n_words = n()) %>% 
  filter(personal == "personal") %>% 
  mutate(pers_words = n()) %>% 
  mutate(personal_prop = pers_words / n_words) %>% 
  select(line, speaker, personal_prop) %>% 
  mutate(personal_bin = if_else(personal_prop > 0.063, "personal", "impersonal")) %>% 
  inner_join(sent_line)

personal %>% 
  filter(speaker == "Trump") %>% 
  ggplot(aes(x = line, y = sentiment, fill = personal_bin)) +
  geom_bar(stat = "identity")
