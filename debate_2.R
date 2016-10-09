# data comes from neg_proba.r

# Author: Conor McDonald
# Contact: @conormacd
# Aim: Determine if sentiment used by the candidates shifted from debate 1

library(tidytext)
library(tidyverse)
library(stringr)
library(broom)
library(ggrepel)

# establish priors based on use of negative sentiment in previous debate

trump_b0 <- eb$beta1[eb$speaker == "Trump"]
trump_a0 <- eb$alpha1[eb$speaker == "Trump"]

hillary_b0 <- eb$beta1[eb$speaker == "Clinton"]
hillary_a0 <- eb$alpha1[eb$speaker == "Clinton"]

eb_1 <- eb %>% filter(speaker != "Holt")
eb_1$speaker <- eb_1$speaker %>% str_c("_1")


