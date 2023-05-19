setwd("~/Desktop/S2/X2MC010")
library(tidyverse)
library(tibble)
library(tidytext)
library(factoextra)
library(quanteda)
library("quanteda.textstats")
library(quanteda.textplots)
library(tm)
library(SnowballC)
library("RColorBrewer")
library("wordcloud")
library(cluster)

# Import data -------------------------------------------------------------

f3 <- list.files("Textes crypto3", full.names = TRUE)
texts3 <- map_chr(f3, read_file)
cnts3 <- map_int(texts3, stringr::str_count)
raw_texts3 <- tibble(texts3,  encryption_method = extract_encryption(f3))



# Tokenize texts ----------------------------------------------------------

tokenized_data3 <- raw_texts3 |> 
  unnest_tokens(
    tokens, texts3, 
    token = "character_shingles", 
    to_lower = FALSE, 
    n = 3L, 
    strip_non_alphanum = FALSE
  )|> 
  group_by(encryption_method) |> 
  mutate(token_id = 1:n()) |> 
  ungroup()




# Rank --------------------------------------------------------------------

count_data3 <-  tokenized_data3 |> 
  count(encryption_method, tokens) |> 
  right_join(tokenized_data3) |> 
  group_by(encryption_method) |> 
  mutate(rank = min_rank(desc(n)), max_token_id = max(token_id)) |> 
  arrange(token_id) |> 
  ungroup()

filtered_count_data3 <- count_data3 |> 
  filter(token_id <= min(count_data3$max_token_id))

count_data_wide3 <- filtered_count_data3 |> 
  select(encryption_method, token_id, rank) |> 
  pivot_wider(names_from = token_id, values_from = rank)


X3 <- count_data_wide3 |> 
  filter(!(encryption_method %in% c("caesar", "viegenere"))) |>
  select(-1) |> 
  as.matrix()
Xs3 <- scale(X3)
row.names(Xs3) = count_data_wide3$encryption_method[-c(19,58)]


# Dendogram ---------------------------------------------------------------
D_can3 <- dist(Xs3, method = "canberra")


res_fin3 = hclust(D_can3, method = "complete")
fviz_dend(res_fin3, k = 3) + 
  labs(title = "Dendogram with 3 groups", 
       subtitle = "Method: Complete \nMetric: Canberra")

#Cophenetic correlation
cor(cophenetic(res_fin3), cophenetic(res_fin)) #0.790


# Visu of words' frequency ------------------------------------------------

tfidf3 <-  tokenized_data3 |> 
  count(encryption_method, tokens, sort = TRUE) |> 
  group_by(encryption_method) |> 
  mutate(id = 1:n(), rank = min_rank(desc(n))) |> 
  ungroup() |> 
  bind_tf_idf(tokens, encryption_method, n)


set.seed(1234)
wordcloud(words = tfidf3$tokens, freq = tfidf3$tf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




