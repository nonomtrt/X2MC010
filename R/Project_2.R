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
library("wordcloud")
library("RColorBrewer")
library(cluster)

# Import data -------------------------------------------------------------

f2 <- list.files("Textes crypto2", full.names = TRUE)
texts2 <- map_chr(f2, read_file)
cnts2 <- map_int(texts2, stringr::str_count)
raw_texts2 <- tibble(texts2,  encryption_method = extract_encryption(f2))



# Tokenize texts ----------------------------------------------------------

tokenized_data2 <- raw_texts2 |> 
  unnest_tokens(
    tokens, texts2, 
    token = "character_shingles", 
    to_lower = FALSE, 
    n = 3L, 
    strip_non_alphanum = FALSE
  )|> 
  group_by(encryption_method) |> 
  mutate(token_id = 1:n()) |> 
  ungroup()




# Rank --------------------------------------------------------------------

count_data2 <-  tokenized_data2 |> 
  count(encryption_method, tokens) |> 
  right_join(tokenized_data2) |> 
  group_by(encryption_method) |> 
  mutate(rank = min_rank(desc(n)), max_token_id = max(token_id)) |> 
  arrange(token_id) |> 
  ungroup()

filtered_count_data2 <- count_data2 |> 
  filter(token_id <= min(count_data2$max_token_id))

count_data_wide2 <- filtered_count_data2 |> 
  select(encryption_method, token_id, rank) |> 
  pivot_wider(names_from = token_id, values_from = rank)


X2 <- count_data_wide2 |> 
  filter(!(encryption_method %in% c("caesar", "viegenere"))) |>
  select(-1) |> 
  as.matrix()
Xs2 <- scale(X2)
row.names(Xs2) = count_data_wide2$encryption_method[-c(19,58)]



# Dendogram ---------------------------------------------------------------
D_can2 <- dist(Xs2, method = "canberra")


res_fin2 = hclust(D_can2, method = "complete")
fviz_dend(res_fin2, k = 3) + 
  labs(title = "Dendogram with 3 groups", 
       subtitle = "Method: Complete \nMetric: Canberra")

#Cophenetic correlation
cor(cophenetic(res_fin2), cophenetic(res_fin)) #0.831


# Visu of words' frequency ------------------------------------------------

tfidf2 <-  tokenized_data2 |> 
  count(encryption_method, tokens, sort = TRUE) |> 
  group_by(encryption_method) |> 
  mutate(id = 1:n(), rank = min_rank(desc(n))) |> 
  ungroup() |> 
  bind_tf_idf(tokens, encryption_method, n)


set.seed(1234)
wordcloud(words = tfidf2$tokens, freq = tfidf2$tf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




