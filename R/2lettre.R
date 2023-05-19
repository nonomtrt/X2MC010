
# Tokenize texts ----------------------------------------------------------

tokenized_data2L <- raw_texts1 |> 
  unnest_tokens(
    tokens, texts1, 
    token = "character_shingles", 
    to_lower = FALSE, 
    n = 2L, 
    strip_non_alphanum = FALSE
  )|> 
  group_by(encryption_method) |> 
  mutate(token_id = 1:n()) |> 
  ungroup()




# Rank --------------------------------------------------------------------

count_data2L <-  tokenized_data2L |> 
  count(encryption_method, tokens) |> 
  right_join(tokenized_data2L) |> 
  group_by(encryption_method) |> 
  mutate(rank = min_rank(desc(n)), max_token_id = max(token_id)) |> 
  arrange(token_id) |> 
  ungroup()

filtered_count_data2L <- count_data2L |> 
  filter(token_id <= min(count_data2L$max_token_id))

count_data_wide2L <- filtered_count_data2L |> 
  select(encryption_method, token_id, rank) |> 
  pivot_wider(names_from = token_id, values_from = rank)


X2L <- count_data_wide2L |> 
  filter(!(encryption_method %in% c("caesar", "viegenere"))) |>
  select(-1) |> 
  as.matrix()
Xs2L <- scale(X2L)
row.names(Xs2L) = count_data_wide2L$encryption_method[-c(19,58)]


D_can2L <- dist(Xs2L, method = "canberra")


res_fin2L = hclust(D_can2L, method = "complete")
fviz_dend(res_fin2L, k = 3) + 
  labs(title = "Dendogram with 3 groups", 
       subtitle = "Method: Complete \nMetric: Canberra")


#Cophenetic correlation
cor(cophenetic(res_fin), cophenetic(res_fin2L)) #0.038



# Visu of words' frequency ------------------------------------------------

tfidf2L <-  tokenized_data2L |> 
  count(encryption_method, tokens, sort = TRUE) |> 
  group_by(encryption_method) |> 
  mutate(id = 1:n(), rank = min_rank(desc(n))) |> 
  ungroup() |> 
  bind_tf_idf(tokens, encryption_method, n)


set.seed(1234)
wordcloud(words = tfidf2L$tokens, freq = tfidf2L$tf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

