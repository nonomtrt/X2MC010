
# Tokenize texts ----------------------------------------------------------

tokenized_data4L <- raw_texts1 |> 
  unnest_tokens(
    tokens, texts1, 
    token = "character_shingles", 
    to_lower = FALSE, 
    n = 4L, 
    strip_non_alphanum = FALSE
  )|> 
  group_by(encryption_method) |> 
  mutate(token_id = 1:n()) |> 
  ungroup()




# Rank --------------------------------------------------------------------

count_data4L <-  tokenized_data4L |> 
  count(encryption_method, tokens) |> 
  right_join(tokenized_data4L) |> 
  group_by(encryption_method) |> 
  mutate(rank = min_rank(desc(n)), max_token_id = max(token_id)) |> 
  arrange(token_id) |> 
  ungroup()

filtered_count_data4L <- count_data4L |> 
  filter(token_id <= min(count_data4L$max_token_id))

count_data_wide4L <- filtered_count_data4L |> 
  select(encryption_method, token_id, rank) |> 
  pivot_wider(names_from = token_id, values_from = rank)


X4L <- count_data_wide4L |> 
  filter(!(encryption_method %in% c("caesar", "viegenere"))) |>
  select(-1) |> 
  as.matrix()
Xs4L <- scale(X4L)
row.names(Xs4L) = count_data_wide4L$encryption_method[-c(19,58)]

D_can4L <- dist(Xs4L, method = "canberra")

res_fin4L = hclust(D_can4L, method = "complete")
fviz_dend(res_fin4L, k = 3) + 
  labs(title = "Dendogram with 3 groups", 
       subtitle = "Method: Complete \nMetric: Canberra")



# Visu of words' frequency ------------------------------------------------

tfidf4L <-  tokenized_data4L |> 
  count(encryption_method, tokens, sort = TRUE) |> 
  group_by(encryption_method) |> 
  mutate(id = 1:n(), rank = min_rank(desc(n))) |> 
  ungroup() |> 
  bind_tf_idf(tokens, encryption_method, n)


set.seed(1234)
wordcloud(words = tfidf4L$tokens, freq = tfidf4L$tf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


cor(cophenetic(res_fin), cophenetic(res_fin4L)) #0.738
cor(cophenetic(res_fin2), cophenetic(res_fin4L)) #0.797


