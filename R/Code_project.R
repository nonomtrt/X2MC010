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

f1 <- list.files("Textes crypto1", full.names = TRUE)
texts1 <- map_chr(f1, read_file)
cnts1 <- map_int(texts1, stringr::str_count)
raw_texts1 <- tibble(texts1,  encryption_method = extract_encryption(f1))



# Tokenize texts ----------------------------------------------------------

tokenized_data1 <- raw_texts1 |> 
  unnest_tokens(
    tokens, texts1, 
    token = "character_shingles", 
    to_lower = FALSE, 
    n = 3L, 
    strip_non_alphanum = FALSE
  )|> 
  group_by(encryption_method) |> 
  mutate(token_id = 1:n()) |> 
  ungroup()




# Rank --------------------------------------------------------------------

count_data1 <-  tokenized_data1 |> 
  count(encryption_method, tokens) |> 
  right_join(tokenized_data1) |> 
  group_by(encryption_method) |> 
  mutate(rank = min_rank(desc(n)), max_token_id = max(token_id)) |> 
  arrange(token_id) |> 
  ungroup()

filtered_count_data1 <- count_data1 |> 
  filter(token_id <= min(count_data1$max_token_id))

count_data_wide1 <- filtered_count_data1 |> 
  select(encryption_method, token_id, rank) |> 
  pivot_wider(names_from = token_id, values_from = rank)


X <- count_data_wide1 |> 
  filter(!(encryption_method %in% c("caesar", "viegenere"))) |>
  select(-1) |> 
  as.matrix()

Xs <- scale(X)
row.names(Xs) = count_data_wide1$encryption_method[-c(19,58)]


D <- dist(Xs)

#Presence of class
res_H = get_clust_tendency(Xs, n = nrow(Xs)-1, graph = FALSE)
res_H$hopkins_stat



#Which method choose?
#k-medoïd
fviz_nbclust(Xs, cluster::pam, method = "wss")+ 
  labs(title = "The Elbow method", subtitle = "k-medoid") #Nothing

fviz_nbclust(Xs, cluster::pam, method = "silhouette") + 
  labs(title = "The silhouette score", subtitle = "k-medoid") #2

#fviz_nbclust(Xs, cluster::pam, method = "gap_stat")         take to long time

pam_xs = pam(Xs, 2, nstart = 25)
fviz_cluster(pam_xs,
             palette = c("#00AFBB", "#FC4E07") ) + 
  labs(title = "Classification with 2-medoid")


#cah
fviz_nbclust(Xs, hcut, method = "wss") + 
  labs(title = "The Elbow method", subtitle = "ACH") #Nothing

fviz_nbclust(Xs, hcut, method = "silhouette")  + 
  labs(title = "The silhouette score", subtitle = "ACH") #3

#fviz_nbclust(Xs, hcut, method = "gap_stat")            take to long time 

D <- dist(Xs)
D_man <- dist(Xs, method = "manhattan")
D_can <- dist(Xs, method = "canberra")

CAH_wd = hclust(D, method = "ward.D2")
fviz_dend(CAH_wd, k = 3) + 
  labs(title = "Ward's method", 
       subtitle = "Euclidean distance")

CAH_s = hclust(D, method = "single")
fviz_dend(CAH_s, k = 3) + 
  labs(title = "Single linkage", 
       subtitle = "Euclidean distance")

CAH_av = hclust(D, method = "average")
fviz_dend(CAH_av, k = 3) + 
  labs(title = "Average linkage", 
       subtitle = "Euclidean distance")

CAH_com = hclust(D, method = "complete")
fviz_dend(CAH_com, k = 3) + 
  labs(title = "Complete linkage", 
       subtitle = "Euclidean distance")

CAH_cen = hclust(D, method = "centroid")
fviz_dend(CAH_cen, k = 3) + 
  labs(title = "Centroid method", 
       subtitle = "Euclidean distance")

CAH_wd_man = hclust(D_man, method = "ward.D2")
fviz_dend(CAH_wd_man, k = 3) + 
  labs(title = "Ward's method", 
       subtitle = "Manhattan distance")

CAH_s_man = hclust(D_man, method = "single")
fviz_dend(CAH_s_man, k = 3) + 
  labs(title = "Single linkage", 
       subtitle = "Manhattan distance")

CAH_av_man = hclust(D_man, method = "average")
fviz_dend(CAH_av_man, k = 3) + 
  labs(title = "Average linkage", 
       subtitle = "Manhattan distance")

CAH_com_man = hclust(D_man, method = "complete")
fviz_dend(CAH_com_man, k = 3) + 
  labs(title = "Complete linkage", 
       subtitle = "Manhattan distance")

CAH_cen_man = hclust(D_man, method = "centroid")
fviz_dend(CAH_cen_man, k = 3) + 
  labs(title = "Centroid method", 
       subtitle = "Manhattan distance")


CAH_wd_can = hclust(D_can, method = "ward.D2")
fviz_dend(CAH_wd_can, k = 3) + 
  labs(title = "Ward's method", 
       subtitle = "Canberra distance")

CAH_s_can = hclust(D_can, method = "single")
fviz_dend(CAH_s_can, k = 3) + 
  labs(title = "Single linkage", 
       subtitle = "Canberra distance")

CAH_av_can = hclust(D_can, method = "average")
fviz_dend(CAH_av_can, k = 3) + 
  labs(title = "Average linkage", 
       subtitle = "Canberra distance")

CAH_com_can = hclust(D_can, method = "complete")
fviz_dend(CAH_com_can, k = 3) + 
  labs(title = "Complete linkage", 
       subtitle = "Canberra distance")

CAH_cen_can = hclust(D_can, method = "centroid")
fviz_dend(CAH_cen_can, k = 3) + 
  labs(title = "Centroid method", 
       subtitle = "Canberra distance")

#Tests to determine which method is the best

km_res = eclust(Xs, "pam", k = 2, nstart = 50, graph = FALSE )
cah_res_com = hcut(D,k=3, hc_func = "hclust", hc_method = "complete")
man_res = hcut(D_man,k=3, hc_func = "hclust", hc_method = "ward.D2")
can_res_wd <- hcut(D_can,k=3, hc_func = "hclust", hc_method = "ward.D2")
can_res_com <- hcut(D_can,k=3, hc_func = "hclust", hc_method = "complete")

#Intern mesure
#Silhouette
sil_km = silhouette(km_res$cluster, D)
(fviz_silhouette(sil_km) + labs(title = "Silhouette vizualisation", 
                                subtitle = "Method: 2-medoids")
  + theme(axis.text.x = element_text(face="bold", color = "#993333", 
                                     size = 8, angle = 90, hjust = 1)))   # 0


sil_cah_com = silhouette(cah_res_com$cluster, D)
(fviz_silhouette(sil_cah_com) + labs(title = "Silhouette vizualisation", 
                                     subtitle = "Method: Cah with Ward.D2")
  + theme(axis.text.x = element_text(face="bold", color = "#993333", 
                                     size = 8, angle = 90, hjust = 1))) #0


sil_man = silhouette(man_res$cluster, D_man)
(fviz_silhouette(sil_man) + labs(title = "Silhouette vizualisation", 
                                     subtitle = "Method: Cah with Ward.D2")
  + theme(axis.text.x = element_text(face="bold", color = "#993333", 
                                     size = 8, angle = 90, hjust = 1))) #0

sil_can_com = silhouette(can_res_com$cluster, D_can)
(fviz_silhouette(sil_can_com) + labs(title = "Silhouette vizualisation", 
                                     subtitle = "Method: Cah with Ward.D2")
  + theme(axis.text.x = element_text(face="bold", color = "#993333", 
                                     size = 8, angle = 90, hjust = 1))) #0

sil_can_wd = silhouette(can_res_wd$cluster, D_can)
(fviz_silhouette(sil_can_wd) + labs(title = "Silhouette vizualisation", 
                                     subtitle = "Method: Cah with Ward.D2")
  + theme(axis.text.x = element_text(face="bold", color = "#993333", 
                                     size = 8, angle = 90, hjust = 1))) #0



#Dunn indice
clValid::dunn(D, km_res$cluster)
clValid::dunn(D, cah_res_com$cluster)
clValid::dunn(D_man, man_res$cluster) 
clValid::dunn(D_can, can_res_wd$cluster) 
clValid::dunn(D_can, can_res_com$cluster) 

#Connectivity indice
clValid::connectivity(D, km_res$cluster) #12.371
clValid::connectivity(D, cah_res_com$cluster) #20.633
clValid::connectivity(D_man, man_res$cluster) 
clValid::connectivity(D_can, can_res_wd$cluster) 
clValid::connectivity(D_can, can_res_com$cluster) 

#Cophenetic 
cor(D, cophenetic(cah_res_com)) 
cor(D_man, cophenetic(man_res)) 
cor(D_can, cophenetic(can_res_com))
cor(D_can, cophenetic(can_res_wd))



res_fin = hclust(D_can, method = "complete")
fviz_dend(res_fin, k = 3) + 
  labs(title = "Dendogram with 3 groups", 
       subtitle = "Method: Complete \nMetric: Canberra")






#TEST

#Visualisation of words' frequency
# By term frequency

tfidf1 <-  tokenized_data1 |> 
  count(encryption_method, tokens, sort = TRUE) |> 
  group_by(encryption_method) |> 
  mutate(id = 1:n(), rank = min_rank(desc(n))) |> 
  ungroup() |> 
  bind_tf_idf(tokens, encryption_method, n)

set.seed(1234)
wordcloud(words = tfidf1$tokens, freq = tfidf1$tf, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



