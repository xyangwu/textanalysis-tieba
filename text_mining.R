## 跑完autism_tieba.R后，再运行以下代码 ---------------------------
library(stringr)
library(tidytext)
library(showtext)
library(RColorBrewer)

## 加载字体
showtext_auto()
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("simsun", "C:/Users/Fonts/simsun.ttc")

# (II) text analysis ----------------------------------------------

## clear "回复 某人:"
lzl$Content = gsub("：", ":", lzl$Content) # str_locate cannot identify Chinese colon (：)
lzl$Content[grep("回复 ", lzl$Content)] = lzl$Content[grep("回复 ", lzl$Content)] %>%
  str_sub(., str_locate(., ":")[, 1]+1, -1) 
  
## clear http links
## 去除http链接

lzl$Content[grep("https?:", lzl$Content)] = gsub("https?://[a-zA-Z0-9.?/&=:]*_?-?[a-zA-Z0-9]+", "", lzl$Content[grep("https?:", lzl$Content)])

post$Content[grep("https?:", post$Content)] = gsub("https?://[a-zA-Z0-9.?/&=:]*_?-?%?[a-zA-Z0-9]+|https?://[a-zA-Z0-9.?/&=:]*_?-?%?[a-zA-Z0-9]+/[a-zA-Z0-9]+=?[a-zA-Z0-9]+/[a-zA-Z0-9]+", "", post$Content[grep("https?:", post$Content)]) 

post$Content[grep("\\[/?url\\]|http:|.jpg|.png", post$Content)] = gsub("\\[/?url\\]|http:|.jpg|.png", "", post$Content[grep("\\[/?url\\]|http:|.jpg|.png", post$Content)])

lzl$Content[grep("[a-zA-Z0-9.+]+@?[a-zA-Z0-9.+]+[ ]?com", lzl$Content)] = gsub("[a-zA-Z0-9.+]+@?[a-zA-Z0-9.+]+[ ]?com", "", lzl$Content[grep("[a-zA-Z0-9.+]+@?[a-zA-Z0-9.+]+[ ]?com", lzl$Content)])

post$Content[grep("[a-zA-Z0-9.+]+@?[a-zA-Z0-9.+]+[ ]?com", post$Content)] = gsub("[a-zA-Z0-9.+]+@?[a-zA-Z0-9.+]+[ ]?com", "", post$Content[grep("[a-zA-Z0-9.+]+@?[a-zA-Z0-9.+]+[ ]?com", post$Content)])


## replcae arab number with Chinese number
number <- data.frame(arab = 1:9, 
                     zh = c("一", "二", "三", "四", "五", "六", "七", "八", "九"),
                     stringsAsFactors = FALSE)

arab2zh <- function(x) {
  num_pattern = str_extract(x, "\\d+个月|\\d+月|\\d+岁") %>% 
    gsub("个月|月|岁", "",. )
  if(nchar(num_pattern) != 1 & substr(num_pattern, 1, 1) != "0"){
    num_replace = paste0(number$zh[number$arab %in% str_sub(num_pattern, 1, 1)],
                         "十", number$zh[number$arab %in% str_sub(num_pattern, 2, 2)])
    gsub(num_pattern, num_replace, x)
  }
  if(num_pattern == "0"){
      gsub(num_pattern, "零", x)
    }else{
    num_replace = number$zh[number$arab %in% num_pattern]
    gsub(num_pattern, num_replace, x)
    }
}

post$Content[grep("\\d+个月|[\\d+]+月|[\\d+]+岁", post$Content)] = str_replace_all(post$Content[grep("\\d+个月|[\\d+]+月|[\\d+]+岁", post$Content)], "[\\d+]+个月|[\\d+]+月|[\\d+]+岁", arab2zh)

lzl$Content[grep("\\d+个月|[\\d+]+月|[\\d+]+岁", lzl$Content)] = str_replace_all(lzl$Content[grep("\\d+个月|[\\d+]+月|[\\d+]+岁", lzl$Content)], "[\\d+]+个月|[\\d+]+月|[\\d+]+岁", arab2zh)


## combine posts and lzl -----------------------------
## 把post和lzl两个数据集放到一个数据框

## 保留 post 和 lzl 独一无二的id 
post_lzl2 = lzl %>% 
  left_join(., post %>% 
              select(Id, Thread_id),
            by = c("Post_id" = "Id")) %>%
  select(Id, Content, User_id = Author_id, Time = Original_Time, Thread_id) %>%
  mutate(type = "lzl") %>%
  rbind(post %>%
          left_join(., thread, by = c("Thread_id" = "Id")) %>%
          # replace contents that have less than 12 words with Titles
          mutate(Content = ifelse(nchar(Content) <= 12 & Floor == 1, Title, Content)) %>% 
          select(Id, Content, User_id = User_id.x, Time, Thread_id) %>%
          mutate(type = "post"), 
        .) %>% 
  left_join(., author_info[, 2:5], by = c("User_id" = "User_id")) %>%
  left_join(., thread[, c("Id", "Title")], by = c("Thread_id" = "Id")) %>% 
  left_join(., thread_reply[, c("Id", "reply_n")], by = c("Thread_id" = "Id"))

write.csv(post_lzl2, "data/tea.csv", row.names = FALSE) 


# text segmentation ----------------------------------

## download a stop word list suitable for Chinese, the Baidu stop words
#cstops <- "https://raw.githubusercontent.com/ropensci/textworkshop17/master/demos/chineseDemo/ChineseStopWords.txt"
#csw <- paste(readLines(cstops, encoding = "UTF-8"), collapse = "\n") # download
#csw <- gsub("\\s", "", csw)           # remove whitespace
#stop_words <- strsplit(csw, ",")[[1]]

stop_words <- read.csv("data/stop_words.csv", stringsAsFactors = FALSE) %>% unlist() 
stop_words2 <- readLines("data/stopwords-zh.txt", encoding = "UTF-8") 

#stop_words2 %>%
 # enc2native(.) %>% 
  #writeLines(., "data/stop_words_3.txt") 

#readLines("data/stopwords-zh.txt") 


# jiebaR ------------------------------------------

library(jiebaR) 

cutter <- worker(bylines = T, 
                 user = "data/UsrWords.txt",
                 stop_word = "data/stopwords-zh.txt") 

userwords = readLines("data/UserWords.txt", encoding = "UTF-8")
stopw_zh <- quanteda::stopwords("zh", source = "misc")

new_user_word(cutter, words = userwords, rep("n", length(userwords))) 


# segmentation ------------------------------------------

## segmentation in each post and lzl 按照每个发言的分词结果

text_seg = segment(post_lzl2$Content, cutter) 

## list to data.fame 
text_df = data.frame(document = rep(post_lzl2$Id, sapply(text_seg, length)),
                     word = unlist(text_seg), 
                     stringsAsFactors = FALSE) 

text_df = text_df %>% 
  filter(!word %in% c(stop_words2, stopw_zh, "　", "NA", "顶", "额", "已经", "现在", 1:100, letters[1:26], LETTERS[1:26])) %>%
  filter(!str_detect(word, "[0-9]\\.[^&]?|\\.[0-9]|[0-9]-|-[0-9]|[a-zA-Z]|[0-9][0-9][0-9]+|^(\\d+)$"))

row.names(text_df) = 1:nrow(text_df) 

write.csv(text_df, "data/teaword.csv", row.names = FALSE) # 3441208 rows
text_df = read.csv("data/teaword.csv")
# text_df$word[text_df$word == "儿童"] = "孩子" 


# word count  --------------------------------------

## total word count 
word_counts <- text_df %>% 
  count(word, sort = TRUE)

word_counts$word %>% head(., n = 20) 

## word count: levels -----------------------------------------
wordcount_level = text_df %>% 
  left_join(., post_lzl2[, c("Id", "Level_name")], by = c("document" = "Id")) %>%
  filter(!is.na(Level_name) & !word %in% c("没有", "现在")) %>% 
  count(Level_name, word) %>%
  group_by(Level_name) %>% 
  arrange(desc(n)) %>%
  ungroup(.) 
  
wordcount_level %>% 
  group_by(Level_name) %>% 
  top_n(10, n) %>%
  ungroup() %>%
  arrange(word, -n) %>%
  ggplot(aes(reorder(word, n), n, fill = factor(Level_name))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Level_name, scales = "free") +
  coord_flip() + 
  scale_x_reordered() +
  theme_light()
  
wordcount_level <- wordcount_level %>%
  bind_tf_idf(word, Level_name, n)

wordcount_level %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Level_name) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Level_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Level_name, ncol = 3, scales = "free") +
  coord_flip()

## word count: genders -----------------------------------------

wordcount_sex = text_df %>% 
  left_join(., post_lzl2[, c("Id", "Sex")], by = c("document" = "Id")) %>%
  filter(!is.na(Sex) & !word %in% c("没有", "现在")) %>% 
  count(Sex, word) %>% 
  group_by(Sex) %>% 
  arrange(desc(n)) %>% 
  ungroup(.) 

wordcount_sex %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Sex) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = Sex)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Sex, scales = "free") +
  coord_flip() + 
  theme_light()

wordcount_sex <- wordcount_sex %>% 
  bind_tf_idf(word, Sex, n)

wordcount_sex %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Sex) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Sex)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Sex, ncol = 3, scales = "free") +
  coord_flip()


## word count: self-generate content in thread -------------------------------------------------------
## 词频计算：每条帖子中楼主自己的发言

wordcount_thread = text_df %>% 
  left_join(., post_lzl2[, c("Id", "User_id", "Thread_id")], by = c("document" = "Id")) %>%
  left_join(., thread[, c("Id", "User_id")], by = c("Thread_id" = "Id")) %>% 
  group_by(Thread_id) %>% 
  filter(User_id.x == User_id.y) %>%
  ungroup() %>%
  count(Thread_id, word, sort = TRUE)

## word count: whole content in thread -------------------------------------------------------
## 词频计算：每条帖子中的所有发言

wordcount_thread_all = text_df %>% 
  left_join(., post_lzl2[, c("Id", "Thread_id")], by = c("document" = "Id")) %>%
  ungroup() %>%
  count(Thread_id, word, sort = TRUE)


## word count: User_id -------------------------------------------------------

text_df$document = as.character(text_df$document) 

## 词频计算：每个成员(以User_id为准)在自己帖子中的发言
wordcount_user = text_df %>% 
  left_join(., post_lzl2[, c("Id", "User_id", "Thread_id")], by = c("document" = "Id")) %>%
  left_join(., thread[, c("Id", "User_id")], by = c("Thread_id" = "Id")) %>% 
  group_by(Thread_id) %>% 
  filter(User_id.x == User_id.y) %>%
  ungroup() %>% 
  count(User_id.x, word, sort = TRUE) 

names(wordcount_user)[1] = "User_id"

## 词频计算：每个成员(以User_id为准)的所有发言
wordcount_user_all = text_df %>% 
  left_join(., post_lzl2[, c("Id", "User_id")], by = c("document" = "Id")) %>%
  count(User_id, word, sort = TRUE) 

write.csv(wordcount_thread, "data/wordcount_user.csv", row.names = FALSE)


# topic modeling -----------------------------------

library(topicmodels) 

## cast a one-token-per-row table into a Document-Term Matrix 
## then fit a LDA model
## 每行一词的表格转化成文档词语矩阵(Document-Term Matrix)
text_dtm <- wordcount_user %>% 
  cast_dtm(User_id, word, n) 
text_lda_user <- LDA(text_dtm, k = 2, control = list(seed = 1234))

text_dtm <- wordcount_user_all %>% 
  cast_dtm(User_id, word, n) 
text_lda_user_all <- LDA(text_dtm, k = 2, control = list(seed = 1234))

text_dtm <- wordcount_thread %>% 
  cast_dtm(Thread_id, word, n) 
text_lda_thread <- LDA(text_dtm, k = 2, control = list(seed = 1234))

text_dtm <- wordcount_thread_all %>% 
  cast_dtm(Thread_id, word, n) 
text_lda_thread_all <- LDA(text_dtm, k = 2, control = list(seed = 1234))

text_lda_thread_all@logLiks[-c(1:(burnin/keep))]
harmonicMean(text_lda_thread_all@logLiks[-c(1:(burnin/keep))])

## find the best value for k 

seqk <- seq(2, 50, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fit_k <- lapply(seqk, function(k) LDA(text_dtm, k = k)))#, method = "Gibbs"))) #,control = list(burnin = burnin, iter = iter, keep = keep) )))

## extract logliks from each topic
logLiks <- lapply(fit_k, function(L)  L@logLiks[-c(1:(burnin/keep))])


## compute harmonic means
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

## find the best value for k 
library("ldatuning")

result <- FindTopicsNumber(
  text_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2020),
  mc.cores = 3L,
  verbose = TRUE
) 

FindTopicsNumber_plot(result)

## per-topic-per-word probabilities.

text_topics <- tidy(text_lda_thread_all, matrix = "beta")

text_topics 
#text_topics$topic[text_topics$topic == 1] = "非正式"
#text_topics$topic[text_topics$topic == 2] = "正式"

## the top 10 terms within each topic.

top_terms <- text_topics %>%
  group_by(topic) %>% 
  top_n(30, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) 

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + 
  scale_x_reordered() + 
  labs(title = '',
       x = NULL,
       y = "β（前30个词语的主题概率）") + 
  theme_classic() +
  theme(axis.text.y = element_text(family = "simsun", size = rel(1.2)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"))

ggsave("fig/topic_word_3.pdf", width = 8, height = 8)

## the per-document-per-topic probabilities,  γ(“gamma”).

text_gamma <- tidy(text_lda_thread_all, matrix = "gamma") 
text_gamma

text_gamma_df = text_gamma %>% 
  pivot_wider(., names_from = "topic", values_from = "gamma") %>% 
  mutate(dif = `2` - `1`, 
         topic_given = ifelse(dif >= 0.25, "非正式", ifelse(dif <= -0.25, "正式", "混合"))) %>%
  left_join(., author_info[, c("User_id", "Level_name")], by = c("document" = "User_id"))

## density plot

text_gamma_df %>% 
  #left_join(., thread[, c("Id", "Level_name")], by = c("document" = "Id")) %>% 
  #filter(!is.na(Level_name)) %>%
  ggplot(., aes(x = dif)) +
  geom_density( alpha = .8, fill = '#C6DBEF', color = 'steelblue') + 
  theme_classic() + 
  labs(title = '', 
       y = NULL,
       x = "主题概率(正式社会支持→非正式社会支持)")

ggsave("fig/topic_thread_all.pdf", width = 8, height = 7)

## density plot by Level_name
ggplot(text_gamma_df, aes(x = dif, fill = Level_name)) +
  geom_histogram(alpha = .8) + 
  facet_wrap(vars(Level_name)) + 
  theme_classic() + 
  labs(title = '', 
       y = NULL,
       x = "主题概率(正式社会支持→非正式社会支持)")

thread$Level_name %>% table()

table(text_gamma_df$topic_given) 

write.xlsx(text_gamma_df, "data/user_all_topic.xlsx") 

## how often positive and negative words occurred in each Level


# n-gram ------------------------------

library(quanteda)

seg_thread_all = text_df %>%
  left_join(., post_lzl2[, c("Id", "User_id", "Thread_id")], by = c("document" = "Id")) %>%
  group_by(Thread_id) %>% 
  summarise(text = list(word)) %>%
  ungroup()

seg_thread = text_df %>%
  left_join(., post_lzl2[, c("Id", "User_id", "Thread_id")], by = c("document" = "Id")) %>%
  left_join(., thread[, c("Id", "User_id")], by = c("Thread_id" = "Id")) %>% 
  group_by(Thread_id) %>% 
  filter(User_id.x == User_id.y) %>%
  summarise(text = list(word)) %>%
  ungroup() 

## list to tokens

tokens_list = as.tokens(seg_thread$text)

tokens_list 

## compute n-grams
ngrams_thread = tokens_ngrams(tokens_list, n = 2, concatenator = " ")
ngrams_thread_all = tokens_ngrams(tokens_list, n = 2, concatenator = " ")

## bigram tokens to one-bigram-one-row data.frame 
ngrams_thread_all = data.frame(document = rep(seg_thread_all$Thread_id, sapply(ngrams_thread_all, length)),
                           bigram = unlist(ngrams_thread_all),
                           stringsAsFactors = FALSE) 

bigrams_separated <- ngrams_thread_all %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words2) %>%
  filter(!word2 %in% stop_words2)

# bigram counts
bigram_counts_threadall <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts_threadall = bigram_counts_threadall %>%
  arrange(n)
head(bigram_counts) 

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf)) 


# network of bigrams -----------------------------------
library(igraph) 
library(ggraph) 

set.seed(2020) 

## filter for only relatively common combinations
bigram_graph <- bigram_counts_threadall %>% 
  filter(n >= 200) %>% 
  graph_from_data_frame() 
 
node_info = left_join(data.frame(name = V(bigram_graph)$name, stringsAsFactors = FALSE), 
                      text_topics %>%
                        pivot_wider(names_from = "topic",
                                    values_from = "beta") %>%
                        mutate(dif = `1` - `2`) %>%
                        select(term, dif),
                      by = c("name" = "term"))

## igraph plot 
V(bigram_graph)$size = 8 
#V(bigram_graph)$label = V(bigram_graph)$name 
#V(bigram_graph)$label.family = "simfang" 
#V(bigram_graph)$label.cex <- .6
#V(bigram_graph)$label.color = rgb(0,0,.2,.8)
#V(bigram_graph)$label.cex <- .6
#V(bigram_graph)$frame.color <- NA
V(bigram_graph)$color <- "lightblue"
#V(bigram_graph)$topic <- node_info$dif

is.directed(bigram_graph) 
bigram_graph_simp = bigram_graph %>% 
  as.undirected(., mode = "collapse", edge.attr.comb = mean) %>%
  simplify(., remove.multiple = FALSE)

groups = cluster_louvain(bigram_graph_simp, weights = E(bigram_graph_simp)$n)

## number of words in each group 
groups_n = sapply(1:36, function(x)length(groups[[x]]))
which(groups_n > 20) 

groups2 = list(groups[[1]], groups[[2]], groups[[4]], groups[[6]], groups[[15]])

alpha.vec = E(bigram_graph)$n/max(E(bigram_graph)$n)

par(mar=c(1, 1, 1, 1)) 

pdf('fig/ngrams_user_200.pdf', width = 8, height = 6)

plot(bigram_graph_simp, 
     layout = layout.fruchterman.reingold, 
     vertex.size = ifelse(V(bigram_graph)$name %in% c("孩子", "自闭症"), 7, 5), 
     vertex.color = ifelse(V(bigram_graph)$name %in% c("孩子", "自闭症"), "lightpink", "lightblue"), 
     vertex.frame.color = NA, 
     vertex.label.cex = ifelse(V(bigram_graph)$name %in% c("孩子", "自闭症"), 1, 0.7),
     vertex.label.color = "black", 
     edge.width = log10(E(bigram_graph_simp)$n)/10, 
     edge.color = alpha("black", alpha = alpha.vec + 0.2), 
     edge.arrow.size = 0.2, 
     mark.groups = groups2) 

dev.off()

tkplot(bigram_graph_simp)
bigram_graph_simp$layout <- tkplot.getcoords(3) 

## ggraph plot

showtext_auto()

a <- grid::arrow(type = "open", length = unit(0.08, "inches"))

pal = wesanderson::wes_palette("Zissou1", length(V(bigram_graph)), type = "continuous")
#pal = brewer.pal(11, "RdYlBu")

set.seed(2022)
ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.1, 'inches')) + 
  geom_node_point(#aes(colour = topic), 
                  colour = ifelse(V(bigram_graph)$name %in% c("孩子", "自闭症"), "lightpink", "lightblue"), show.legend = FALSE, 
                  size = ifelse(V(bigram_graph)$name %in% c("孩子", "自闭症"), 9, 7)) + 
  #scale_colour_gradientn(colours = pal, name = "主题概率") +
  geom_node_text(aes(label = name), size = 3) + 
  theme_void() 

ggsave("fig/ngrams_user_n150_3.pdf", width = 10, height = 8)

bigram_tf_idf


# sentiment analysis ---------------------------------
sentiment_result1 = read.csv("data/sentiment_result1.csv")
sentiment_result2 = read.csv("data/sentiment_result2.csv")

head(sentiment_result1)

## sentiment 1
sentiment_thread_all1 = sentiment_result1 %>% 
  group_by(Thread_id) %>%
  summarise(senti_sum = sum(weight, na.rm = TRUE))

quantile(sentiment_thread_all1$senti_sum, probs = seq(0, 1, 0.1))
quantile(sentiment_thread_all1$senti_sum) 

sentiment_thread_all1$senti_sum_log = log(abs(sentiment_thread_all1$senti_sum) + 0.01)
sentiment_thread_all1$senti_sum_log[sentiment_thread_all1$senti_sum < 0] = -c(sentiment_thread_all1$senti_sum_log[sentiment_thread_all1$senti_sum < 0])
sentiment_thread_all1$source = "cnki"

sentiment_thread_all1 %>%
  ggplot(aes(senti_sum_log)) + 
  geom_histogram(bins = 50, fill = '#C6DBEF', color = 'steelblue') +
  theme_classic() 

## sentiment 2
sentiment_thread_all2 = sentiment_result2 %>% 
  group_by(Thread_id) %>% 
  summarise(senti_sum = sum(weight, na.rm = TRUE)) 

quantile(sentiment_thread_all2$senti_sum, probs = seq(0, 1, 0.1)) 

quantile(sentiment_thread_all2$senti_sum) 

sentiment_thread_all2$senti_sum_log = log(abs(sentiment_thread_all2$senti_sum) + 0.01)
sentiment_thread_all2$senti_sum_log[sentiment_thread_all2$senti_sum < 0] = -c(sentiment_thread_all2$senti_sum_log[sentiment_thread_all2$senti_sum < 0])
sentiment_thread_all2$source = "dalian"

sentiment_thread_all2 %>% 
  ggplot(aes(senti_sum)) + 
  geom_boxplot(fill = '#C6DBEF', color = 'steelblue') +
  theme_classic() 

sentiment_thread_all = rbind(sentiment_thread_all1, sentiment_thread_all2)

sentiment_thread_all %>% 
  ggplot(aes(senti_sum_log, fill = source, colour = source)) + 
  geom_histogram(alpha = 0.9) +
  scale_color_manual(values = c("coral", "deepskyblue"), name = "情感词汇库") +
  scale_fill_manual(values = c("lightpink", "lightblue"), name = "情感词汇库") +
  theme_classic() 

# pairwise correlation -------------------------------
# pairwise correlation of word frequencies within each group
library(widyr)

word_pairs <- wordcount_thread_all %>%
  pairwise_cor(User_id, word, n, sort = TRUE)

sum(word_pairs$correlation > 0.7)

cor_graph = word_pairs %>%
  filter(correlation > .7) %>%
  graph_from_data_frame() %>%
  as.undirected(mode = "collapse", edge.attr.comb = mean)

brewer.pal(4, "Spectral")
display.brewer.pal(4, "Spectral")

node_cor = left_join(data.frame(name = V(cor_graph)$name, stringsAsFactors = FALSE), 
                     author_info %>% 
                       select(User_id, Level_name),
                     by = c("name" = "User_id")) %>% 
  left_join(., data.frame(Level_name = c("初级粉丝", "中级粉丝", "高级粉丝", "正式会员", "核心会员", "铁杆会员", "知名人士", "人气楷模"),
                          colour = rep(brewer.pal(4, "Spectral"), each = 2)),
            by = c("Level_name" = "Level_name"))
  

V(cor_graph)$level_name = node_cor$Level_name
V(cor_graph)$color = node_cor$colour

plot(cor_graph, vertex.label = NA, vertex.size = 5,
     layout = layout_nicely)

ggraph(cor_graph, layout = "fr") +
  geom_edge_link(aes(width = correlation),
                 end_cap = circle(.1, 'inches')) +
  geom_node_point(aes(colour = level_name), size = 3) + 
  theme_void()

ggsave("fig/paircor_user.pdf", width = 10, height = 8)

# cloud plotting -------------------------------------

library(wordcloud)

wordall = text_df %>% 
  count(word) 

wordall = wordall %>%
  filter(!word %in% c("没有", "现在"))

## wordcloud 
showtext_auto()
font_family <- par("family") # the previous font family
par(family = "simyou") # change to a nice Chinese font

pdf("fig/wordcloud.pdf", width = 12, height = 10)

with(wordall, {
  wordcloud::wordcloud(word, n, min.freq = 300, max.words = 200,
                       random.order = FALSE, rot.per = 0,
                       colors = brewer.pal(8, "Dark2"))
})#,colors = brewer.pal(8, "Dark2")

dev.off()


# reference ------------------------------------------------------
# https://www.tidytextmining.com/ngrams.html
# https://bookdown.org/markhoff/social_network_analysis
# http://jjohn987.rbind.io/post/a-quasi-tidytext-analysis-of-3-chinese-classics/
# https://my.oschina.net/u/2529303/blog/546816
# https://quanteda.io/reference/tokens_ngrams.html
# https://www.r-bloggers.com/2019/10/advancing-text-mining-with-r-and-quanteda/
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
# http://videolectures.net/mlss09uk_blei_tm/
