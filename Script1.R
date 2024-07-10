df <- read.csv("C:/Users/mholt/Desktop/UU/Publicatie/Data Analysis/Translated/Translated_c.csv", #Loading translated data
               na.strings = NA)

#1. Combine pageCategory 
library(dplyr)
mapping_df <- data.frame(platformId = df$platformId,
                         old_category = df$account.pageCategory,
                         new_category = NA)

new_categories <- c("Media", "Community", "Public Figure", "Government", 
                    "Civil_Society", "Education & Culture", "Industry", "undef")

for (old_category in unique(df$account.pageCategory)) {
  cat_input <- as.integer(readline(prompt = paste("Enter the number corresponding to", old_category, ": ")))
  if (cat_input %in% seq_along(new_categories)) {
    new_category <- new_categories[cat_input]
    mapping_df$new_category[which(mapping_df$old_category == old_category)] <- new_category
  } else {
    cat_input <- as.integer(readline(prompt = "Invalid number. Enter again: "))
  }
}                                                                               #Function for collapsing categories

df$platformId <- as.character(df$platformId)
mapping_df$platformId <- as.character(mapping_df$platformId)
df <- merge(df, mapping_df, by = "platformId", all.x = TRUE)                    #Df with new category (names)

df <- mutate(df, new_category_code = recode(new_category, 
                                            "Media" = 1,
                                            "Community" = 2,
                                            "Public Figure" = 3,
                                            "Government" = 4,
                                            "Civil_Society" = 5,
                                            "Education & Culture" = 6,
                                            "Industry" = 7,
                                            "undef" = 8))

df <- df %>%                                                                    #Correcting mistakes
  mutate(new_category_code = ifelse(new_category == "SCIENCE_MUSEUM", 
                                    6, new_category_code))

write.csv(mapping_df, file = "mapping_df.csv", row.names = FALSE)               #Storing map
write.csv(df, file = "df.csv", row.names = FALSE)                               #Recoded df

#2.1 Computing engagement and comparing across groups
df <- read.csv("C:/Users/mholt/Desktop/UU/Publicatie/Rproj/df.csv",             #Loading df with compiled page_category
               na.strings = NA)
df$eng <- df$statistics.actual.likeCount + df$statistics.actual.shareCount + df$statistics.actual.commentCount
met.eng <- df %>%
  group_by(new_category) %>%
  summarize(mean.eng = mean(eng, na.rm = TRUE))
max.mean.eng <- max(met.eng$mean.eng)
met.eng <- met.eng %>% mutate(share = mean.eng / max.mean.eng)
met.eng %>%arrange(desc(mean.eng)) 

#2.2 Compiling and comparing media types 
df <- df %>%
  mutate(new.type = case_when(
    type == "status" ~ 1,
    type == "photo" ~ 2,
    type == "link" ~ 3,
    type == "youtube" ~ 3,
    type == "video" ~ 4,
    type == "native_video" ~ 4,
    type == "live_video_complete" ~ 4,
    type == "live_video" ~ 4,
    type == "live_video_scheduled" ~ 3,
    TRUE ~ NA_integer_ 
  ))

type_count <- df %>%
  group_by(new_category, new.type) %>%
  summarize(count = n(), .groups = 'drop')
type_count %>% pivot_wider(names_from = new.type, values_from = count, values_fill = list(count = 0))

#2.3 Compiling country of origin
df <- df %>%
  mutate(native = ifelse(account.pageAdminTopCountry == "MM", 1, 0))
n.count <- df %>%
  group_by(new_category, native) %>%
  summarize(count = n(), .groups = 'drop')
n.count %>% pivot_wider(names_from = native, values_from = count, values_fill = list(count = 0))

#3. Text pre-processing
library(readxl)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(progress)
library(pbapply)
library(hunspell)
library(stringi)
library(textcat)
library(wordcloud)
library(igraph)
library(ggraph)
library(widyr)
library(countrycode)
library(arules)
library(tm)
library(topicmodels)
library(stringr)
library(lda)
library(quanteda)
library(textreuse)
library(vcd)
library(patchwork)
library(psych)

#3.1 Pre-processing: removing Burmese, Emojis, NA's, HTML tags, URLs, and non-ASII
dfc <- df
dfc$nmessage <- dfc$translated
dfc <- distinct(dfc)                                                            #Removing complete duplicates
remove_emojis <- function(text) {
  emoji_range <- "[\U00002600-\U000027BF\U0001F300-\U0001F64F\U0001F680-\U0001F6FF\U0001F900-\U0001F9FF\u2600-\u26FF\u2700-\u27BF]"
  cleaned_text <- iconv(text, to = "ASCII", sub = "")
  
  return(cleaned_text)
}

dfc$nmessage <- gsub("[\u1000-\u109F]", " ", dfc$nmessage, perl = TRUE)         #Removing Remaining Burmese
dfc$nmessage <- remove_emojis(dfc$nmessage)                                     #Removing Emoticons

remove_html_tags <- function(text) {                                            #Function for removing HTML tags
  return(str_replace_all(text, "<[^>]*>", ""))
}
remove_urls <- function(text) {                                                 #Function for removinf URLs
  url_pattern <- "http[s]?://\\S+|www\\.\\S+"
  return(str_replace_all(text, url_pattern, ""))
}
clean_text <- function(text) {                                                  #Integration
  text_no_html <- remove_html_tags(text)
  text_no_urls <- remove_urls(text_no_html)
  return(text_no_urls)
}
dfc$nmessage <- pbsapply(dfc$nmessage, clean_text)                              #Removing HTML tags and Links

remove_non_ascii <- function(text) {
  stri_trans_general(text, "Latin-ASCII")
}
dfc$nmessage <- sapply(dfc$nmessage, remove_non_ascii)                          #Remove Remianing non-ASCII
dfc$nmessage[dfc$nmessage == "NA"] <- NA                                        #Removing NA
dfc$nmessage[dfc$nmessage == ""] <- NA                                          #Removing empty string
dfc <- dfc[!is.na(dfc$nmessage), ]

#3.2 Investigating near-duplicates based on Jaccard similarity
text <- dfc$nmessage
text <- text[nchar(text) >= 10]
text <- gsub("[^[:alnum:][:space:]]", "", text)
text <- tolower(text)                                                           #Cleaning

minhash <- minhash_generator(n = 400, seed = 3552)                              #Minhash function
corpus <- TextReuseCorpus(text = text,                                          #Corpus creation with minhasing
                          tokenizer = tokenize_ngrams, 
                          n = 5, 
                          minhash_func = minhash,  
                          keep_tokens = TRUE,
                          progress = TRUE,
                          skip_short = TRUE)
buckets <- lsh(corpus, bands = 80, progress = TRUE)
candidates <- lsh_candidates(buckets)                                           #Potentially similar cases
similarities <- lsh_compare(candidates, corpus, jaccard_bag_similarity, 
                            progress = TRUE)
high_similarity_pairs <- similarities[similarities$score > 0.95, c("a", "b")]   #Identifying very similar pairs

#3.3 Tokenising, cleaning and stemming
numbers_to_remove <- data.frame(word = as.character(0:9))
country_names <- data.frame(
  word = tolower(countrycode::codelist$country.name.en)
)
split_country_names <- unlist(strsplit(country_names$word, " "))
all_country_names <- unique(c(country_names$word, split_country_names))
country_names <- data.frame(word = all_country_names)

dfc$nmessage <- removePunctuation(dfc$nmessage)                                 #Removing punctuations
dfc$nmessage <- stripWhitespace(dfc$nmessage)                                   #Stripping whitespaces 
mes.tm <- unnest_tokens(dfc, word, nmessage)                                    #Tokenising
mes.tm$word <- tolower(mes.tm$word)                                             #Cleaning stopwords, numbers, countries
mes.tm <- mes.tm %>% anti_join(filter(stop_words, lexicon == "snowball"))
mes.tm <- mes.tm %>% anti_join(numbers_to_remove, by = "word")
mes.tm <- mes.tm %>% filter(!grepl("\\d", word))
mes.tm <- mes.tm %>% anti_join(country_names, by = "word")

stem_hunspell <- function(term) {
  stems <- hunspell_stem(term)[[1]]
  
  if (length(stems) == 0) {
    stem <- NA
  } else {
    if (nchar(stems[[length(stems)]]) > 1) 
      stem <- stems[[length(stems)]] else stem <- term
  }
  stem
}      

word.list <- count(mes.tm, word)                                                
words.stem <- pblapply(word.list$word, stem_hunspell)                           #Stemming
stem.list <- cbind(word.list, stem = unlist(words.stem))
stem.list <- stem.list[!is.na(stem.list[, 3]) & stem.list[, 1] != stem.list[, 3], ]

write.csv(stem.list, "stem.list.csv")
stem.list <- read.csv("stem.list.csv", stringsAsFactors = FALSE)                #Writing stemlist

mes.stem <- dfc
orig.words <- paste0("\\b", stem.list[, 1], "\\b")
stem.words <- stem.list[, 3]
mes.stem$nmessage <- stri_replace_all_regex(mes.stem$nmessage, orig.words,      
                                            stem.words, vectorize_all = FALSE)
mes.stem <- mes.stem[!duplicated(mes.stem$nmessage), ]
write.csv(mes.stem, "mes.stem.csv", row.names = F)                              #Df with stemmed messages 

#3.4 Word frequencies 
df.stem <- read.csv("C:/Users/mholt/Desktop/UU/Publicatie/Rproj2/mes.stem.csv")

df.stem$nmessage <- removePunctuation(df.stem$nmessage)                         #Removing punctuations
df.stem$nmessage <- stripWhitespace(df.stem$nmessage)                           #Stripping whitespaces 
mes.tm <- unnest_tokens(df.stem, word, nmessage)                                #Tokenising
mes.tm$word <- tolower(mes.tm$word)                                             #Cleaning stopwords, numbers, countries
mes.tm <- mes.tm %>% anti_join(filter(stop_words, lexicon == "snowball"))
mes.tm <- mes.tm %>% anti_join(numbers_to_remove, by = "word")
mes.tm <- mes.tm %>% filter(!grepl("\\d", word))
mes.tm <- mes.tm %>% anti_join(country_names, by = "word")

word.freq <- mes.tm %>% count(word, sort = TRUE)                                #Word count
word.freq %>% top_n(30) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word,#Word count histogram (30)
                                    n)) + geom_col() + xlab(NULL) + coord_flip()
word.freq %>% with(wordcloud(word, n, max.words = 50, random.order = FALSE,     #Word count wordcloud (50)
                             rot.per = 0.35, colors = brewer.pal(8, "Dark2")))

#3.5 Bigram analysis 
df.stem <- read.csv("C:/Users/mholt/Desktop/UU/Publicatie/Rproj2/mes.stem.csv")
df.stem$nmessage <- removePunctuation(df.stem$nmessage)                         #Removing punctuations
df.stem$nmessage <- stripWhitespace(df.stem$nmessage)                           #Stripping whitespaces 
mes.tm.bi <- unnest_tokens(df.stem, word, nmessage, token = "ngrams", n = 2)
head(mes.tm.bi[, c("platformId", "word")])
mes.tm.bi %>% count(word, sort = TRUE)

mes.separated.bi <- mes.tm.bi %>%                                               #Removing stopwords
  separate(word, c("word1", "word2"), sep = " ") %>% 
  anti_join(filter(stop_words, lexicon == "snowball"), by = c(word1 = "word")) %>% 
  anti_join(filter(stop_words, lexicon == "snowball"), by = c(word2 = "word"))
mes.separated.bi <- mes.separated.bi %>%
  anti_join(country_names, by = c(word1 = "word")) %>%
  anti_join(country_names, by = c(word2 = "word"))

mes.count.bi <- mes.separated.bi %>% unite(word, word1, word2, sep = " ") %>% count(word, 
                                                                    sort = TRUE)
mes.count.bi %>% top_n(30) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, #co-word count histogram
                                    n)) + geom_col() + xlab(NULL) + coord_flip()
mes.count.bi %>% with(wordcloud(word, n, scale = c(2, 0.5), max.words = 50,         #co-word count wordcloud
        random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2")))
word.network.bi <- mes.separated.bi %>% count(word1, word2, sort = TRUE) %>% filter(n > 
                                                800) %>% graph_from_data_frame()
a <- arrow(angle = 30, length = unit(0.1, "inches"), ends = "last", type = "open")
ggraph(word.network.bi, layout = "fr") + 
  geom_edge_link(aes(color = n, width = n), 
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#3.6 Pairwise association and correlation
word.pairs <- mes.tm %>% pairwise_count(word, platformId, sort = TRUE)
print(word.pairs, n=50)                                                         #Wordpairs (50)
word.phi <- mes.tm %>% group_by(word) %>% filter(n() >= 1000) %>% pairwise_cor(word, 
                                                                id, sort = TRUE)
word.phi %>% print(n = 100)                                                     #100 strongest correlates
a <- arrow(angle = 30, length = unit(0.05, "inches"), ends = "last", type = "open")

word.phi %>% filter(correlation > 0.50) %>% graph_from_data_frame() %>% ggraph(layout = "fr") +  #Strongest correlations web
  geom_edge_link(aes(color = correlation, width = correlation), arrow = a) + geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
mes.dtm <- mes.tm %>% count(platformId, word) %>% cast_dtm(platformId, word, n)
mes.subset <- removeSparseTerms(mes.dtm, 0.98)                                  #Removing very sparse terms
mes.mat <- as.matrix(mes.subset)
mes.mat[mes.mat > 0] <- 1
mes.arules <- as(mes.mat, "transactions")
summary(mes.arules)
itemFrequencyPlot(mes.arules, topN = 20)
image(mes.arules[1:100])
mes.rules <- apriori(mes.arules, parameter = list(support = 0.017,             
                                      confidence = 0.7, minlen = 2))
summary(mes.rules)
rules_df <- as(mes.rules, "data.frame")
rules_df                                                                        #If A, then how likely B

#4.1 Determining topic numbers
df.stem <- read.csv("C:/Users/mholt/Desktop/UU/Publicatie/Rproj2/mes.stem.csv")
df.stem$nmessage <- removePunctuation(df.stem$nmessage)                         #Removing punctuations
df.stem$nmessage <- stripWhitespace(df.stem$nmessage)                           #Stripping whitespaces 
mes.tm <- unnest_tokens(df.stem, word, nmessage)  
mes.tm$word <- tolower(mes.tm$word)                                             #Cleaning stopwords, numbers, countries
mes.tm <- mes.tm %>% anti_join(filter(stop_words, lexicon == "snowball"))
mes.tm <- mes.tm %>% anti_join(numbers_to_remove, by = "word")
mes.tm <- mes.tm %>% filter(!grepl("\\d", word))

set.seed(123456)
mes.dtm <- mes.tm %>%                                                           #Creating dtm
  count(platformId, word) %>%    
  cast_dtm(platformId, word, n)  

k.topics <- 5:25                                                                #Topic range 

folding <- rep(1:5, each = 2000)                                                #5 Folds, 2000 cases 
runonce <- function(k, fold) {                                                  #Perplexity
  testing.dtm <- which(folding == fold)
  training.dtm <- which(folding != fold)
  
  training.model <- LDA(mes.dtm[training.dtm, ], k = k)
  test.model <- LDA(mes.dtm[testing.dtm, ], model = training.model, 
                    control = list(estimate.beta = FALSE))
  
  perplexity(test.model)
}

res <- NULL

for (k in 5:25) {                                                               #Running for all instances of k
  for (fold in 1:5) {
    res <- rbind(res, c(k, fold, runonce(k, fold)))
  }
}

total.perp <- tapply(res[, 3], res[, 1], sum)
round(total.perp)
plot(5:25, total.perp, type = "b", xlab = "Number of topics", ylab = "Perplexity")

resdata <- as.data.frame(res)
names(resdata) <- c("topics", "fold", "perplexity")
resdata$fold <- as.factor(resdata$fold)

ggplot(data = resdata, aes(x = topics, y = perplexity, group = fold, shape = fold, 
 color = fold)) + geom_line() + geom_point() + scale_shape_manual(values = 1:10)#Plotting perplexity

avgdata <- cbind(topics = 5:25, means = tapply(res[, 3], res[, 1], mean))
avgdata <- as.data.frame(avgdata)
names(avgdata) <- c("topics", "perplexity")

ggplot(data = resdata, aes(x = topics, y = perplexity, group = fold, color = fold)) + 
  geom_line() + geom_point() + scale_shape_manual(values = 1:5) + geom_line() + 
  geom_line(data = avgdata, aes(x = topics, y = perplexity, color = "6", size = 1.5), 
            show.legend = F)

avgdata <- cbind(topics = 5:15, tapply(res[, 3], res[, 1], mean))
avgdata <- as.data.frame(avgdata)
names(avgdata) <- c("topics", "perplexity")

ggplot(data = resdata, aes(x = topics, y = perplexity, group = fold, color = fold)) + 
  geom_line() + geom_point() + scale_shape_manual(values = 1:10) + geom_line() + 
  geom_line(data = avgdata, aes(x = topics, y = perplexity, color = "6", size = 1.5), 
            show.legend = FALSE)

#4.2 LDA
mes.lda <- LDA(mes.dtm, k = 10)
terms(mes.lda, 20) 

set.seed(123456)
mes.lda1 <- LDA(mes.dtm, k = 10)
terms(mes.lda1, 20)                                                             #terms per topic

set.seed(654321)
mes.lda2 <- LDA(mes.dtm, k = 10)
terms(mes.lda2, 20)                                                             #terms per topic

set.seed(123654)
mes.lda3 <- LDA(mes.dtm, k = 10)
terms(mes.lda3, 20)                                                             #terms per topic

mes.topics <- tidy(mes.lda, matrix = "beta")
mes.topics
mes.terms <- mes.topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% #Words associated with topics
  arrange(topic, -beta)
mes.terms

plot.ts(log(mes.topics[mes.topics$topic == 1, "beta"]))                         #Enter useful topics
lines(log(mes.topics[mes.topics$topic == 11, "beta"]), col = 2)

topics(mes.lda)[1:10]                                                           #Extract topics
mes.comments <- tidy(mes.lda, matrix = "gamma")                                 #Prop. words from each topic
mes.comments                                                                    #Per document

#4.3 Filtering posts (round 1)
likely.topics <- topics(mes.lda)
df.ordered <- dfc[order(dfc$platformId), ]

df.document.ids <- dfc$platformId                                               #Matching on platformId
lda.document.ids <- names(likely.topics)
common.ids <- intersect(df.document.ids, lda.document.ids)
df.filtered <- dfc %>% filter(platformId %in% common.ids)
df.filtered <- df.filtered[match(common.ids, df.filtered$platformId), ]
likely.topics.filtered <- likely.topics[common.ids]
likely.topics.filtered <- likely.topics.filtered[order(names(likely.topics.filtered))]
df.filtered$topic <- likely.topics.filtered
table(df.filtered$topic)
rel.df <- df.filtered %>% filter(topic %in% c(1, 6))

fin.df <- rel.df
write.csv(fin.df, "final.csv", row.names = F)                                   #Final dataset 

#4.4 Descriptives and visualisations 
df.filtered$disc <- ifelse(df.filtered$topic %in% c(1,6), 1, 0)                 #Binary (a)political discussion variable
table(df.filtered$disc, df.filtered$native)                                     #(non)-native
table(df.filtered$disc, df.filtered$new.type)                                   #Page type
table(df.filtered$disc, df.filtered$new_category)                               #Account type 

df.filtered <- df.filtered %>%
  mutate(native = as.factor(native),
         new.type = as.factor(new.type),
         new_category = as.factor(new_category),
         disc = as.factor(disc))
mosaic(~ disc + native + new_category, data = df.filtered,
       highlighting = "native",
       highlighting_fill = c("red", "blue"),
       main = "Mosaic Plot of Discussion, Native status, and User group")
heatmap_data <- df.filtered %>%
  group_by(native, new_category, disc) %>%
  summarise(count = n()) %>%
  spread(key = native, value = count, fill = 0)

ggplot(heatmap_data, aes(x = new_category, y = disc)) +
  geom_tile(aes(fill = `0`)) +  
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of Engagement by User group and Discussion",
       x = "Category",
       y = "Discourse") +
  theme_minimal()

#4.5 Determining topics in relevant data
rel.df$nmessage <- removePunctuation(rel.df$nmessage)                           #Removing punctuations
rel.df$nmessage <- stripWhitespace(rel.df$nmessage)                             #Stripping whitespaces 
rel.tm <- unnest_tokens(rel.df, word, nmessage)  
rel.tm$word <- tolower(rel.tm$word)                                             #Cleaning stopwords, numbers, countries
rel.tm <- rel.tm %>% anti_join(filter(stop_words, lexicon == "snowball"))
rel.tm <- rel.tm %>% anti_join(numbers_to_remove, by = "word")
rel.tm <- rel.tm %>% filter(!grepl("\\d", word))
rel.tm <- rel.tm %>% anti_join(country_names, by = "word")

rel.dtm <- rel.tm %>%
  count(platformId, word) %>%    
  cast_dtm(platformId, word, n)  

k.topics <- 2:12

folding <- rep(1:5, each = 1000)
runonce <- function(k, fold) {
  testing.dtm <- which(folding == fold)
  training.dtm <- which(folding != fold)
  
  training.model <- LDA(rel.dtm[training.dtm, ], k = k)
  test.model <- LDA(rel.dtm[testing.dtm, ], model = training.model, control = list(estimate.beta = FALSE))
  
  perplexity(test.model)
}

res <- NULL

for (k in 2:12) {
  for (fold in 1:5) {
    res <- rbind(res, c(k, fold, runonce(k, fold)))
  }
}

total.perp <- tapply(res[, 3], res[, 1], sum)
round(total.perp)
plot(2:12, total.perp, type = "b", xlab = "Number of topics", ylab = "Perplexity")

resdata <- as.data.frame(res)
names(resdata) <- c("topics", "fold", "perplexity")
resdata$fold <- as.factor(resdata$fold)

ggplot(data = resdata, aes(x = topics, y = perplexity, group = fold, shape = fold, 
color = fold)) + geom_line() + geom_point() + scale_shape_manual(values = 1:10)

avgdata <- cbind(topics = 2:12, tapply(res[, 3], res[, 1], mean))
avgdata <- as.data.frame(avgdata)
names(avgdata) <- c("topics", "perplexity")

ggplot(data = resdata, aes(x = topics, y = perplexity, group = fold, color = fold)) + 
  geom_line() + geom_point() + scale_shape_manual(values = 1:10) + geom_line() + 
  geom_line(data = avgdata, aes(x = topics, y = perplexity, color = "6", size = 1.5), 
            show.legend = F)

#4.6 Correlated topic models
fin.df <- read.csv("C:/Users/mholt/Desktop/UU/Publicatie/Rproj2/final.csv")
fin.tm <- unnest_tokens(fin.df, word, nmessage)  
fin.tm$word <- tolower(fin.tm$word)                                             #Cleaning stopwords, numbers, countries
fin.tm <- fin.tm %>% anti_join(filter(stop_words, lexicon == "SMART"))
fin.tm <- fin.tm %>% anti_join(filter(stop_words, lexicon == "ONIX"))
fin.tm <- fin.tm %>% anti_join(filter(stop_words, lexicon == "snowball"))
fin.tm <- fin.tm %>% anti_join(numbers_to_remove, by = "word")
fin.tm <- fin.tm %>% filter(!grepl("\\d", word))
fin.tm <- fin.tm %>% anti_join(country_names, by = "word")

fin.dtm <- fin.tm %>%
  count(platformId, word) %>%    
  cast_dtm(platformId, word, n)  
fin.ctm <- CTM(fin.dtm, k = 7, control = list(seed = 123456))                   #Correlated topics 
terms(fin.ctm, 25)                                                              #Terms per topic
class(fin.ctm) <- "LDA_VEM"
fin.topics.cor <- tidy(fin.ctm, matrix = "beta")                                
fin.topics.cor
fin.terms.cor <- fin.topics.cor %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% #Words associated with the topics
  arrange(topic, -beta)                                                            
fin.terms.cor %>% print(n = 30)                                                 #Top 30 topics

topic_names <- c(
  "1" = "1. Int. Coop",
  "2" = "2. Pros.",
  "3" = "3. Pol. Act.",
  "4" = "4. Int. Dev. Coop.",
  "5" = "5. Nat. Dis.",
  "6" = "6. Arm. Res. Clash.",
  "7" = "7. HR & Atr"
)

fin.terms.cor <- fin.terms.cor %>%
  mutate(topic_name = factor(topic_names[as.character(topic)], levels = topic_names))
fin.terms.cor %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~topic_name, scales = "free") +
  xlab("Terms") + ylab("Topics") + coord_flip()

#4.7 Correlations and top documents
fin.comments.cor <- tidy(fin.ctm, matrix = "gamma")                             #Prop. words for each topic
fin.comments.cor %>% filter(document == "10")

theta_matrix <- fin.comments.cor %>%
  spread(topic, gamma) %>%
  select(-document) %>%
  as.matrix()
topic_correlations <- cor(theta_matrix)
topic_correlations                                                              #Correlation matrix for all topics

top_docs <- fin.comments.cor %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  arrange(topic, -gamma)
print(top_docs, n=70)                                                           #Top documents for topics
df %>%                                                                          #Requesting documents from df 
  filter(platformId == "554852994662858_2338275346320605") %>%
  select(translated)

#4.8 Descriptives
most_likely_topics <- fin.comments.cor %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  select(document, topic)
most_likely_topics <- most_likely_topics %>%
  rename(platformId = document, most_likely_topic = topic)
fin.df <- fin.df %>%
  left_join(most_likely_topics, by = "platformId")
fin.df <- fin.df %>%
  mutate(topic_name = factor(topic_names[as.character(most_likely_topic)], 
                             levels = topic_names))
write.csv(fin.df, "C:/Users/mholt/Desktop/UU/Publicatie/Rproj2/final_with_topics.csv", 
          row.names = FALSE)                                                    #Final data with topics appended

#4.9 Visualisations
actor_topic_count <- fin.df %>%
  group_by(most_likely_topic, new_category) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(actor_topic_count, aes(x = factor(most_likely_topic), y = count, fill = new_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Topic") + ylab("Number of Documents") +
  ggtitle("Prevalence of Actors Across Topics") +
  theme_minimal()

native_topic_count <- fin.df %>%
  group_by(most_likely_topic, native) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(native_topic_count, aes(x = factor(most_likely_topic), y = count, fill = native)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Topic") + ylab("Number of Documents") +
  ggtitle("Prevalence of Nativeness Across Topics") +
  theme_minimal()

media_type_topic_count <- fin.df %>%
  group_by(most_likely_topic, new.type) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(media_type_topic_count, aes(x = factor(most_likely_topic), y = count, fill = new.type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Topic") + ylab("Number of Documents") +
  ggtitle("Prevalence of Media Types Across Topics") +
  theme_minimal() +
  scale_x_discrete(labels = most_likely_topics)

ggplot(fin.df, aes(x = factor(most_likely_topic), y = eng)) +
  geom_boxplot(aes(fill = factor(most_likely_topic))) +
  xlab("Topic") + ylab("Engagement") +
  ggtitle("Engagement Across Topics") +
  theme_minimal() +
  scale_x_discrete(labels = topic_names) +
  theme(legend.position = "none") 

label_mapping <- c("Public Figure" = "PF", 
                   "Industry" = "Ind", 
                   "Government" = "Gov", 
                   "undef" = "Und", 
                   "Media" = "Med", 
                   "Community" = "Com", 
                   "Civil_Society" = "CS", 
                   "Education & Culture" = "EC")
fin.df <- fin.df %>%
  mutate(new_category = recode(new_category, !!!label_mapping))
engagement_actor <- fin.df %>%
  group_by(most_likely_topic, new_category) %>%
  summarise(mean_engagement = mean(eng, na.rm = TRUE)) %>%
  ungroup()
p1 <- ggplot(engagement_actor, aes(y = factor(most_likely_topic), x = new_category, fill = mean_engagement)) +
  geom_tile() +
  labs(y = "Topic", x = "Actor Category", fill = "Mean Engagement", title = "Engagement Across Topics and Actors") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  scale_y_discrete(labels = topic_names)
engagement_native <- fin.df %>%
  group_by(most_likely_topic, native) %>%
  summarise(mean_engagement = mean(eng, na.rm = TRUE)) %>%
  ungroup()
p2 <- ggplot(engagement_native, aes(y = factor(most_likely_topic), x = native, fill = mean_engagement)) +
  geom_tile() +
  labs(y = "Topic", x = "Native Status", fill = "Mean Engagement", title = "Engagement Across Topics and Native Status") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  scale_y_discrete(labels = topic_names)
engagement_media <- fin.df %>%
  group_by(most_likely_topic, new.type) %>%
  summarise(mean_engagement = mean(eng, na.rm = TRUE)) %>%
  ungroup()

p3 <- ggplot(engagement_media, aes(y = factor(most_likely_topic), x = new.type, fill = mean_engagement)) +
  geom_tile() +
  labs(y = "Topic", x = "Media Type", fill = "Mean Engagement", title = "Engagement Across Topics and Media Types") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  scale_y_discrete(labels = topic_names)
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3)
print(combined_plot)

#5.1 Emoticon responses
custom_standardize_01 <- function(x) {                                          #0-1 min-max scaling
  if (min(x) == max(x)) {
    return(rep(0, length(x)))  
  } else {
    (x - min(x)) / (max(x) - min(x))
  }
}
fin.df <- fin.df %>%                                                            #standardizing variables
  mutate(
    stand.likes = custom_standardize_01(statistics.actual.likeCount),
    stand.love = custom_standardize_01(statistics.actual.loveCount),
    stand.sad = custom_standardize_01(statistics.actual.sadCount),
    stand.angry = custom_standardize_01(statistics.actual.angryCount)
  )

fin.df$reac <- (fin.df$stand.likes + fin.df$stand.love) - (fin.df$stand.angry + fin.df$stand.sad)
describe(fin.df$reac)                                                           #Computing score 

#5.2 Visualization
fin.df <- fin.df %>%
  mutate(most_likely_topic = factor(most_likely_topic, levels = names(topic_names)),
         new_category = factor(new_category),
         native = factor(native),
         new.type = factor(new.type))
calculate_color_limits <- function(data) {
  max_abs <- max(abs(range(data)))
  return(c(-max_abs, max_abs))
}
reactions_actor <- fin.df %>%
  group_by(most_likely_topic, new_category) %>%
  summarise(mean_reactions = mean(reac, na.rm = TRUE)) %>%
  ungroup()
p1 <- ggplot(reactions_actor, aes(y = factor(most_likely_topic), x = new_category, fill = mean_reactions)) +
  geom_tile() +
  labs(y = "Topic", x = "Actor Category", fill = "Mean Reactions", title = "Crowd Reactions Across Topics and Actors") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = calculate_color_limits(reactions_actor$mean_reactions)) +
  theme_minimal() +
  scale_y_discrete(labels = topic_names)
reactions_native <- fin.df %>%
  group_by(most_likely_topic, native) %>%
  summarise(mean_reactions = mean(reac, na.rm = TRUE)) %>%
  ungroup()
p2 <- ggplot(reactions_native, aes(y = factor(most_likely_topic), x = native, fill = mean_reactions)) +
  geom_tile() +
  labs(y = "Topic", x = "Native Status", fill = "Mean Reactions", title = "Crowd Reactions Across Topics and Native Status") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = calculate_color_limits(reactions_native$mean_reactions)) +
  theme_minimal() +
  scale_y_discrete(labels = topic_names)
reactions_media <- fin.df %>%
  group_by(most_likely_topic, new.type) %>%
  summarise(mean_reactions = mean(reac, na.rm = TRUE)) %>%
  ungroup()
p3 <- ggplot(reactions_media, aes(y = factor(most_likely_topic), x = new.type, fill = mean_reactions)) +
  geom_tile() +
  labs(y = "Topic", x = "Media Type", fill = "Mean Reactions", title = "Crowd Reactions Across Topics and Media Types") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = calculate_color_limits(reactions_media$mean_reactions)) +
  theme_minimal() +
  scale_y_discrete(labels = topic_names)
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3)
print(combined_plot)


