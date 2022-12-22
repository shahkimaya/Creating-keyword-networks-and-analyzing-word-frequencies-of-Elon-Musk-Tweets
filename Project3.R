library(stringr)
library(dplyr)
library(ggplot2)
library(igraph)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggraph)

###################TASK 1################################################
key_data <- read.csv("/Users/kimayashah/Downloads/Keyword_data.csv")
data_na<-key_data%>%
  filter(Title!="")

x = str_which(data_na$Title,"/03$")
data_na = data_na[-c(x),]
dim(data_na)

data_na<-data_na[,2:13]

len_keyword <- length(data_na)
no_of_articles <- dim(data_na)[1]
## Count Number of Unique Keywords
key_count = 1
key_unique <- list()
for (article in 1:no_of_articles){
  for(keyword in 1:len_keyword){
    key_unique[[key_count]] <- data_na[[keyword]][[article]]
    #data_na[[j]][[i]]
    key_count <- key_count + 1
  }
}
#Removed Duplicate values
key_unique <- unique((key_unique))
key_unique <- key_unique[!key_unique == '']
#  key_unique[1:10]
length_uniq_keywords <- length(key_unique)
# Creating a 248*248 matrix
key_matrix <- matrix(0, nrow = length_uniq_keywords, ncol = length_uniq_keywords)
# Name the rows & cols of the matrix
colnames(key_matrix) <- key_unique
rownames(key_matrix) <- key_unique
# Iteration through each unique keyword
# For each article
for (article in 1:no_of_articles){
  
  # For each keyword in an article  
  for (keyword_cnt in 1:(len_keyword-1) ){
    current_keyword <- data_na[[keyword_cnt]][[article]]
    if (current_keyword == ''){
      next
    }
    # For next keyword in an article
    for (next_keyword_cnt in (keyword_cnt+1):len_keyword){
      next_keyword <- data_na[[next_keyword_cnt]][[article]]
      
      if (next_keyword == ''){
        
        next
      }
      
      key_matrix[current_keyword, next_keyword] <- key_matrix[current_keyword, next_keyword] + 1 -> key_matrix[next_keyword, current_keyword]  
    }
  }
}
#########################
# 1.2
#sum(key_matrix)
net1 <- graph_from_adjacency_matrix(as.matrix(key_matrix),mode="undirected",weighted = T)
plot(net1)
# 1.3
#  Degree of network
deg <- degree(net1, mode = "all")
d1 <- as.data.frame(deg)
d1
# Strength of network
strength <- strength(net1, mode = "all")
d2 <- as.data.frame(strength)
d2
# 1.4
# Top 10 nodes by degree
d1 %>% top_n(10)
deg_10 <- d1 %>% slice_max(deg, n = 10)
deg_10
# Top 10 nodes by strength
d2 %>% top_n(10)
str_10 <- d2 %>% slice_max(strength, n = 10)
str_10
# 1.5
W <-  get.data.frame(net1)
# Top 10 node pairs by weight
W1 <- head(W[order(W$weight, decreasing=TRUE), ], 10)
rownames(W1) <- c("1","2","3","4","5","6","7","8","9","10")
W1
# 1.6
D0 <- as.data.frame(deg)
D1 <- as.data.frame(strength)
D0 <- cbind(D0,D1$strength)
colnames(D0)[2] <- "strength"
New_df <- D0 %>%
  group_by(deg) %>%
  summarise(Average_strength = mean(strength))
# Plotting Degree vs Average strength
plot(x= New_df$deg, y = New_df$Average_strength , main="Degree vs Average Strength",
     xlab="Degree", ylab="Average_Strength")


############################TASK 2#################################

#Reading data from 2017-2020 since 2020.csv contains data from 2010-2020.
df_20<-read.csv('/Users/kimayashah/Downloads/2020.csv')
head(df_20,10)
df_20<-df_20%>% #fetching only required columns i.e date and tweet
  select(date,tweet)
df_20

df_21<-read.csv('/Users/kimayashah/Downloads/2021_1.csv')
head(df_21,10)
df_21<-df_21%>% #fetching only required columns i.e date and tweet
  select(date,tweet)
df_21

df_22<-read.csv('/Users/kimayashah/Downloads/2022.csv')
head(df_22,10)
df_22<-df_22%>% #fetching only required columns i.e date and tweet
  select(date,tweet)
df_22


separate_df <- separate(df_20, col = date, into = c("Year", "Month", "Date"), sep = "-") 
#tail(separate_df1,10)
final_df <- separate_df[c(1,4)]


separate_df_21 <- separate(df_21, col = date, into = c("Year", "Month", "Date"), sep = "-") 
#tail(separate_df1,10)
final_df_21 <- separate_df_21[c(1,4)]


separate_df_22 <- separate(df_22, col = date, into = c("Year", "Month", "Date"), sep = "-") 
#tail(separate_df1,10)
final_df_22 <- separate_df_22[c(1,4)]

df_final_17_20 <- final_df %>%
  filter(Year >= 2017)

# splitting the tweet column into tokens
e_tweet_words <- df_final_17_20 %>%
  unnest_tokens(word, tweet)

#filtering only 2017 data into a separate dataframe
df_17<-df_final_17_20%>%
  filter(Year==2017)

#filtering only 2018 data into a separate dataframe
df_18<-df_final_17_20%>%
  filter(Year==2018)

#filtering only 2019 data into a separate dataframe
df_19<-df_final_17_20%>%
  filter(Year==2019)

#filtering only 2020 data into a separate dataframe
df_20<-df_final_17_20%>%
  filter(Year==2020)

#combined all dataframes into one so that it gets easier for computation
df_all<-rbind(df_17,df_18,df_19,df_20,final_df_21,final_df_22)
dim(df_all)

#Part 1 -  Calculating the word frequencies for each year & excluding the stop words

#creating some cusotmized stop words based on data
customized_stop_words <- data.frame('word' = c("http","https","t.co", "amp" , "itâ" , "â"))

word_freq <- df_all %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words) %>%
  filter(!word %in% customized_stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>%
  count(word, Year, sort = TRUE)
word_freq
dim(word_freq)

#2.2 Show top 10 words (for each year) by the highest value of word frequency

#word frequency for 2017 
wf_2017 <- word_freq%>%
  filter(Year=='2017')
top_17<-head(wf_2017,10)

#word frequency for 2018 
wf_2018 <- word_freq%>%
  filter(Year=='2018')
top_18<-head(wf_2018,10)

#word frequency for 2019 
wf_2019 <- word_freq%>%
  filter(Year=='2019')
top_19<-head(wf_2019,10)

#word frequency for 2020 
wf_2020 <- word_freq%>%
  filter(Year=='2020')
top_20<-head(wf_2020,10)

#word frequency for 2021 
wf_2021 <- word_freq%>%
  filter(Year=='2021')
top_21<-head(wf_2021,10)

#word frequency for 2022 
wf_2022 <- word_freq%>%
  filter(Year=='2022')
top_22<-head(wf_2022,10)

#2.3 - Plot histogram of word frequencies for each year

# Calculate Total Count for 2017
total_tw_17 <- wf_2017 %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))

tweet_words_17 <- left_join(top_17, total_tw_17)
#Histogram for 2017 
ggplot(tweet_words_17, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) + xlab('Frequency')
facet_wrap(~Year, ncol = 2, scales = "free_y")


# Calculate Total Count for 2018
total_tw_18 <- wf_2018 %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))

tweet_words_18 <- left_join(top_18, total_tw_18)
#Histogram for 2018
ggplot(tweet_words_18, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) + xlab('Frequency')
facet_wrap(~Year, ncol = 2, scales = "free_x","free_y")  

# Calculate Total Count for 2019
total_tw_19 <- wf_2019 %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))

tweet_words_19 <- left_join(top_19, total_tw_19)
#Histogram for 2019 
ggplot(tweet_words_19, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) + xlab('Frequency')
facet_wrap(~Year, ncol = 2, scales = "free_y")


# Calculate Total Count for 2020
total_tw_20 <- wf_2020 %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))

tweet_words_20 <- left_join(top_20, total_tw_20)
#Histogram for 2020 
ggplot(tweet_words_20, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) + xlab('Frequency')
facet_wrap(~Year, ncol = 2, scales = "free_y")

# Calculate Total Count for 2021
total_tw_21 <- wf_2021 %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))

tweet_words_21 <- left_join(top_21, total_tw_21)
#Histogram for 2021
ggplot(tweet_words_21, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) + xlab('Frequency')+
  facet_wrap(~Year, ncol = 2, scales = "free_y")

#Calculate Total Count for 2022
total_tw_22 <- wf_2022 %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))

tweet_words_22 <- left_join(top_22, total_tw_22)
#Histogram for 2022 
ggplot(tweet_words_22, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) + xlab('Frequency')
+facet_wrap(~Year, ncol = 2, scales = "free_y")


# Calculate Total Count for all years
total_tw <- word_freq %>% 
  group_by(Year) %>% 
  summarize(total = sum(n))
tweet_words <- left_join(word_freq, total_tw)
ggplot(tweet_words, aes(n/total, fill = Year)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~Year, ncol = 2, scales = "free_y")  


#2.4 Use Zipf’s law and plot log-log plots of word frequencies and rank for each year
e_zipf <- tweet_words %>%
  group_by(Year) %>% 
  mutate(frequency= n/total, rank = row_number() )

lm(log10(frequency) ~ log10(rank), data = e_zipf)

# plotting log log plots
ggplot(e_zipf, aes(rank, frequency, color = factor(Year))) + 
  geom_abline(intercept = -0.62, slope = -1.02, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.84, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()


#2.5 Create bigram network graphs for each year

tw_bi <- df_all %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2) 

#separating words
bi_separated = tw_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Removing Stop words from bigram
sw_bi <- data.frame('word' = stop_words$word)
cust_stop_words <- rbind(customized_stop_words,sw_bi)

# filtering the bigram

bi_filtered <- bi_separated %>%
  filter(!word1 %in% cust_stop_words$word) %>%
  filter(!word2 %in% cust_stop_words$word) %>%
  filter(str_detect(word1, "[a-z]")) %>%
  filter(str_detect(word2, "[a-z]"))


# new bigram counts:
bi_counts <- bi_filtered %>% 
  group_by(Year) %>%
  count(word1, word2, sort = TRUE)   

#Combining the words
bi_united = bi_filtered %>%
  unite(bigram, word1, word2, sep = " ")  

bi_tf_idf <- bi_united %>%
  count(Year, bigram) %>%
  bind_tf_idf(bigram, Year, n) %>%
  arrange(desc(tf_idf))
#Visualizing bigrams

#2017
bi_2017 <- bi_counts %>% 
  filter(Year == "2017") 

#2018
bi_2018 <- bi_counts %>% 
  filter(Year == "2018") 

#2019
bi_2019 <- bi_counts %>% 
  filter(Year == "2019") 

#2020
bi_2020 <- bi_counts %>% 
  filter(Year == "2020") 

#2021
bi_2021 <- bi_counts %>% 
  filter(Year == "2021") 

#2022
bi_2022 <- bi_counts %>% 
  filter(Year == "2022") 


bigram_graph_2017 <- bi_2017 %>%
  filter(n > 5) %>%
  graph_from_data_frame()

bigram_graph_2018 <- bi_2018 %>%
  filter(n > 10) %>%
  graph_from_data_frame()

bigram_graph_2019 <- bi_2019 %>%
  filter(n > 11) %>%
  graph_from_data_frame()


bigram_graph_2020 <- bi_2020 %>%
  filter(n > 8) %>%
  graph_from_data_frame()

bigram_graph_2021 <- bi_2021 %>%
  filter(n > 4) %>%
  graph_from_data_frame()

bigram_graph_2022 <- bi_2022 %>%
  filter(n > 2) %>%
  graph_from_data_frame()


b <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Plot Biagrams
ggraph(bigram_graph_2017, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for 2017 Tweets")

ggraph(bigram_graph_2018, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for 2018 Tweets")

ggraph(bigram_graph_2019, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for 2019 Tweets")

ggraph(bigram_graph_2020, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for 2020 Tweets")

ggraph(bigram_graph_2021, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for 2021 Tweets")  


ggraph(bigram_graph_2022, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = b, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Bigram for 2022 Tweets")  

##################################################################