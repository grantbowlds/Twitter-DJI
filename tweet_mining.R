trumptweets <- read_csv(url("https://raw.githubusercontent.com/grantbowlds/Twitter-DJI/master/Data/trumptweets.csv"))
# install.packages('tidytext')

library(readr) # reading and writing delimited text files
library(dplyr) # SQL-style data processing
library(tidytext) # text analysis in R
library(stringr) # working with text strings
library(tidyr) # data reshaping


# -----------------------------------------------------------------------------------
# Exploratory Analysis
# --------------------------------------------------------------------------------

# Top Words
replace_reg <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\bRT\\b"
# Top Single Words
words <- trumptweets %>%
  mutate(text = str_replace_all(content, replace_reg, "")) %>%
  unnest_tokens(word, content, token = "tweets")
words <- words %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "realdonaldtrump"))

words_count <- words %>%
  group_by(word) %>%
  count()
trump_words <- words_count %>%
  arrange(-n)

# --------------------------------------------------------------------------------

# Top Word Pairs
bigrams <- trumptweets %>% 
  mutate(text = str_replace_all(content, replace_reg, "")) %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2)
bigrams <- bigrams %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]")) %>%
  filter(!str_detect(first, "[.]") & 
           !str_detect(second, "[.]")) %>%
  filter(!str_detect(first, "http") & !str_detect(first, "donaldtrump") &
           !str_detect(second, "http") & !str_detect(first, "donaldtrump")) 
  
bigrams_count <- bigrams %>%
  group_by(bigram) %>%
  count()
trump_bigrams <- bigrams_count %>%
  arrange(-n)

# --------------------------------------------------------------------------------
# Pre-processing - Text Analysis
# --------------------------------------------------------------------------------

# Sentiment Analysis
# install.packages("SentimentAnalysis")
library(SentimentAnalysis)

# split into two to avoid vector memory exhaustion
tweets1 <- trumptweets$content[1:20000]
sentiment1 <- analyzeSentiment(tweets1)
tweets2 <- trumptweets$content[20001:41122]
sentiment2 <- analyzeSentiment(tweets2)

# intermediate processing
tweets1_df <- data.frame(tweets1)
tweets2_df <- data.frame(tweets2)
names(tweets2_df)[1] <- "tweets1"
tweets <- rbind(tweets1_df, tweets2_df)

sentiment <- rbind(sentiment1, sentiment2)
binary_sent <- data.frame(convertToBinaryResponse(sentiment)$SentimentQDAP)

tweet_sentiments <- cbind(tweets, binary_sent)

# join with main df
trumptweets <- cbind(trumptweets, tweet_sentiments)
trumptweets[,10] <- NULL


# --------------------------------------------------------------------------------
# Word Detection
tweet_list <- str_to_lower(trumptweets$content)

# Check if terms are in the list
# Market related terms
market_check <- c("money|business|businesses|economy|economic|tax|taxes|dollars|dollar|pay|
                  oil|market|spending|debt|unemployment|employment|tariff|tariffs|stock|prices|
                  budget|growth|fed|financial|manufacturing|trade|buy|costs|cost|wealth|job|jobs|
                  invest|investment|middle class|working|workers|work|credit|rate|rates|cuts|cut|
                  deal|deals|healthcare|energy|gas|inflation|gdp|monetary|interest|fund|index|surge|
                  housing")
detect_market <- data.frame(str_detect(tweet_list, market_check))

# International Relations related terms
int_relations_check <- c("china|chinese|russia|russian|iran|mexico|mexican|foreign|international|
                         korea|korean|war|iraq|syria|israel|global|isis|borders|immigration|illegal|
                         canada|france|england|japan|india|australia|germany|saudi|arabian|argentina|
                         brazil|latin america|g20|un|united nations|italy|united kingdom|eu|
                         european union|putin|xi|trudeau|merkel")
detect_int_relations <- data.frame(str_detect(tweet_list, int_relations_check))

trumptweets <- cbind(trumptweets, detect_market, detect_int_relations)




