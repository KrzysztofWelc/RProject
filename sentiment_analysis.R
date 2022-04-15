#data import

simpsons_csv = read.csv("./data/simpsons_data.csv", header = TRUE)
guy_csv = read.csv("./data/guy_data.csv", header = TRUE)
southpark_csv = read.csv("./data/southpark_data.csv", header = TRUE)

install.packages("textdata")
library(tidyr)

#preparing & tokenizing data

Sys.setlocale("LC_ALL","English")

prepare_my_data <- function(data){
  new_data <- tibble(line = 1:nrow(data), text = data$content, date = data$date)
  new_data$date <- as.Date(new_data$date, format = "%d %B %Y")
  new_data <- new_data %>%
    arrange(date)
  new_data$line <- c(1:nrow(new_data))
  
  #tokenizing
  new_data <- new_data %>%
    unnest_tokens(word, text)
  
  new_data
}

simpsons_tidy <- prepare_my_data(simpsons_csv)
guy_tidy <- prepare_my_data(guy_csv)
southpark_tidy <- prepare_my_data(southpark_csv)


#bing lexicon

bing_sentiment <- function(data){
  new_data <- data %>%
    inner_join(get_sentiments("bing")) %>%
    count(comment = line, date, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)
}

simpson_sentiment_bing <- bing_sentiment(simpsons_tidy)
guy_sentiment_bing <- bing_sentiment(guy_tidy)
southpark_sentiment_bing <- bing_sentiment(southpark_tidy)


#afinn lexicon

afinn_sentiment <- function(data){
  new_data <- data %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(date) %>% 
    summarise(sentiment = sum(value)) %>% 
    mutate(method = "AFINN")
}

simpson_sentiment_afinn <- afinn_sentiment(simpsons_tidy) 
guy_sentiment_afinn <- afinn_sentiment(guy_tidy)
southpark_sentiment_afinn <- afinn_sentiment(southpark_tidy) 



#nrc lexicon

nrc_sentiment <- function(data){
  new_data <- data %>% 
    inner_join(get_sentiments("nrc") %>% 
                 #   filter(sentiment %in% c("positive", 
                 #                           "negative"))
                 # %>%
                 mutate(method = "NRC")) %>%
    count(comment = line, date, sentiment) %>%
    pivot_wider(names_from = sentiment,
                values_from = n,
                values_fill = 0) %>% 
    mutate(sentiment = positive - negative)
}

simpson_sentiment_nrc <- nrc_sentiment(simpsons_tidy)
guy_sentiment_nrc <- nrc_sentiment(guy_tidy)
southpark_sentiment_nrc <- nrc_sentiment(southpark_tidy)



# plots

plot(simpson_sentiment_bing$date, simpson_sentiment_bing$sentiment, type="l")
lines(guy_sentiment_bing$date, guy_sentiment_bing$sentiment, col='green', lwd=2)
lines(southpark_sentiment_bing$date, southpark_sentiment_bing$sentiment, col='red', lwd=1)



