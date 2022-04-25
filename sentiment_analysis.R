#data import

simpsons_csv = read.csv("./data/simpsons_data.csv", header = TRUE)
guy_csv = read.csv("./data/guy_data.csv", header = TRUE)
southpark_csv = read.csv("./data/southpark_data.csv", header = TRUE)

install.packages("textdata")
install.packages("gridExtra")
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(gridExtra)
data(stop_words)

#preparing & tokenizing data

Sys.setlocale("LC_ALL","English")

prepare_my_data <- function(data){
  new_data <- tibble(line = 1:nrow(data), text = data$content, date = data$date)
  new_data$date <- as.Date(new_data$date, format = "%d %B %Y")
  new_data$date <- format(new_data$date, "%Y")
  new_data <- new_data %>%
    arrange(date)
  new_data$line <- c(1:nrow(new_data))
  
  #tokenizing
  new_data <- new_data %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
    
  
  new_data
}

simpsons_tidy <- prepare_my_data(simpsons_csv)
guy_tidy <- prepare_my_data(guy_csv)
southpark_tidy <- prepare_my_data(southpark_csv)



#afinn lexicon

afinn_sentiment <- function(data){
  new_data <- data %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(date) %>% 
    summarise(sentiment = sum(value)) %>% 
    mutate(method = "AFINN")
  
  new_data <- new_data %>%
    group_by(date) %>%
    mutate(sentiment = sum(sentiment)/nrow(new_data)) 
  
  new_data$sentiment_ptg_first <- vector(mode="integer", length=nrow(new_data))
  new_data$sentiment_ptg_next <- vector(mode="integer", length=nrow(new_data))
  
  for (i in 1:nrow(new_data)){
    new_data$sentiment_ptg_first[i] <- ((abs(new_data$sentiment[i]/new_data$sentiment[1]-1)))
  }
  for (i in 2:nrow(new_data)){
    new_data$sentiment_ptg_next[i] <- ((abs(new_data$sentiment[i]/new_data$sentiment[i-1]-1)))
  }
  new_data
}


simpson_sentiment_afinn <- afinn_sentiment(simpsons_tidy) 
guy_sentiment_afinn <- afinn_sentiment(guy_tidy)
southpark_sentiment_afinn <- afinn_sentiment(southpark_tidy) 


# afinn plots

  

df1 <- ggplot(simpson_sentiment_afinn)  + 
  geom_bar(aes(x=date, y=sentiment),stat="identity", fill="yellow",colour="#006000")+
 geom_line(aes(x=date, y=sentiment_ptg_first, group=1),stat="identity",color="red") +
  ggtitle("The Simpsons")

df2 <- ggplot(guy_sentiment_afinn)  + 
  geom_bar(aes(x=date, y=sentiment),stat="identity", fill="cyan",colour="#006000")+
 geom_line(aes(x=date, y=sentiment_ptg_first, group=1),stat="identity",color="red") +
  ggtitle("The Family Guy")

df3 <- ggplot(southpark_sentiment_afinn)  + 
  geom_bar(aes(x=date, y=sentiment),stat="identity", fill="green",colour="#006000")+
 geom_line(aes(x=date, y=sentiment_ptg_first, group=1),stat="identity",color="red") +
  ggtitle("The South Park")


grid.arrange(df1, df2, df3)


# top 5 emotion words

to_grepl_puller <- function(emotion){
  dict <- get_sentiments("nrc") %>%
    filter(sentiment == emotion)
  
  pulled_out <- pull(dict, word)
  pulled_out <- toString(pulled_out)
  pulled_out <- gsub(", ", "|", pulled_out)
}


joy <- to_grepl_puller("joy")
anger <- to_grepl_puller("anger")
anticipation <- to_grepl_puller("anticipation")
disgust <- to_grepl_puller("disgust")
fear <- to_grepl_puller("fear")
sadness <- to_grepl_puller("sadness")
trust <- to_grepl_puller("trust")


knit_plot <- function(data, emotion){
  
  plot_title <- deparse(substitute(emotion))
  
  plot <- data %>%
    count(word, sort = TRUE) %>%
    filter(grepl(emotion, word)) %>%
    slice_head(n=5) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, label=n)) +
    geom_col() +
    labs(y = NULL) +
    ggtitle(plot_title)
}


top_5 <- function(data) {
  plot1 <- knit_plot(data, joy)
  plot2 <- knit_plot(data, anger)
  plot3 <- knit_plot(data, anticipation)
  plot4 <- knit_plot(data, disgust)
  plot5 <- knit_plot(data, fear)
  plot6 <- knit_plot(data, sadness)
  plot7 <- knit_plot(data, trust)
  grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7)
}


top_5(simpsons_tidy)
top_5(guy_tidy)
top_5(southpark_tidy)
